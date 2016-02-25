module FsWebTools.Evaluator

open System
open System.IO
open System.Text
open Microsoft.FSharp.Compiler.SourceCodeServices
open Microsoft.FSharp.Compiler.Interactive.Shell

// ------------------------------------------------------------------------------------------------
// Agent that protects a specified resource
// ------------------------------------------------------------------------------------------------

/// Agent that allows only one caller to use the specified resource.
type ResourceAgent<'T>(resource:'T) =
  let agent = MailboxProcessor.Start(fun inbox -> async {
    while true do
      try
        let! work = inbox.Receive()
        do! work resource
      with e -> 
        () })
  member x.Process<'R>(work) : Async<'R> = async {
    let! result = agent.PostAndAsyncReply(fun reply checker -> async {
      try
        let! res = work checker
        reply.Reply(Choice1Of2 res)
      with e ->
        reply.Reply(Choice2Of2 e) })
    match result with 
    | Choice1Of2 res -> return res
    | Choice2Of2 e -> return raise (Exception("Processing failed", e)) }

// ------------------------------------------------------------------------------------------------
// F# interactive service wrapper
// ------------------------------------------------------------------------------------------------


/// Initialize an FSI session. After crating FSI session, it changes the current
/// directory to the specified one and loads the initialization script
let createSession rootFolder loadScriptString =
  // Intialize output and input streams
  let sbOut = new StringBuilder()
  let sbErr = new StringBuilder()
  let inStream = new StringReader("")
  let outStream = new StringWriter(sbOut)
  let errStream = new StringWriter(sbErr)

  // Build command line arguments & start FSI session
  let argv = [| Path.Combine(Path.GetTempPath(), "fsi.exe") |]
  let allArgs = Array.append argv [|"--noninteractive"|]
  let fsiConfig = FsiEvaluationSession.GetDefaultConfiguration()
  let fsi = FsiEvaluationSession.Create(fsiConfig, allArgs, inStream, outStream, errStream)
  fsi.EvalInteraction("#cd \"\"\"" + rootFolder + "\"\"\";;")
  fsi.EvalInteraction(loadScriptString)
  fsi

/// Evaluate the specified code nd return the 'it' value. Returns 
/// Choice1Of3 with formatted 'it' result and output, Choice2Of3 with
/// compiler error messages or Choice3Of3 with exception.
let evalInteraction code (fsi:FsiEvaluationSession) = async {
  let sbOut = new StringBuilder()
  Console.SetOut(new StringWriter(sbOut))
  let res, errors = fsi.EvalInteractionNonThrowing(code)
  if errors.Length > 0 then 
    return Choice2Of3(errors)
  else  
    match res with 
    | Choice2Of2 exn -> return Choice3Of3(exn)
    | _ ->
      match fsi.EvalExpressionNonThrowing("it") with
      | Choice1Of2(Some res), _ when res.ReflectionValue <> null -> 
          return Choice1Of3(res.ReflectionValue.ToString(), sbOut.ToString())
      | _ -> return Choice1Of3(null, sbOut.ToString()) }

// ------------------------------------------------------------------------------------------------
// Suave.io web part that handles evaluator requests
// ------------------------------------------------------------------------------------------------

open Suave
open Suave.Filters
open Suave.Operators
open FSharp.Data

/// Types of JSON values that we are returning from F# Compiler Service calls
type JsonTypes = JsonProvider<"""[
    { "result": "error", "data": [{"startLine":1, "startColumn":1, "endLine":1, "endColumn":1, "message":"error"}] },
    { "result": "exception", "data": "something went wrong" },
    { "result": "success", "data": { "result":"foo", "output":"hello" } }
  ]""">

/// Read the request stream and pass it to the specified WebPart function
let requestString f = request (fun r -> 
  use sr = new StreamReader(new MemoryStream(r.rawForm))
  f (sr.ReadToEnd()) )

let noCache = 
  Writers.setHeader "Cache-Control" "no-cache, no-store, must-revalidate"
  >=> Writers.setHeader "Pragma" "no-cache"
  >=> Writers.setHeader "Expires" "0"

/// The main handler for Suave server!
let part rootFolder loadScriptLines = 
  let loadScriptString = String.concat "" [ for l in loadScriptLines -> l + "\n" ]
  let fsi = ResourceAgent(createSession rootFolder loadScriptString)
  path "/fsi/eval" >=> noCache >=> requestString (fun source ctx -> async {
    let! res = evalInteraction source |> fsi.Process
    let res = 
      match res with
      | Choice1Of3(result, output) ->
          let data = JsonTypes.Data(result, output)
          JsonTypes.Root("success", JsonTypes.StringOrArrayOrData(data))
      | Choice2Of3(errs) ->
          let data = errs |> Array.map (fun e ->
            JsonTypes.Datum(e.StartLineAlternate+1-Seq.length loadScriptLines, e.StartColumn, 
              e.EndLineAlternate+1-Seq.length loadScriptLines, e.EndColumn, e.Message))
          JsonTypes.Root("error", JsonTypes.StringOrArrayOrData(data))      
      | Choice3Of3(exn) ->
          let data = exn.ToString()
          JsonTypes.Root("exception", JsonTypes.StringOrArrayOrData(data))
    return! Successful.OK (res.JsonValue.ToString()) ctx }) 
    