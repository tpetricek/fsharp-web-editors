module FsWebTools.Editor

open System
open System.IO
open System.Text
open Microsoft.FSharp.Compiler.Ast
open Microsoft.FSharp.Compiler.Interactive.Shell
open Microsoft.FSharp.Compiler.SourceCodeServices

// ------------------------------------------------------------------------------------------------
// Agent that protects a specified resource
// ------------------------------------------------------------------------------------------------

/// Agent that allows only one caller to use the specified resource.
type ResourceAgent<'T>(resource:'T) =
  let agent = MailboxProcessor.Start(fun inbox -> async {
    while true do
      try
        let! work = inbox.Receive()
        printfn "Got work"
        do! work resource
        printfn "Done work"
      with e -> printfn "Failed: %A" e })
  member x.Process<'R>(work) : Async<'R> =
    agent.PostAndAsyncReply(fun reply checker -> async {
      let! res = work checker
      reply.Reply(res) })

// ------------------------------------------------------------------------------------------------
// F# compiler service wrapper
// ------------------------------------------------------------------------------------------------

/// Extracts all consecutive identifiers to the left of the charIndex for a specified line of code
let extractIdentTokens line charIndex =
    let sourceTok = SourceTokenizer([], "/home/test.fsx")
    let tokenizer = sourceTok.CreateLineTokenizer(line)

    let rec gatherTokens (tokenizer:FSharpLineTokenizer) state = seq {
      match tokenizer.ScanToken(state) with
      | Some tok, state ->
          yield tok
          yield! gatherTokens tokenizer state
      | None, state -> () }

    let tokens = gatherTokens tokenizer 0L |> Seq.toArray
    let idx = tokens |> Array.tryFindIndex(fun x ->
      charIndex > x.LeftColumn && charIndex <= x.LeftColumn + x.FullMatchedLength)

    match idx with
    | Some(endIndex) ->
        let startIndex =
            tokens.[0..endIndex]
            |> Array.rev
            |> Array.tryFindIndex (fun x -> x.TokenName <> "IDENT" && x.TokenName <> "DOT")
            |> Option.map (fun x -> endIndex - x)
        let startIndex = defaultArg startIndex 0
        let idents = tokens.[startIndex..endIndex] |> Array.filter (fun x -> x.TokenName = "IDENT")
        Some tokens.[endIndex], idents

    | None -> None, Array.empty

/// Parses the line of F# code and builds a list of identifier names in order
/// to be passed into the `GetDeclarations`, `GetMethods`, or other functions
/// For tooltips and overlodas, set identOffset=0; For completion set identOffset=1
let extractNames line charIndex identOffset =
    let charToken, tokens = extractIdentTokens line charIndex
    match charToken with
    | None -> 0, 0, []
    | Some(charToken) ->
        let names = tokens |> Array.map (fun x ->
          line.Substring(x.LeftColumn, x.FullMatchedLength).Trim('`'))
        let takeSize = tokens.Length - identOffset
        let finalList =
          if charToken.TokenName = "IDENT" && Array.length(tokens) > takeSize then
            names |> Seq.take takeSize |> Seq.toList
          else
            names |> Seq.toList
        (charToken.LeftColumn, charToken.LeftColumn + charToken.FullMatchedLength, finalList)

// Mostly boring code to format tooltips reported from method overloads
let htmlEncode (s:string) = 
    s.Trim().Replace("&", "&amp;").Replace("<", "&lt;").Replace(">", "&gt;")

let formatComment cmt (sb:StringBuilder) =
    match cmt with
    | FSharpXmlDoc.Text(s) -> sb.AppendLine(s.Trim()) |> ignore
    | FSharpXmlDoc.XmlDocFileSignature(file, signature) -> ()
    | FSharpXmlDoc.None -> ()

let formatTipElement isSingle el (sbSig:StringBuilder) (sbText:StringBuilder) =
    match el with
    | FSharpToolTipElement.None -> ()
    | FSharpToolTipElement.Single(it, comment) ->
        sbSig.AppendLine(htmlEncode it) |> ignore
        formatComment comment sbText
    | FSharpToolTipElement.Group(items) ->
        let items, msg =
          if items.Length > 10 then
            (items |> Seq.take 10 |> List.ofSeq),
            sprintf "   (+%d other overloads)" (items.Length - 10)
          else items, ""
        if isSingle && items.Length > 1 then
          sbSig.AppendLine("Multiple overloads") |> ignore
        for (it, comment) in items do
          sbSig.AppendLine(it) |> ignore
          formatComment comment sbText
        if msg <> null then sbSig.AppendFormat(msg) |> ignore
    | FSharpToolTipElement.CompositionError(err) ->
        sbText.Append("Composition error: " + err) |> ignore
let formatTip tip =
  let sbSig = StringBuilder()
  let sbText = StringBuilder()
  match tip with
  | FSharpToolTipText([single]) -> formatTipElement true single sbSig sbText
  | FSharpToolTipText(its) -> for item in its do formatTipElement false item sbSig sbText
  sbSig.ToString().Trim('\n', '\r'),
  sbText.ToString().Trim('\n', '\r')

/// Check specified file and return parsing & type checking results
let checkFile (fileName, source) (checker:FSharpChecker) = async {
    let! options = checker.GetProjectOptionsFromScript(fileName, source)
    match checker.TryGetRecentTypeCheckResultsForFile(fileName, options, source) with
    | Some(parse, check, _) -> return parse, check
    | None ->
        let! parse = checker.ParseFileInProject(fileName, source, options)
        let! answer = checker.CheckFileInProject(parse, fileName, 0, source, options)
        match answer with
        | FSharpCheckFileAnswer.Succeeded(check) -> return parse, check
        | FSharpCheckFileAnswer.Aborted -> return failwith "Parsing did not finish" }

/// Split the input string into an array of lines (using \r\n or \n as separator)
let getLines (s:string) = s.Replace("\r\n", "\n").Split('\n')

/// Get declarations (completion) at the specified line & column (lines are 1-based)
let getDeclarations (fileName, source) (line, col) (checker:FSharpChecker) = async {
    let! parse, check = checkFile (fileName, source) checker
    let textLine = getLines(source).[line-1]
    let _, _, names = extractNames textLine col 1
    let! decls = check.GetDeclarationListInfo(Some parse, line, col, textLine, names, "")
    return [ for it in decls.Items -> it.Name, it.Glyph, formatTip it.DescriptionText ] }

/// Get method overloads (for the method before '('). Lines are 1-based
let getMethodOverloads (fileName, source) (line, col) (checker:FSharpChecker) = async {
    let! parse, check = checkFile (fileName, source) checker
    let textLine = getLines(source).[line-1]
    match extractNames textLine col 0 with
    | _, _, [] -> return List.empty
    | _, _, names ->
        let! methods = check.GetMethodsAlternate(line, col, textLine, Some names)
        return [ for m in methods.Methods -> formatTip m.Description ] }

// ------------------------------------------------------------------------------------------------
// Suave.io web part that handles editor requests
// ------------------------------------------------------------------------------------------------

open Suave
open Suave.Filters
open Suave.Operators
open FSharp.Data
open System.Runtime.Serialization

/// Types of JSON values that we are returning from F# Compiler Service calls
type JsonTypes = JsonProvider<"""{
    "declaration":{"name":"Method", "glyph":1, "signature":"Text", "documentation":"Text"},
    "error":{"startLine":1, "startColumn":1, "endLine":1, "endColumn":1, "message":"error"}
  }""">

/// Read the request stream and pass it to the specified WebPart function
let requestString f = request (fun r -> 
  use sr = new StreamReader(new MemoryStream(r.rawForm))
  f (sr.ReadToEnd()) )

let noCache = 
  Writers.setHeader "Cache-Control" "no-cache, no-store, must-revalidate"
  >=> Writers.setHeader "Pragma" "no-cache"
  >=> Writers.setHeader "Expires" "0"

/// The main handler for Suave server!
let part scriptFile loadScriptLines (checker:FSharpChecker) = 
  let loadScriptString = String.concat "" [ for l in loadScriptLines -> l + "\n" ]
  let checker = ResourceAgent(checker)
  choose [
    // Type-check the source code & return list with error information
    path "/fseditor/check" >=> noCache >=> requestString (fun source ctx -> async {
      printfn "[APP] Check"
      let! _, check = 
          checkFile (scriptFile, loadScriptString + source) 
          |> checker.Process
      printfn "[APP] Checked"
      let res = 
        check.Errors 
        |> Array.map (fun err ->
            JsonTypes.Error( err.StartLineAlternate-1-Seq.length loadScriptLines, err.StartColumn,
                err.EndLineAlternate-1-Seq.length loadScriptLines, err.EndColumn, err.Message).JsonValue)
        |> JsonValue.Array
      printfn "[APP] Res: %O" res
      return! Successful.OK (res.ToString()) ctx }) 
    
    // Get method overloads & parameter info at the specified location in the source
    pathScan "/fseditor/methods/%d/%d" (fun (line, col) -> noCache >=> requestString (fun source ctx -> async {
      let! meths =
        getMethodOverloads (scriptFile, loadScriptString + source) (line + Seq.length loadScriptLines, col)
        |> checker.Process
      let res = [| for s1, s2 in meths -> JsonValue.String(s1 + s2) |] |> JsonValue.Array
      return! Successful.OK (res.ToString()) ctx }))

    // Get auto-completion for the specified location
    pathScan "/fseditor/declarations/%d/%d" (fun (line, col) -> noCache >=> requestString (fun source ctx -> async {
      printfn "[APP] Declarations"
      let! decls =
        getDeclarations (scriptFile, loadScriptString + source) (line + Seq.length loadScriptLines, col)
        |> checker.Process
      printfn "[APP] Got: %A" decls
      let res = 
        [| for name, glyph, (sg, info) in decls ->
             JsonTypes.Declaration(name, glyph, sg, info).JsonValue |] 
        |> JsonValue.Array
      printfn "[APP] Returning: %O" res
      return! Successful.OK (res.ToString()) ctx })) ]