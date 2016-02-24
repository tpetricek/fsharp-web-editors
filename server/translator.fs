namespace Katas.Server

open System
open Microsoft.FSharp.Quotations

// ------------------------------------------------------------------------------------------------
// A minimal class that takes a compiled assembly (as bytes) and translates its 
// ReflectedDefinition functions to JavaScript using FunScript. This is compiled separately
// in 'build.fsx' and loded in an AppDomain in 'evaluator.fs' so that we can unload the 
// assembly after the translation.
// ------------------------------------------------------------------------------------------------

type Translator(data:byte[]) =
  inherit MarshalByRefObject()
  
  let mutable result = None
  member x.Result = Option.get result

  member x.Run() =
    // Get all functions with reflected defintion
    let asm = System.Reflection.Assembly.Load(data)
    let functions =
      [ for t in asm.GetTypes() do
          for m in t.GetMethods() do
            match m with
            | DerivedPatterns.MethodWithReflectedDefinition(e) -> yield m.Name, e
            | _ -> () ]

    // Build a single F# expression and run the translator
    let res =
      functions
      |> Seq.fold (fun body (name, e) ->
            Expr.Let(Var.Global(name, e.Type), e, body) ) (Expr.Value( () ))
      |> FunScript.Compiler.Compiler.Compile
    result <- Some res
