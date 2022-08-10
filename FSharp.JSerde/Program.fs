open FSharp.JSerde

// For more information see https://aka.ms/fsharp-console-apps
printfn "Hello from F#"

type A =
  | Case1
  | Case2 of int
  | Case3 of hoge:string * piyo:float
  | Case4 of B

and B = {
  Foo: int
  Bar: string
}

type SingleCaseUnion = private SingleCaseUnion of int

try
  ([ Case3 ("piyo", 3.14); Case4 { Foo = 321; Bar = "blah" } ], Some 999)
  |> serialize
  |> string
  |> printfn "%O"

  [Some 111; None]
  |> serialize
  |> printfn "%A"

  SingleCaseUnion 333
  |> serialize
  |> printfn "%A"

  // Map [SingleCaseUnion 8372, 1; SingleCaseUnion 29492, 2]
  // Map ["a", 1; "b", 2]
  Map [{ Foo = 1; Bar = "a" }, 111; { Foo = 2; Bar = "b" }, 222 ]
  |> serialize
  |> printfn "%A"
with e -> printfn "%A" e
