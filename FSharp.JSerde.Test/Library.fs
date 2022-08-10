module FSharp.JSerde.Test

open NUnit.Framework
open FSharp.Data

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

[<Test>]
let ``A.Case1`` () =
  let expected = JsonValue.String "Case1"
  Assert.AreEqual (expected, FSharp.JSerde.serialize Case1)
  Assert.AreEqual (Case1, FSharp.JSerde.deserialize<A> expected)

[<Test>]
let ``A.Case2`` () =
  let expected = JsonValue.Record [| "Case2", JsonValue.Number (decimal 123) |]
  Assert.AreEqual (expected, FSharp.JSerde.serialize (Case2 123))
  Assert.AreEqual (Case2 123, FSharp.JSerde.deserialize<A> expected)
