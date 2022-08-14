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

let test<'a> (value: 'a) (json: JsonValue) =
  Assert.AreEqual (json, FSharp.JSerde.serialize value)
  Assert.AreEqual (value, FSharp.JSerde.deserialize<'a> json)

[<Test>]
let ``A.Case1`` () =
  test
    Case1
    (JsonValue.String "Case1")

[<Test>]
let ``A.Case2`` () =
  test
    (Case2 123)
    (JsonValue.Record [| "Case2", JsonValue.Number (decimal 123) |])

[<Test>]
let ``B``() =
  test
    { Foo = 100; Bar = "bar" }
    (JsonValue.Record [| "Foo", JsonValue.Number (decimal 100); "Bar", JsonValue.String "bar" |])

[<Test>]
let tuple() =
  test
    ("foo", true)
    (JsonValue.Array [| JsonValue.String "foo"; JsonValue.Boolean true |])

[<Test>]
let array() =
  test
    [| "foo"; "bar"; "buzz" |]
    (JsonValue.Array [| JsonValue.String "foo"; JsonValue.String "bar"; JsonValue.String "buzz" |])

[<Test>]
let list() =
  test
    [ "foo"; "bar"; "buzz" ]
    (JsonValue.Array [| JsonValue.String "foo"; JsonValue.String "bar"; JsonValue.String "buzz" |])
