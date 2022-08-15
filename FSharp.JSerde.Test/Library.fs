module FSharp.JSerde.Test

open NUnit.Framework
open FSharp.Data
open FSharp.JSerde

type A =
  private
  | Case1
  | Case2 of int
  | Case3 of hoge:string * piyo:float
  | Case4 of B
  | Case5 of string list
  | Case6 of string[]
  | Case7 of int option

and B = private {
  Foo: int
  Bar: string
}

type SingleCaseUnion = private SingleCaseUnion of int

let testBy<'a> custom (value: 'a) (json: JsonValue) =
  match json, JSerde.serialize custom value with
  | JsonValue.Record lhs, JsonValue.Record rhs -> Assert.AreEqual (Map lhs, Map rhs)
  | lhs, rhs -> Assert.AreEqual (lhs, rhs)

  Assert.AreEqual (value, JSerde.deserialize<'a> custom json)

let test<'a> = testBy<'a> None

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
let ``A.Case5`` () =
  test
    (Case5 ["aaa"; "bbb"; "ccc"])
    (JsonValue.Record [| "Case5", JsonValue.Array [| JsonValue.String "aaa"; JsonValue.String "bbb"; JsonValue.String "ccc" |] |])

[<Test>]
let ``A.Case6`` () =
  test
    (Case6 [| "aaa"; "bbb"; "ccc" |])
    (JsonValue.Record [| "Case6", JsonValue.Array [| JsonValue.String "aaa"; JsonValue.String "bbb"; JsonValue.String "ccc" |] |])

[<Test>]
let ``A.Case7`` () =
  test
    (Case7 (Some 777))
    (JsonValue.Record [| "Case7", JsonValue.Number (decimal 777) |])

  test
    (Case7 None)
    (JsonValue.Record [| "Case7", JsonValue.Null |])
    // (JsonValue.String "Case7")

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

[<Test>]
let option() =
  test
    (Some 999)
    (JsonValue.Number (decimal 999))
  
  test<int option>
    None
    JsonValue.Null

[<Test>]
let map () =
  test
    (Map ["foo", 3.21; "bar", 6.54])
    (JsonValue.Record [| "foo", JsonValue.Float 3.21; "bar", JsonValue.Float 6.54 |])

[<Test>]
let singleCaseUnion () =
  test
    (SingleCaseUnion 8888)
    (JsonValue.Number (decimal 8888))

  test
    (Map [SingleCaseUnion 1010, 3.21; SingleCaseUnion 2020, 6.54])
    (JsonValue.Record [| "1010", JsonValue.Float 3.21; "2020", JsonValue.Float 6.54 |])

[<Test>]
let guid () =
  let guid = System.Guid.NewGuid()
  test guid (JsonValue.String (guid.ToString()))

[<Test>]
let datetime () =
  let value = System.DateTime(2022, 8, 15, 12, 34, 56)
  let json = JsonValue.String "2022/08/15 12:34:56" // default format of DateTime.ToString()
  test value json

[<Test>]
let datetimeByCustom () =
  let value = System.DateTime.Now
  let json = JsonValue.Number (decimal value.Ticks)
  let custom =
    JSerde.custom<System.DateTime>
      (fun value -> value.Ticks |> decimal |> JsonValue.Number)
      (function JsonValue.Number ticks -> int64 ticks |> System.DateTime | _ -> failwith "DateTime format error")
    |> Seq.singleton
    |> Serializer
  testBy (Some custom) value json
