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
  Buzz: double option
}

type SingleCaseUnion = private SingleCaseUnion of int

type Flags =
  | A = 0b001uy
  | B = 0b010uy
  | C = 0b100uy
  | BC = 0b110uy

let testBy<'a> cfg (value: 'a) (json: JsonValue) =
  match json, JSerde.toJsonValue cfg value with
  | JsonValue.Record lhs, JsonValue.Record rhs -> Assert.AreEqual (Map lhs, Map rhs)
  | lhs, rhs -> Assert.AreEqual (lhs, rhs)

  Assert.AreEqual (value, JSerde.fromJsonValue<'a> cfg json)

let test<'a> = testBy<'a> JSerde.Config.Default

[<Test>]
let ``enum`` () =
  test Flags.A (JsonValue.String "A")
  test Flags.B (JsonValue.String "B")
  test (Flags.A ||| Flags.B) (JsonValue.Number (decimal 0b011uy))
  test (Flags.B ||| Flags.C) (JsonValue.String "BC")

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
    { Foo = 100; Bar = "bar"; Buzz = Some 3.14 }
    (JsonValue.Record [| "Foo", JsonValue.Number (decimal 100); "Bar", JsonValue.String "bar"; "Buzz", JsonValue.Float 3.14 |])

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
  let cfg =
    JSerde.custom<System.DateTime>
      (fun value -> value.Ticks |> decimal |> JsonValue.Number)
      (function JsonValue.Number ticks -> int64 ticks |> System.DateTime | _ -> failwith "DateTime format error")
    |> Seq.singleton
    |> JSerde.Config.FromCustomSerializers
  testBy cfg value json

[<Test>]
let omitNoneFieldOfRecord() =
  // JSON doesn't contain "Buzz" because `Buzz` is None
  test
    { Foo = 100; Bar = "bar"; Buzz = None }
    (JsonValue.Record [| "Foo", JsonValue.Number (decimal 100); "Bar", JsonValue.String "bar" |])


[<Test>]
let primitiveTypes () =
  test true  (JsonValue.Boolean true)
  test false (JsonValue.Boolean false)
  test 123uy (JsonValue.Number (decimal 123))
  test 123y  (JsonValue.Number (decimal 123))
  test 123s  (JsonValue.Number (decimal 123))
  test 123us (JsonValue.Number (decimal 123))
  test 123   (JsonValue.Number (decimal 123))
  test 123u  (JsonValue.Number (decimal 123))
  test 123L  (JsonValue.Number (decimal 123))
  test 123UL (JsonValue.Number (decimal 123))
  test 12.3m (JsonValue.Number 12.3m)
  test 12.3  (JsonValue.Float 12.3)
  test 12.3f (JsonValue.Float (double 12.3f))
  test (nativeint  123) (JsonValue.Number (decimal 123))
  test (unativeint 123) (JsonValue.Number (decimal 123))
  test 'c' (JsonValue.String "c")

module Example =

  type UnionType =
    | Case1
    | Case2 of string
    | Case3 of {| Foo: int; Bar: bool |}

  type SingleCaseUnion = private SingleCaseUnion of int

  type RecordType = {
    A: string
    B: int option
    C: Map<SingleCaseUnion, UnionType option>
  }

  [<Test>]
  let testExample () =
    let value = {
      A = "hello"
      B = Some 123
      C = Map [
        SingleCaseUnion 111, Some Case1
        SingleCaseUnion 222, None
        SingleCaseUnion 333, Some (Case2 "bye")
        SingleCaseUnion 444, Some (Case3 {| Foo = 555; Bar = true |})
      ] 
    }

    let json = JSerde.toJsonString JSerde.Config.Default value
    // printfn "json = %O" json

    let parsed = JSerde.fromJsonString<RecordType> JSerde.Config.Default json
    // printfn "parsed = %A" parsed

    Assert.AreEqual (value, parsed)
