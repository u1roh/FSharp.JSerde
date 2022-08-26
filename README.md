# FSharp.JSerde: JSON Serialization for F# Types

## Example
```fsharp
#r "nuget: FSharp.JSerde"
open FSharp.JSerde

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
printfn "json = %O" json

let parsed = JSerde.fromJsonString<RecordType> JSerde.Config.Default json
printfn "parsed = %A" parsed
```

Output:
```
json = {"A":"hello","B":123,"C":{"111":"Case1","222":null,"333":{"Case2":"bye"},"444":{"Case3":{"Bar":true,"Foo":555}}}}
```
