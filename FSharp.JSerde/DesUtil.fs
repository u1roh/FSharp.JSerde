// utility for deserialization
module internal FSharp.JSerde.DesUtil
open Microsoft.FSharp.Reflection

let private bindingFlags = System.Reflection.BindingFlags.Public ||| System.Reflection.BindingFlags.NonPublic

let private createArray (elmType: System.Type) (src: obj[]) =
    let dst = System.Array.CreateInstance (elmType, src.Length)
    src |> Array.iteri (fun i obj -> dst.SetValue(obj, i))
    dst :> obj

let private createList (t: System.Type) (src: obj[]) =
  let cons = t.GetMethod "Cons"
  let empty = (t.GetProperty "Empty").GetValue null
  Array.foldBack (fun item list -> cons.Invoke (null, [| item; list |])) src empty

let private createOption (t: System.Type) (src: obj option) =
  let cases = FSharpType.GetUnionCases t
  match src with
  | Some obj -> FSharpValue.MakeUnion (cases[1], [| obj |])
  | None -> FSharpValue.MakeUnion (cases[0], [||])

let private createMap (t: System.Type) (src: (obj * obj)[]) =
  let ctor = t.GetConstructors()[0]
  let tupleType = FSharpType.MakeTupleType t.GenericTypeArguments
  src
  |> Array.map (fun (key, value) -> FSharpValue.MakeTuple ([| key; value |], tupleType))
  |> createArray tupleType
  |> fun src -> ctor.Invoke [| src |]
  
type Type =
  | Array of elmType:System.Type *  createArray:(obj[] -> obj)
  | Option of System.Type * createOption:(obj option -> obj)
  | List of System.Type * createList:(obj[] -> obj)
  | Map of key:System.Type * value:System.Type * createMap:((obj * obj)[] -> obj)
  | SingleCaseUnion of fields:System.Reflection.PropertyInfo[] * create:(obj[] -> obj)
  | Union of case:UnionCaseInfo[] * createUnion:(UnionCaseInfo -> obj[] -> obj)
  | Record of fields:System.Reflection.PropertyInfo[] * createRecord:(obj[] -> obj)
  | Tuple of System.Type[] * createTuple:(obj[] -> obj)
  | Parsable of (string -> obj)
  | String
  | Char
  | Bool
  | Byte
  | SByte
  | Int16
  | UInt16
  | Int32
  | UInt32
  | Int64
  | UInt64
  | Double
  | Single
  | Decimal
  | IntPtr
  | UIntPtr
  | Other

let private primitiveTypes =
  [ typeof<string>, String
    typeof<char>,   Char
    typeof<bool>,   Bool
    typeof<byte>,   Byte
    typeof<sbyte>,  SByte
    typeof<int16>,  Int16
    typeof<uint16>, UInt16
    typeof<int32>,  Int32
    typeof<uint32>, UInt32
    typeof<int64>,  Int64
    typeof<uint64>, UInt64
    typeof<double>, Double
    typeof<single>, Single
    typeof<decimal>, Decimal
    typeof<nativeint>,  IntPtr
    typeof<unativeint>, UIntPtr
  ] |> readOnlyDict

let classify (t: System.Type) =
  let genericDef = if t.IsGenericType then t.GetGenericTypeDefinition() |> Some else None
  match t, genericDef with
  | t, _ when primitiveTypes.ContainsKey t -> primitiveTypes[t]
  | t, _ when t.IsArray && t.HasElementType -> let elm = t.GetElementType() in Array (elm, createArray elm)
  | _, Some def when def = typedefof<option<_>> -> Option (t.GenericTypeArguments[0], createOption t)
  | _, Some def when def = typedefof<list<_>>   -> List (t.GenericTypeArguments[0], createList t)
  | _, Some def when def = typedefof<Map<_, _>> -> Map (t.GenericTypeArguments[0], t.GenericTypeArguments[1], createMap t)
  | t, _ when FSharpType.IsUnion (t, bindingFlags) ->
    match FSharpType.GetUnionCases (t, true) with
    | [| case |] -> SingleCaseUnion (case.GetFields(), fun args -> FSharpValue.MakeUnion (case, args, bindingFlags))
    | cases -> Union (cases, fun case args -> FSharpValue.MakeUnion (case, args, bindingFlags))
  | t, _ when FSharpType.IsRecord (t, bindingFlags) ->
    Record (FSharpType.GetRecordFields (t, bindingFlags), fun args -> FSharpValue.MakeRecord(t, args, true))
  | t, _ when FSharpType.IsTuple t ->
    Tuple (FSharpType.GetTupleElements t, fun args -> FSharpValue.MakeTuple (args, t))
  | _ ->
    let parse = t.GetMethod ("Parse", [| typeof<string> |])
    if not (isNull parse)
      then Parsable (fun (s: string) -> parse.Invoke (null, [| s |]))
      else Other
