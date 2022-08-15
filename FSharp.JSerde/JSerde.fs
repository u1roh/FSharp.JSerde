module FSharp.JSerde.JSerde
open Microsoft.FSharp.Reflection
open FSharp.Data

type ICustomSerializer =
  abstract TargetType: System.Type
  abstract Serialize: obj -> JsonValue
  abstract Deserialize: JsonValue -> obj

let custom<'a> (serialize: 'a -> JsonValue) (deserialize: JsonValue -> 'a) =
  { new ICustomSerializer with
      member _.TargetType = typeof<'a>
      member _.Serialize obj = serialize (obj :?> 'a)
      member _.Deserialize json = deserialize json :> obj }

type Serializer (customs: ICustomSerializer seq) =
  let customs = customs |> Seq.map (fun item -> item.TargetType, item) |> readOnlyDict 

  member _.Serialize (obj: obj): JsonValue option =
    let t = obj.GetType() in if t.IsGenericType then t.GetGenericTypeDefinition() else t
    |> customs.TryGetValue
    |> function (true, ser) -> Some (ser.Serialize obj) | _ -> None

  member _.Deserialize (ty: System.Type, json: JsonValue): obj option =
    customs.TryGetValue ty
    |> function (true, ser) -> Some (ser.Deserialize json) | _ -> None

exception UnsupportedType of System.Type

let private bindingFlags = System.Reflection.BindingFlags.Public ||| System.Reflection.BindingFlags.NonPublic

let rec serialize (custom : Serializer option) (obj: obj) =
  if isNull obj then JsonValue.Null else
  match custom |> Option.bind (fun c -> c.Serialize obj) with
  | Some json -> json
  | _ ->
    match obj with
    | :? bool as value -> JsonValue.Boolean value
    | :? int as value -> JsonValue.Number (decimal value)
    | :? decimal as value -> JsonValue.Number value
    | :? float as value -> JsonValue.Float value
    | :? string as value -> JsonValue.String value
    | :? System.Array as a -> Array.init a.Length (fun i -> a.GetValue i |> serialize custom) |> JsonValue.Array
    | _ ->
      let t = obj.GetType()
      if FSharpType.IsTuple t then
        FSharpValue.GetTupleFields obj
        |> Array.map (serialize custom)
        |> JsonValue.Array
      elif t.IsGenericType && t.GetGenericTypeDefinition() = typedefof<option<_>> then
        let case, fields = FSharpValue.GetUnionFields (obj, obj.GetType(), false)
        if case.Name = "Some"
          then serialize custom fields[0]
          else JsonValue.Null
      elif t.IsGenericType && t.GetGenericTypeDefinition() = typedefof<list<_>> then
        serializeList custom obj
        |> List.toArray
        |> JsonValue.Array
      elif t.IsGenericType && t.GetGenericTypeDefinition() = typedefof<Map<_, _>> then
        obj :?> System.Collections.IEnumerable
        |> Seq.cast<obj>
        |> Seq.map (fun pair ->
          let t = pair.GetType()
          let get name = (t.GetProperty name).GetValue pair |> serialize custom
          let key = get "Key" |> function JsonValue.String s -> s | json -> string json
          key, get "Value")
        |> Seq.toArray
        |> JsonValue.Record
      elif FSharpType.IsRecord (t, bindingFlags) then
        FSharpType.GetRecordFields (t, bindingFlags)
        |> Array.map (fun prop -> prop.Name, FSharpValue.GetRecordField (obj, prop) |> serialize custom)
        |> JsonValue.Record
      elif FSharpType.IsUnion (t, bindingFlags) then
        let case, fields = FSharpValue.GetUnionFields (obj, t, true)
        if (FSharpType.GetUnionCases (t, true)).Length = 1 then // single case union
          match fields with
          | [||] -> JsonValue.String case.Name
          | [| item |] -> serialize custom item
          | array -> array |> Array.map (serialize custom) |> JsonValue.Array
        else
          match fields with
          | [||] -> JsonValue.String case.Name
          | [| item |] -> JsonValue.Record [| case.Name, serialize custom item |]
          | array -> JsonValue.Record [| case.Name, array |> Array.map (serialize custom) |> JsonValue.Array |]
      elif t.GetMethod ("Parse", [| typeof<string> |]) |> isNull |> not then
        obj.ToString() |> JsonValue.String
      else
        raise (UnsupportedType t)

and private serializeList (custom: Serializer option) obj =
  let case, fields = FSharpValue.GetUnionFields (obj, obj.GetType(), false)
  if case.Name = "Cons" then
    serialize custom fields[0] :: serializeList custom fields[1]
  else
    []

exception TypeMismatched of System.Type * JsonValue

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
  let dst = System.Array.CreateInstance (tupleType, src.Length)
  src |> Array.iteri (fun i (key, value) ->
    dst.SetValue(FSharpValue.MakeTuple ([| key; value |], tupleType), i))
  ctor.Invoke [| dst |]

type Type =
  | Array of elmType:System.Type
  | Option of System.Type * createOption:(obj option -> obj)
  | List of System.Type * createList:(obj[] -> obj)
  | Map of key:System.Type * value:System.Type * createMap:((obj * obj)[] -> obj)
  | SingleCaseUnion of fields:System.Reflection.PropertyInfo[] * create:(obj[] -> obj)
  | Union of UnionCaseInfo[]
  | Record of System.Reflection.PropertyInfo[]
  | Tuple of System.Type[]
  | Parsable of (string -> obj)
  | String
  | Bool
  | Int
  | Float
  | Decimal
  | Other

module Type =
  let private primitiveTypes =
    [ typeof<string>, String
      typeof<bool>, Bool
      typeof<int>, Int
      typeof<float>, Float
      typeof<decimal>, Decimal
    ] |> readOnlyDict

  let classify (t: System.Type) =
    let genericDef = if t.IsGenericType then t.GetGenericTypeDefinition() |> Some else None
    match t, genericDef with
    | t, _ when primitiveTypes.ContainsKey t -> primitiveTypes[t]
    | t, _ when t.IsArray && t.HasElementType -> t.GetElementType () |> Array
    | _, Some def when def = typedefof<option<_>> -> Option (t.GenericTypeArguments[0], createOption t)
    | _, Some def when def = typedefof<list<_>>   -> List (t.GenericTypeArguments[0], createList t)
    | _, Some def when def = typedefof<Map<_, _>> -> Map (t.GenericTypeArguments[0], t.GenericTypeArguments[1], createMap t)
    | t, _ when FSharpType.IsUnion (t, bindingFlags) ->
      match FSharpType.GetUnionCases (t, true) with
      | [| case |] -> SingleCaseUnion (case.GetFields(), fun args -> FSharpValue.MakeUnion (case, args, bindingFlags))
      | cases -> Union cases
    | t, _ when FSharpType.IsRecord (t, bindingFlags) -> FSharpType.GetRecordFields (t, bindingFlags) |> Record
    | t, _ when FSharpType.IsTuple t -> FSharpType.GetTupleElements t |> Tuple
    | _ ->
      let parse = t.GetMethod ("Parse", [| typeof<string> |])
      if not (isNull parse)
        then Parsable (fun (s: string) -> parse.Invoke (null, [| s |]))
        else Other

let rec private deserializeByType (custom: Serializer option) (t: System.Type) (json: JsonValue) : obj =
  let fail () = TypeMismatched (t, json) |> raise
  match custom |> Option.bind (fun c -> c.Deserialize (t, json)) with
  | Some obj -> obj
  | _ ->
    match Type.classify t, json with
    | Array elmType, JsonValue.Array src ->
      let dst = System.Array.CreateInstance (elmType, src.Length)
      src |> Array.iteri (fun i obj -> dst.SetValue(deserializeByType custom elmType obj, i))
      dst :> obj
    | Option (elmType, createOption), json ->
      if json = JsonValue.Null
        then None
        else deserializeByType custom elmType json |> Some 
      |> createOption
    | List (elmType, createList), JsonValue.Array src ->
      src
      |> Array.map (deserializeByType custom elmType)
      |> createList
    | Map (keyType, valueType, createMap), JsonValue.Record src ->
      src
      |> Array.map (fun (key, value) ->
        let key =
          JsonValue.TryParse key
          |> Option.map (deserializeByType custom keyType)
          |> Option.defaultValue key
        let value = deserializeByType custom valueType value
        key, value)
      |> createMap
    | SingleCaseUnion (fields, create), json ->
      match fields, json with
      | [| field |], _ -> create [| deserializeByType custom field.PropertyType json |]
      | fields, JsonValue.Array a when fields.Length = a.Length ->
        Array.zip fields a
        |> Array.map (fun (field, json) -> deserializeByType custom field.PropertyType json)
        |> create
      | _ -> fail()
    | Union cases, JsonValue.String name ->
      cases
      |> Array.tryFind (fun case -> case.Name = name && case.GetFields().Length = 0)
      |> function Some case -> FSharpValue.MakeUnion (case, [||]) | _ -> fail()
    | Union cases, JsonValue.Record [| name, json |] ->
      cases
      |> Array.tryFind (fun case -> case.Name = name)
      |> Option.bind (fun case ->
          match case.GetFields(), json with
          | [| field |], _ -> FSharpValue.MakeUnion (case, [| deserializeByType custom field.PropertyType json |]) |> Some
          | fields, JsonValue.Array a when fields.Length = a.Length -> 
            Array.zip fields a
            |> Array.map (fun (field, json) -> deserializeByType custom field.PropertyType json)
            |> fun a -> FSharpValue.MakeUnion (case, a) |> Some
          | _ -> None)
      |> function Some obj -> obj | _ -> fail ()
    | Record fields, JsonValue.Record src ->
      let values =
        fields
        |> Array.map (fun field -> field.PropertyType, src |> Array.find (fst >> (=) field.Name))
        |> Array.map (fun (elmType, (_, obj)) -> deserializeByType custom elmType obj)
      FSharpValue.MakeRecord (t, values, true)
    | Tuple elmTypes, JsonValue.Array src when src.Length = elmTypes.Length ->
      Array.zip elmTypes src
      |> Array.map (fun (t, json) -> deserializeByType custom t json)
      |> fun values -> FSharpValue.MakeTuple (values, t)
    | String,   JsonValue.String s -> s :> obj
    | Bool,     JsonValue.Boolean b -> b :> obj
    | Int,      JsonValue.Number n -> int n :> obj
    | Float,    JsonValue.Number n -> float n :> obj
    | Decimal,  JsonValue.Number n -> n :> obj
    | Int,      JsonValue.Float n -> int n :> obj
    | Float,    JsonValue.Float n -> n :> obj
    | Decimal,  JsonValue.Float n -> decimal n :> obj
    | Parsable parse, JsonValue.String s -> parse s
    | _ -> fail ()

let deserialize<'a> custom json = deserializeByType custom typeof<'a> json :?> 'a
