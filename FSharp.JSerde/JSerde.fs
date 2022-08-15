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
  | Union of UnionCaseInfo[]
  | Record of System.Reflection.PropertyInfo[]
  | Tuple of System.Type[]
  | Other

module Type =
  let classify (t: System.Type) =
    if t.IsArray && t.HasElementType then
      t.GetElementType () |> Array
    elif FSharpType.IsUnion (t, bindingFlags) then
      FSharpType.GetUnionCases (t, true) |> Union
    elif FSharpType.IsRecord (t, bindingFlags) then
      FSharpType.GetRecordFields (t, bindingFlags) |> Record
    elif FSharpType.IsTuple t then
      FSharpType.GetTupleElements t |> Tuple
    else if t.IsGenericType then
      match t.GetGenericTypeDefinition() with
      | def when def = typedefof<option<_>> -> Option (t.GenericTypeArguments[0], createOption t)
      | def when def = typedefof<list<_>>   -> List (t.GenericTypeArguments[0], createList t)
      | def when def = typedefof<Map<_, _>> -> Map (t.GenericTypeArguments[0], t.GenericTypeArguments[1], createMap t)
      | _ -> Other
    else Other


let rec private deserializeByType (custom: Serializer option) (t: System.Type) (json: JsonValue) : obj =
  let fail () = TypeMismatched (t, json) |> raise
  match custom |> Option.bind (fun c -> c.Deserialize (t, json)) with
  | Some obj -> obj
  | _ ->
    if t.IsArray && t.HasElementType then
      match json with
      | JsonValue.Array src ->
        let elmType = t.GetElementType()
        let dst = System.Array.CreateInstance (elmType, src.Length)
        src |> Array.iteri (fun i obj -> dst.SetValue(deserializeByType custom elmType obj, i))
        dst :> obj
      | _ -> fail()
    elif t.IsGenericType && t.GetGenericTypeDefinition() = typedefof<option<_>> then
      if json = JsonValue.Null
        then None
        else deserializeByType custom t.GenericTypeArguments[0] json |> Some 
      |> createOption t
    elif t.IsGenericType && t.GetGenericTypeDefinition() = typedefof<list<_>> then
      match json with
      | JsonValue.Array src ->
        src
        |> Array.map (deserializeByType custom t.GenericTypeArguments[0])
        |> createList t
      | _ -> fail()
    elif t.IsGenericType && t.GetGenericTypeDefinition() = typedefof<Map<_, _>> then
      match json with
      | JsonValue.Record src ->
        let keyType = t.GenericTypeArguments[0]
        let valueType = t.GenericTypeArguments[1]
        src
        |> Array.map (fun (key, value) ->
          let key =
            JsonValue.TryParse key
            |> Option.map (deserializeByType custom keyType)
            |> Option.defaultValue key
          let value = deserializeByType custom valueType value
          key, value)
        |> createMap t
      | _ -> fail()
    elif FSharpType.IsUnion (t, bindingFlags) then
      let cases = FSharpType.GetUnionCases (t, true)
      if cases.Length = 1 then // single case union
        match cases[0].GetFields(), json with
        | [| field |], _ ->
          FSharpValue.MakeUnion (cases[0], [| deserializeByType custom field.PropertyType json |], bindingFlags)
        | fields, JsonValue.Array a when fields.Length = a.Length ->
          Array.zip fields a
          |> Array.map (fun (field, json) -> deserializeByType custom field.PropertyType json)
          |> fun a -> FSharpValue.MakeUnion (cases[0], a)
        | _ -> fail()
      else
        match json with
        | JsonValue.String name ->
          cases
          |> Array.tryFind (fun case -> case.Name = name && case.GetFields().Length = 0)
          |> function Some case -> FSharpValue.MakeUnion (case, [||]) | _ -> fail()
        | JsonValue.Record [| name, json |] ->
          cases
          |> Array.tryFind (fun case -> case.Name = name)
          |> function
            | Some case ->
              match case.GetFields(), json with
              | [| field |], _ -> FSharpValue.MakeUnion (case, [| deserializeByType custom field.PropertyType json |])
              | fields, JsonValue.Array a when fields.Length = a.Length -> 
                Array.zip fields a
                |> Array.map (fun (field, json) -> deserializeByType custom field.PropertyType json)
                |> fun a -> FSharpValue.MakeUnion (case, a)
              | _ -> fail()
            | _ -> fail()
        | _ -> fail()
    elif FSharpType.IsRecord (t, bindingFlags) then
      match json with
      | JsonValue.Record src ->
        let values =
          FSharpType.GetRecordFields (t, bindingFlags)
          |> Array.map (fun field -> field.PropertyType, src |> Array.find (fst >> (=) field.Name))
          |> Array.map (fun (elmType, (_, obj)) -> deserializeByType custom elmType obj)
        FSharpValue.MakeRecord (t, values, true)
      | _ -> fail ()
    elif FSharpType.IsTuple t then
      let elmTypes = FSharpType.GetTupleElements t
      match json with
      | JsonValue.Array src when src.Length = elmTypes.Length ->
        Array.zip elmTypes src
        |> Array.map (fun (t, json) -> deserializeByType custom t json)
        |> fun values -> FSharpValue.MakeTuple (values, t)
      | _ -> fail ()
    else
      match json with
      | JsonValue.String s ->
        if t = typeof<string> then
          s :> obj
        else
          let parse = t.GetMethod ("Parse", [| typeof<string> |])
          if not (isNull parse) then
            parse.Invoke (null, [| s |])
          else
            fail ()
      | JsonValue.Boolean b ->
        if t = typeof<bool> then b :> obj else fail ()
      | JsonValue.Number n ->
        if    t = typeof<decimal> then n :> obj
        elif  t = typeof<int>     then int n :> obj
        elif  t = typeof<float>   then float n :> obj
        else fail ()
      | JsonValue.Float n ->
        if    t = typeof<decimal> then n :> obj
        elif  t = typeof<int>     then int n :> obj
        elif  t = typeof<float>   then float n :> obj
        else fail ()
      | _ -> fail ()

let deserialize<'a> custom json = deserializeByType custom typeof<'a> json :?> 'a
