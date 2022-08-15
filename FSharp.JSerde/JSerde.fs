module FSharp.JSerde.JSerde
open Microsoft.FSharp.Reflection
open FSharp.Data

exception UnsupportedType of System.Type

let private bindingFlags = System.Reflection.BindingFlags.Public ||| System.Reflection.BindingFlags.NonPublic

let rec serialize (obj: obj) =
  if isNull obj then JsonValue.Null else
  match obj with
  | :? bool as value -> JsonValue.Boolean value
  | :? int as value -> JsonValue.Number (decimal value)
  | :? decimal as value -> JsonValue.Number value
  | :? float as value -> JsonValue.Float value
  | :? string as value -> JsonValue.String value
  | :? System.Array as a -> Array.init a.Length (fun i -> a.GetValue i |> serialize) |> JsonValue.Array
  | _ ->
    let t = obj.GetType()
    if FSharpType.IsTuple t then
      FSharpValue.GetTupleFields obj
      |> Array.map serialize
      |> JsonValue.Array
    elif t.IsGenericType && t.GetGenericTypeDefinition() = typedefof<option<_>> then
      let case, fields = FSharpValue.GetUnionFields (obj, obj.GetType(), false)
      if case.Name = "Some"
        then serialize fields[0]
        else JsonValue.Null
    elif t.IsGenericType && t.GetGenericTypeDefinition() = typedefof<list<_>> then
      serializeList obj
      |> List.toArray
      |> JsonValue.Array
    elif t.IsGenericType && t.GetGenericTypeDefinition() = typedefof<Map<_, _>> then
      obj :?> System.Collections.IEnumerable
      |> Seq.cast<obj>
      |> Seq.map (fun pair ->
        let t = pair.GetType()
        let get name = (t.GetProperty name).GetValue pair |> serialize
        let key = get "Key" |> function JsonValue.String s -> s | json -> string json
        key, get "Value")
      |> Seq.toArray
      |> JsonValue.Record
    elif FSharpType.IsRecord (t, bindingFlags) then
      FSharpType.GetRecordFields (t, bindingFlags)
      |> Array.map (fun prop -> prop.Name, FSharpValue.GetRecordField (obj, prop) |> serialize)
      |> JsonValue.Record
    elif FSharpType.IsUnion (t, bindingFlags) then
      let case, fields = FSharpValue.GetUnionFields (obj, t, true)
      if (FSharpType.GetUnionCases (t, true)).Length = 1 then // single case union
        match fields with
        | [||] -> JsonValue.String case.Name
        | [| item |] -> serialize item
        | array -> array |> Array.map serialize |> JsonValue.Array
      else
        match fields with
        | [||] -> JsonValue.String case.Name
        | [| item |] -> JsonValue.Record [| case.Name, serialize item |]
        | array -> JsonValue.Record [| case.Name, array |> Array.map serialize |> JsonValue.Array |]
    else
      raise (UnsupportedType t)

and private serializeList obj =
  let case, fields = FSharpValue.GetUnionFields (obj, obj.GetType(), false)
  if case.Name = "Cons" then
    serialize fields[0] :: serializeList fields[1]
  else
    []

exception TypeMismatched of System.Type * JsonValue

let rec deserializeByType (t: System.Type) (json: JsonValue) : obj =
  let fail () = TypeMismatched (t, json) |> raise
  if t.IsArray && t.HasElementType then
    match json with
    | JsonValue.Array src ->
      let elmType = t.GetElementType()
      let dst = System.Array.CreateInstance (elmType, src.Length)
      src |> Array.iteri (fun i obj -> dst.SetValue(deserializeByType elmType obj, i))
      dst :> obj
    | _ -> fail()
  elif t.IsGenericType && t.GetGenericTypeDefinition() = typedefof<option<_>> then
    let cases = FSharpType.GetUnionCases t
    if json = JsonValue.Null then 
      FSharpValue.MakeUnion (cases[0], [||])
    else
      FSharpValue.MakeUnion (cases[1], [| deserializeByType t.GenericTypeArguments[0] json |])
  elif t.IsGenericType && t.GetGenericTypeDefinition() = typedefof<list<_>> then
    match json with
    | JsonValue.Array src ->
      let elmType = t.GenericTypeArguments[0]
      let arr = src |> Array.map (deserializeByType elmType)
      let cons = t.GetMethod "Cons"
      let empty = (t.GetProperty "Empty").GetValue null
      Array.foldBack (fun item list -> cons.Invoke (null, [| item; list |])) arr empty
    | _ -> fail()
  elif t.IsGenericType && t.GetGenericTypeDefinition() = typedefof<Map<_, _>> then
    match json with
    | JsonValue.Record src ->
      let ctor = t.GetConstructors()[0]
      let keyType = t.GenericTypeArguments[0]
      let valueType = t.GenericTypeArguments[1]
      let tupleType = FSharpType.MakeTupleType [| keyType; valueType |]
      let dst = System.Array.CreateInstance (tupleType, src.Length)
      src |> Array.iteri (fun i (key, value) ->
        let key =
          JsonValue.TryParse key
          |> Option.map (deserializeByType keyType)
          |> Option.defaultValue key
        let value = deserializeByType valueType value
        dst.SetValue(FSharpValue.MakeTuple ([| key; value |], tupleType), i))
      ctor.Invoke [| dst |]
    | _ -> fail()
  elif FSharpType.IsUnion (t, bindingFlags) then
    let cases = FSharpType.GetUnionCases (t, true)
    if cases.Length = 1 then // single case union
      match cases[0].GetFields(), json with
      | [| field |], _ ->
        FSharpValue.MakeUnion (cases[0], [| deserializeByType field.PropertyType json |], bindingFlags)
      | fields, JsonValue.Array a when fields.Length = a.Length ->
        Array.zip fields a
        |> Array.map (fun (field, json) -> deserializeByType field.PropertyType json)
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
            | [| field |], _ -> FSharpValue.MakeUnion (case, [| deserializeByType field.PropertyType json |])
            | fields, JsonValue.Array a when fields.Length = a.Length -> 
              Array.zip fields a
              |> Array.map (fun (field, json) -> deserializeByType field.PropertyType json)
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
        |> Array.map (fun (elmType, (_, obj)) -> deserializeByType elmType obj)
      FSharpValue.MakeRecord (t, values, true)
    | _ -> fail ()
  elif FSharpType.IsTuple t then
    let elmTypes = FSharpType.GetTupleElements t
    match json with
    | JsonValue.Array src when src.Length = elmTypes.Length ->
      Array.zip elmTypes src
      |> Array.map (fun (t, json) -> deserializeByType t json)
      |> fun values -> FSharpValue.MakeTuple (values, t)
    | _ -> fail ()
  else
    match json with
    | JsonValue.String s ->
      if t = typeof<string> then
        s :> obj
      elif FSharpType.IsUnion (t, bindingFlags) then
        FSharpType.GetUnionCases (t, true)
        |> Array.tryFind (fun case -> case.Name = s && case.GetFields().Length = 0)
        |> function Some case -> FSharpValue.MakeUnion (case, [||]) | _ -> fail()
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

let deserialize<'a> json = deserializeByType typeof<'a> json :?> 'a
