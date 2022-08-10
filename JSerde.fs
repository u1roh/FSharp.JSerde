module FSharp.JSerde
open Microsoft.FSharp.Reflection
open FSharp.Data

exception UnsupportedType of System.Type

let private bindingFlags = System.Reflection.BindingFlags.Public ||| System.Reflection.BindingFlags.NonPublic

let rec serialize (obj: obj) =
  if isNull obj then JsonValue.Null else
  match obj with
  | :? bool as value -> JsonValue.Boolean value
  | :? int as value -> JsonValue.Number (decimal value)
  | :? float as value -> JsonValue.Number (decimal value)
  | :? decimal as value -> JsonValue.Number value
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

let rec deserialize (t: System.Type) (json: JsonValue) : obj =
  let fail () = TypeMismatched (t, json) |> raise
  match json with
  | JsonValue.Null -> null
  | JsonValue.String s ->
    if t = typeof<string> then s :> obj else fail ()
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
  | JsonValue.Array src ->
    if t.IsArray && t.HasElementType then
      let elmType = t.GetElementType()
      let dst = System.Array.CreateInstance (elmType, src.Length)
      src |> Array.iteri (fun i obj -> dst.SetValue(deserialize elmType obj, i))
      dst :> obj
    elif t.IsGenericType && t.GetGenericTypeDefinition() = typedefof<list<_>> then
      let elmType = t.GenericTypeArguments[0]
      let arr = src |> Array.map (deserialize elmType)
      let cons = t.GetMethod "Cons"
      let empty = (t.GetProperty "Empty").GetValue null
      Array.foldBack (fun item list -> cons.Invoke (null, [| item; list |])) arr empty
    else fail ()
  | JsonValue.Record src ->
    if FSharpType.IsRecord (t, bindingFlags) then
      let values =
        FSharpType.GetRecordFields (t, bindingFlags)
        |> Array.map (fun field -> field.PropertyType, src |> Array.find (fst >> (=) field.Name))
        |> Array.map (fun (elmType, (_, obj)) -> deserialize elmType obj)
      FSharpValue.MakeRecord (t, values, true)
    elif FSharpType.IsUnion (t, bindingFlags) then
      let case, fields = FSharpValue.GetUnionFields (obj, t, true)
      if (FSharpType.GetUnionCases (t, true)).Length = 1 then // single case union
        ()
      else
        ()
    elif t.IsGenericType && t.GetGenericTypeDefinition() = typedefof<Map<_, _>> then
      ()
    else fail ()
