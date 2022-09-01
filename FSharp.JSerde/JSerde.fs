module FSharp.JSerde.JSerde
open Microsoft.FSharp.Reflection
open FSharp.Data

/// Create custom serializer
let custom<'a> (serialize: 'a -> JsonValue) (deserialize: JsonValue -> 'a) =
  { new ICustomSerializer with
      member _.TargetType = typeof<'a>
      member _.Serialize obj = serialize (obj :?> 'a)
      member _.Deserialize json = deserialize json :> obj }

type UnionTagging = {
  Tag: string
  Content: string
}

type Config = {
  Serializer: Serializer
  UnionTagging: UnionTagging option
} with
  static member Default =
    { Serializer = Serializer.Empty
      UnionTagging = None }

  static member FromCustomSerializers customs =
    { Config.Default with Serializer = Serializer customs }


// ------------------------------------------------------------------------
// serialization


exception UnsupportedType of System.Type

let private bindingFlags = System.Reflection.BindingFlags.Public ||| System.Reflection.BindingFlags.NonPublic

/// Serialize F# value into <c>FSharp.Data.JsonValue</c>
let rec toJsonValue cfg (obj: obj) =
  if isNull obj then JsonValue.Null else
  match cfg.Serializer.Serialize obj with
  | Some json -> json
  | _ ->
    match obj with
    | :? string as value -> JsonValue.String value
    | :? char as value -> JsonValue.String (string value)
    | :? bool as value -> JsonValue.Boolean value
    | :? int as value -> JsonValue.Number (decimal value)
    | :? uint as value -> JsonValue.Number (decimal value)
    | :? double as value -> JsonValue.Float value
    | :? single as value -> JsonValue.Float (double value)
    | :? decimal as value -> JsonValue.Number value
    | :? int64 as value -> JsonValue.Number (decimal value)
    | :? uint64 as value -> JsonValue.Number (decimal value)
    | :? int16 as value -> JsonValue.Number (decimal value)
    | :? uint16 as value -> JsonValue.Number (decimal value)
    | :? byte as value -> JsonValue.Number (decimal value)
    | :? sbyte as value -> JsonValue.Number (decimal value)
    | :? nativeint as value -> JsonValue.Number (decimal value)
    | :? unativeint as value -> JsonValue.Number (decimal value)
    | :? System.Array as a -> Array.init a.Length (fun i -> a.GetValue i |> toJsonValue cfg) |> JsonValue.Array
    | _ ->
      let t = obj.GetType()
      if FSharpType.IsTuple t then
        FSharpValue.GetTupleFields obj
        |> Array.map (toJsonValue cfg)
        |> JsonValue.Array
      elif t.IsGenericType && t.GetGenericTypeDefinition() = typedefof<option<_>> then
        let case, fields = FSharpValue.GetUnionFields (obj, obj.GetType(), false)
        if case.Name = "Some"
          then toJsonValue cfg fields[0]
          else JsonValue.Null
      elif t.IsGenericType && t.GetGenericTypeDefinition() = typedefof<list<_>> then
        serializeList cfg obj
        |> List.toArray
        |> JsonValue.Array
      elif t.IsGenericType && t.GetGenericTypeDefinition() = typedefof<Map<_, _>> then
        obj :?> System.Collections.IEnumerable
        |> Seq.cast<obj>
        |> Seq.map (fun pair ->
          let t = pair.GetType()
          let get name = (t.GetProperty name).GetValue pair |> toJsonValue cfg
          let key = get "Key" |> function JsonValue.String s -> s | json -> string json
          key, get "Value")
        |> Seq.toArray
        |> JsonValue.Record
      elif FSharpType.IsRecord (t, bindingFlags) then
        FSharpType.GetRecordFields (t, bindingFlags)
        |> Array.map (fun prop -> prop.Name, FSharpValue.GetRecordField (obj, prop) |> toJsonValue cfg)
        |> Array.filter (snd >> (<>) JsonValue.Null)
        |> JsonValue.Record
      elif FSharpType.IsUnion (t, bindingFlags) then
        let case, fields = FSharpValue.GetUnionFields (obj, t, true)
        (match fields |> Array.map (toJsonValue cfg) with
          | [||] -> None
          | [| item |] -> Some item
          | array -> JsonValue.Array array |> Some)
        |> Option.map (fun json ->
          match FSharpType.GetUnionCases (t, true) with
          | [| case |] when case.Name = t.Name -> json // single case union
          | _ ->
            match cfg.UnionTagging with
            | Some { Tag = t; Content = c } -> JsonValue.Record [| t, JsonValue.String case.Name; c, json |]
            | None -> JsonValue.Record [| case.Name, json |])
        |> Option.defaultValue (JsonValue.String case.Name)
      elif t.IsEnum then
        let name = System.Enum.GetName (t, obj)
        if System.String.IsNullOrEmpty name
          then System.Convert.ChangeType (obj, t.GetEnumUnderlyingType()) |> toJsonValue cfg
          else JsonValue.String name
      elif t.GetMethod ("Parse", [| typeof<string> |]) |> isNull |> not then
        obj.ToString() |> JsonValue.String
      else
        raise (UnsupportedType t)

and private serializeList cfg obj =
  let case, fields = FSharpValue.GetUnionFields (obj, obj.GetType(), false)
  if case.Name = "Cons" then
    toJsonValue cfg fields[0] :: serializeList cfg fields[1]
  else
    []

/// Serialize F# value into JSON string
let toJsonStringWith fmt cfg (obj: obj) =
  (toJsonValue cfg obj).ToString(if fmt then JsonSaveOptions.None else JsonSaveOptions.DisableFormatting)

/// Serialize F# value into unformatted JSON string
let toJsonString: _ -> obj -> _ = toJsonStringWith false

/// Serialize F# value into formatted JSON string
let toPrettyJsonString: _ -> obj -> _ = toJsonStringWith true


// ------------------------------------------------------------------------
// deserialization


exception TypeMismatch of Type:System.Type * Json:JsonValue with
  override this.Message =
    sprintf "`%s` doesn't match with %A" this.Type.FullName this.Json

let rec private fromJsonValueByType cfg (t: System.Type) (json: JsonValue) : obj =
  let fail () = TypeMismatch (t, json) |> raise
  match cfg.Serializer.Deserialize (t, json) with
  | Some obj -> obj
  | _ ->
    match DesUtil.classify t, json with
    | DesUtil.Array (elmType, createArray), JsonValue.Array src ->
      src
      |> Array.map (fun obj -> fromJsonValueByType cfg elmType obj)
      |> createArray
    | DesUtil.Option (elmType, createOption), json ->
      if json = JsonValue.Null
        then None
        else fromJsonValueByType cfg elmType json |> Some 
      |> createOption
    | DesUtil.List (elmType, createList), JsonValue.Array src ->
      src
      |> Array.map (fromJsonValueByType cfg elmType)
      |> createList
    | DesUtil.Map (keyType, valueType, createMap), JsonValue.Record src ->
      src
      |> Array.map (fun (key, value) ->
        let key =
          JsonValue.TryParse key
          |> Option.map (fromJsonValueByType cfg keyType)
          |> Option.defaultValue key
        let value = fromJsonValueByType cfg valueType value
        key, value)
      |> createMap
    | DesUtil.SingleCaseUnion (fields, create), json ->
      match fields, json with
      | [| field |], _ -> create [| fromJsonValueByType cfg field.PropertyType json |]
      | fields, JsonValue.Array a when fields.Length = a.Length ->
        Array.zip fields a
        |> Array.map (fun (field, json) -> fromJsonValueByType cfg field.PropertyType json)
        |> create
      | _ -> fail()
    | DesUtil.Union (cases, create), JsonValue.String name ->
      cases
      |> Array.tryFind (fun case -> case.Name = name && case.GetFields().Length = 0)
      |> function Some case -> create case [||] | _ -> fail()
    | DesUtil.Union (cases, create), JsonValue.Record [| name, json |] ->
      (name, json)
      |> tryCreateUnionCase cfg (cases, create)
      |> function Some obj -> obj | _ -> fail ()
    | DesUtil.Union (cases, create), JsonValue.Record fields ->
      cfg.UnionTagging
      |> Option.bind (fun tag ->
        let t = fields |> Array.tryFind (fst >> (=) tag.Tag)
        let c = fields |> Array.tryFind (fst >> (=) tag.Content)
        match t, c with
        | Some (_, JsonValue.String name), Some (_, json) -> Some (name, json)
        | _ -> None)
      |> Option.bind (tryCreateUnionCase cfg (cases, create))
      |> function Some obj -> obj | _ -> fail ()
    | DesUtil.Record (fields, create), JsonValue.Record src ->
      fields
      |> Array.map (fun field ->
        src
        |> Array.tryFind (fst >> (=) field.Name)
        |> function Some (_, json) -> json | None -> JsonValue.Null
        |> fromJsonValueByType cfg field.PropertyType)
      |> create
    | DesUtil.Tuple (elmTypes, create), JsonValue.Array src when src.Length = elmTypes.Length ->
      Array.zip elmTypes src
      |> Array.map (fun (t, json) -> fromJsonValueByType cfg t json)
      |> create
    | DesUtil.Enum t, _ ->
      let mutable value = null
      match json with
      | JsonValue.String s when System.Enum.TryParse (t, s, &value) -> value
      | _ -> System.Enum.ToObject (t, fromJsonValueByType cfg (t.GetEnumUnderlyingType()) json)
    | DesUtil.String,   JsonValue.String s -> s :> obj
    | DesUtil.Char,     JsonValue.String s -> s[0] :> obj
    | DesUtil.Bool,     JsonValue.Boolean b -> b :> obj
    | DesUtil.Byte,     JsonValue.Number n -> byte   n :> obj
    | DesUtil.Byte,     JsonValue.Float  n -> byte   n :> obj
    | DesUtil.SByte,    JsonValue.Number n -> sbyte  n :> obj
    | DesUtil.SByte,    JsonValue.Float  n -> sbyte  n :> obj
    | DesUtil.Int16,    JsonValue.Number n -> int16  n :> obj
    | DesUtil.Int16,    JsonValue.Float  n -> int16  n :> obj
    | DesUtil.UInt16,   JsonValue.Number n -> uint16 n :> obj
    | DesUtil.UInt16,   JsonValue.Float  n -> uint16 n :> obj
    | DesUtil.Int32,    JsonValue.Number n -> int    n :> obj
    | DesUtil.Int32,    JsonValue.Float  n -> int    n :> obj
    | DesUtil.UInt32,   JsonValue.Number n -> uint   n :> obj
    | DesUtil.UInt32,   JsonValue.Float  n -> uint   n :> obj
    | DesUtil.Int64,    JsonValue.Number n -> int64  n :> obj
    | DesUtil.Int64,    JsonValue.Float  n -> int64  n :> obj
    | DesUtil.UInt64,   JsonValue.Number n -> uint64 n :> obj
    | DesUtil.UInt64,   JsonValue.Float  n -> uint64 n :> obj
    | DesUtil.Double,   JsonValue.Number n -> double n :> obj
    | DesUtil.Double,   JsonValue.Float  n -> n :> obj
    | DesUtil.Single,   JsonValue.Number n -> single n :> obj
    | DesUtil.Single,   JsonValue.Float  n -> single n :> obj
    | DesUtil.Decimal,  JsonValue.Number n -> n :> obj
    | DesUtil.Decimal,  JsonValue.Float  n -> decimal n :> obj
    | DesUtil.IntPtr,   JsonValue.Number n -> nativeint  (int64 n) :> obj
    | DesUtil.IntPtr,   JsonValue.Float  n -> nativeint  n :> obj
    | DesUtil.UIntPtr,  JsonValue.Number n -> unativeint (int64 n) :> obj
    | DesUtil.UIntPtr,  JsonValue.Float  n -> unativeint n :> obj
    | DesUtil.Parsable parse, JsonValue.String s -> parse s
    | _ -> fail ()

and private tryCreateUnionCase cfg (cases: UnionCaseInfo[], create) (name, json) =
  cases
  |> Array.tryFind (fun case -> case.Name = name)
  |> Option.bind (fun case ->
      match case.GetFields(), json with
      | [| field |], _ -> create case [| fromJsonValueByType cfg field.PropertyType json |] |> Some
      | fields, JsonValue.Array a when fields.Length = a.Length -> 
        Array.zip fields a
        |> Array.map (fun (field, json) -> fromJsonValueByType cfg field.PropertyType json)
        |> create case |> Some
      | _ -> None)

/// Deserialize <c>FSharp.Data.JsonValue</c> into F# type
let fromJsonValue<'a> cfg json = fromJsonValueByType cfg typeof<'a> json :?> 'a


/// Deserialize JSON string into F# type
let fromJsonString<'a> cfg json =
  JsonValue.Parse json
  |> fromJsonValue<'a> cfg
