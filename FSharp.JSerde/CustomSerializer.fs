namespace FSharp.JSerde
open FSharp.Data

/// Interface of custom serializer. The instances can be created by `JSerde.custom` function.
type ICustomSerializer =
  abstract TargetType: System.Type
  abstract Serialize: obj -> JsonValue
  abstract Deserialize: JsonValue -> obj

/// Collection of `ICustomSerializer`. `JSerde`'s `serialize`/`deserialize` can be customized by this.
type Serializer (customs: ICustomSerializer seq) =
  let customs = customs |> Seq.map (fun item -> item.TargetType, item) |> readOnlyDict 

  member _.Serialize (obj: obj): JsonValue option =
    if customs.Count = 0 then None else
      let t = obj.GetType() in if t.IsGenericType then t.GetGenericTypeDefinition() else t
      |> customs.TryGetValue
      |> function (true, ser) -> Some (ser.Serialize obj) | _ -> None

  member _.Deserialize (ty: System.Type, json: JsonValue): obj option =
    if customs.Count = 0 then None else
      customs.TryGetValue ty
      |> function (true, ser) -> Some (ser.Deserialize json) | _ -> None

  static member Empty = Serializer Seq.empty
