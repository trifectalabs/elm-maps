module Vector_tile exposing (..)


import Json.Decode as JD exposing ((:=))
import Json.Encode as JE


(<$>) : (a -> b) -> JD.Decoder a -> JD.Decoder b
(<$>) =
  JD.map


(<*>) : JD.Decoder (a -> b) -> JD.Decoder a -> JD.Decoder b
(<*>) f v =
  f `JD.andThen` \x -> x <$> v


optionalDecoder : JD.Decoder a -> JD.Decoder (Maybe a)
optionalDecoder decoder =
  JD.oneOf
    [ JD.map Just decoder
    , JD.succeed Nothing
    ]


requiredFieldDecoder : String -> a -> JD.Decoder a -> JD.Decoder a
requiredFieldDecoder name default decoder =
  withDefault default (name := decoder)


optionalFieldDecoder : String -> JD.Decoder a -> JD.Decoder (Maybe a)
optionalFieldDecoder name decoder =
  optionalDecoder (name := decoder)


repeatedFieldDecoder : String -> JD.Decoder a -> JD.Decoder (List a)
repeatedFieldDecoder name decoder =
  withDefault [] (name := (JD.list decoder))


withDefault : a -> JD.Decoder a -> JD.Decoder a
withDefault default decoder =
  JD.oneOf
    [ decoder
    , JD.succeed default
    ]


optionalEncoder : String -> (a -> JE.Value) -> Maybe a -> Maybe (String, JE.Value)
optionalEncoder name encoder v =
  case v of
    Just x ->
      Just (name, encoder x)

    Nothing ->
      Nothing


requiredFieldEncoder : String -> (a -> JE.Value) -> a -> a -> Maybe (String, JE.Value)
requiredFieldEncoder name encoder default v =
  if
    v == default
  then
    Nothing
  else
    Just (name, encoder v)


repeatedFieldEncoder : String -> (a -> JE.Value) -> List a -> Maybe (String, JE.Value)
repeatedFieldEncoder name encoder v =
  case v of
    [] ->
      Nothing
    _ ->
      Just (name, JE.list <| List.map encoder v)


type alias Tile =
  { layers : List Vector_tile.Tile_Layer -- 3
  }


type Tile_GeomType
  = Tile_Unknown -- 0
  | Tile_Point -- 1
  | Tile_Linestring -- 2
  | Tile_Polygon -- 3


tileDecoder : JD.Decoder Tile
tileDecoder =
  Tile
    <$> (repeatedFieldDecoder "layers" vector_tile_Tile_LayerDecoder)


tile_GeomTypeDecoder : JD.Decoder Tile_GeomType
tile_GeomTypeDecoder =
  let
    lookup s = case s of
      "UNKNOWN" -> Tile_Unknown
      "POINT" -> Tile_Point
      "LINESTRING" -> Tile_Linestring
      "POLYGON" -> Tile_Polygon
      _ -> Tile_Unknown
  in
    JD.map lookup JD.string


tile_GeomTypeDefault : Tile_GeomType
tile_GeomTypeDefault = Tile_Unknown


tileEncoder : Tile -> JE.Value
tileEncoder v =
  JE.object <| List.filterMap identity <|
    [ (repeatedFieldEncoder "layers" vector_tile_Tile_LayerEncoder v.layers)
    ]


tile_GeomTypeEncoder : Tile_GeomType -> JE.Value
tile_GeomTypeEncoder v =
  let
    lookup s = case s of
      Tile_Unknown -> "UNKNOWN"
      Tile_Point -> "POINT"
      Tile_Linestring -> "LINESTRING"
      Tile_Polygon -> "POLYGON"
  in
    JE.string <| lookup v


type alias Tile_Value =
  { stringValue : String -- 1
  , floatValue : Float -- 2
  , doubleValue : Float -- 3
  , intValue : Int -- 4
  , uintValue : Int -- 5
  , sintValue : Int -- 6
  , boolValue : Bool -- 7
  }


tile_ValueDecoder : JD.Decoder Tile_Value
tile_ValueDecoder =
  Tile_Value
    <$> (requiredFieldDecoder "stringValue" "" JD.string)
    <*> (requiredFieldDecoder "floatValue" 0.0 JD.float)
    <*> (requiredFieldDecoder "doubleValue" 0.0 JD.float)
    <*> (requiredFieldDecoder "intValue" 0 JD.int)
    <*> (requiredFieldDecoder "uintValue" 0 JD.int)
    <*> (requiredFieldDecoder "sintValue" 0 JD.int)
    <*> (requiredFieldDecoder "boolValue" False JD.bool)


tile_ValueEncoder : Tile_Value -> JE.Value
tile_ValueEncoder v =
  JE.object <| List.filterMap identity <|
    [ (requiredFieldEncoder "stringValue" JE.string "" v.stringValue)
    , (requiredFieldEncoder "floatValue" JE.float 0.0 v.floatValue)
    , (requiredFieldEncoder "doubleValue" JE.float 0.0 v.doubleValue)
    , (requiredFieldEncoder "intValue" JE.int 0 v.intValue)
    , (requiredFieldEncoder "uintValue" JE.int 0 v.uintValue)
    , (requiredFieldEncoder "sintValue" JE.int 0 v.sintValue)
    , (requiredFieldEncoder "boolValue" JE.bool False v.boolValue)
    ]


type alias Tile_Feature =
  { id : Int -- 1
  , tags : List Int -- 2
  , type : Vector_tile.Tile_GeomType -- 3
  , geometry : List Int -- 4
  }


tile_FeatureDecoder : JD.Decoder Tile_Feature
tile_FeatureDecoder =
  Tile_Feature
    <$> (requiredFieldDecoder "id" 0 JD.int)
    <*> (repeatedFieldDecoder "tags" JD.int)
    <*> (requiredFieldDecoder "type" vector_tile_Tile_GeomTypeDefault vector_tile_Tile_GeomTypeDecoder)
    <*> (repeatedFieldDecoder "geometry" JD.int)


tile_FeatureEncoder : Tile_Feature -> JE.Value
tile_FeatureEncoder v =
  JE.object <| List.filterMap identity <|
    [ (requiredFieldEncoder "id" JE.int 0 v.id)
    , (repeatedFieldEncoder "tags" JE.int v.tags)
    , (requiredFieldEncoder "type" vector_tile_Tile_GeomTypeEncoder vector_tile_Tile_GeomTypeDefault v.type)
    , (repeatedFieldEncoder "geometry" JE.int v.geometry)
    ]


type alias Tile_Layer =
  { version : Int -- 15
  , name : String -- 1
  , features : List Vector_tile.Tile_Feature -- 2
  , keys : List String -- 3
  , values : List Vector_tile.Tile_Value -- 4
  , extent : Int -- 5
  }


tile_LayerDecoder : JD.Decoder Tile_Layer
tile_LayerDecoder =
  Tile_Layer
    <$> (requiredFieldDecoder "version" 0 JD.int)
    <*> (requiredFieldDecoder "name" "" JD.string)
    <*> (repeatedFieldDecoder "features" vector_tile_Tile_FeatureDecoder)
    <*> (repeatedFieldDecoder "keys" JD.string)
    <*> (repeatedFieldDecoder "values" vector_tile_Tile_ValueDecoder)
    <*> (requiredFieldDecoder "extent" 0 JD.int)


tile_LayerEncoder : Tile_Layer -> JE.Value
tile_LayerEncoder v =
  JE.object <| List.filterMap identity <|
    [ (requiredFieldEncoder "version" JE.int 0 v.version)
    , (requiredFieldEncoder "name" JE.string "" v.name)
    , (repeatedFieldEncoder "features" vector_tile_Tile_FeatureEncoder v.features)
    , (repeatedFieldEncoder "keys" JE.string v.keys)
    , (repeatedFieldEncoder "values" vector_tile_Tile_ValueEncoder v.values)
    , (requiredFieldEncoder "extent" JE.int 0 v.extent)
    ]
