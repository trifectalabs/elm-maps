module GeoJsonHelpers exposing (..)


import String exposing (concat)
import Json.Encode
import GeoJson exposing
  (GeoJson, GeoJsonObject(..), Geometry(..), FeatureObject)


parseFeatureObject : GeoJson -> Maybe FeatureObject
parseFeatureObject geojson =
  case (fst geojson) of
    Geometry g ->
      Nothing
    Feature f ->
      Just f
    FeatureCollection fc ->
      Nothing


parseFeatureCollection : GeoJson -> Maybe (List FeatureObject)
parseFeatureCollection geojson =
  case (fst geojson) of
    Geometry g ->
      Nothing
    Feature f ->
      Nothing
    FeatureCollection fc ->
      Just fc


parseGeometry : GeoJson -> Geometry
parseGeometry geojson =
  geojson
    |> parseFeatureCollection
    |> Maybe.withDefault []
    |> List.head
    |> Maybe.withDefault
      { geometry = Nothing
      , properties = Json.Encode.string ""
      , id = Nothing
      }
    |> .geometry
    |> Maybe.withDefault (Point (0, 0, []))

parsePolygonCoordinates : Geometry -> List (List (Float, Float))
parsePolygonCoordinates poly =
  case poly of
    Point p ->
      case p of
        (lng, lat, _) ->
          [[(lng, lat)]]
    MultiPoint p ->
      [ (List.map (\(lng, lat, _) -> (lng, lat)) p) ]
    LineString p ->
      [ (List.map (\(lng, lat, _) -> (lng, lat)) p) ]
    MultiLineString p ->
      let
        multiLineString = p
          |> List.head
          |> Maybe.withDefault []
          |> List.map (\(lng, lat, _) -> (lng, lat))
      in
       [multiLineString]
    Polygon p ->
      let
        polygon = p
          |> List.head
          |> Maybe.withDefault []
          |> List.map (\(lng, lat, _) -> (lng, lat))
      in
        [polygon]
    MultiPolygon p ->
      List.map (\polygon ->
          List.head polygon
            |> Maybe.withDefault []
            |> List.map (\(lng, lat, _) -> (lng, lat))) p
    GeometryCollection p ->
      List.concat (List.map (\subGeo -> parsePolygonCoordinates subGeo) p)


generatePolygonStrings : GeoJson -> List String
generatePolygonStrings geojson =
  let
    geometry = geojson
      |> parseFeatureCollection
      |> Maybe.withDefault []
      |> List.head
      |> Maybe.withDefault
        { geometry = Nothing
        , properties = Json.Encode.string ""
        , id = Nothing
        }
      |> .geometry
      |> Maybe.withDefault (Point (0, 0, []))
    coordinates = parsePolygonCoordinates geometry
    pointsToString = (\points ->
      List.map
        (\( p1, p2 ) -> (String.concat [ (toString p1), ",", (toString p2) ]))
        points
    )
    polygonStrings = coordinates
      |> List.map pointsToString
      |> List.map (\polygon -> (String.concat (List.intersperse " " polygon)))
  in
    polygonStrings
