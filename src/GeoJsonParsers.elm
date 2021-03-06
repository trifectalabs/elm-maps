module GeoJsonParsers exposing (..)

import Tuple exposing (first, second)
import String exposing (concat)
import Json.Encode
import CanonicalTypes exposing (Tile)
import GeoJson exposing
  (GeoJson, GeoJsonObject(..), Geometry(..), FeatureObject, decoder)


parseFeatureObject : GeoJson -> Maybe FeatureObject
parseFeatureObject geojson =
  case (first geojson) of
    Geometry g ->
      Nothing
    Feature f ->
      Just f
    FeatureCollection fc ->
      Nothing


parseGeometry : GeoJson -> Maybe Geometry
parseGeometry geojson =
  case (first geojson) of
    Geometry g ->
      Just g
    Feature f ->
      Nothing
    FeatureCollection fc ->
      Nothing


parseFeatureCollection : GeoJson -> Maybe (List FeatureObject)
parseFeatureCollection geojson =
  case (first geojson) of
    Geometry g ->
      Nothing
    Feature f ->
      Nothing
    FeatureCollection fc ->
      Just fc


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
          |> List.map (\(lng, lat, _) -> normalizeCoordinate (lng, lat))
      in
        [polygon]
    MultiPolygon p ->
      List.map (\polygon ->
          List.head polygon
            |> Maybe.withDefault []
            |> List.map (\(lng, lat, _) -> normalizeCoordinate (lng, lat))) p
    GeometryCollection p -> List.concat (List.map (\subGeo -> parsePolygonCoordinates subGeo) p)


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
      |> Maybe.withDefault (Point (0, 0, 0))
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

parseToTile : GeoJson -> Tile
parseToTile geojson =
  geojson
    |> parseFeatureCollection
    |> Maybe.withDefault []
    |> List.map (\featureObject ->
      let
        geom =featureObject.geometry
          |> Maybe.withDefault (Point (0, 0, 0))
      in
        parsePolygonCoordinates geom)
    |> List.concat

normalizeCoordinate : (Float, Float) -> (Float, Float)
normalizeCoordinate coordinate =
  (180 + first coordinate , 90 - second coordinate)


