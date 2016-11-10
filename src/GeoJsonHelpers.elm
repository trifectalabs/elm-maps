module GeoJsonHelpers exposing (..)

import String exposing (concat)
import Json.Encode
import GeoJson exposing (GeoJson, GeoJsonObject(..), Geometry(..), FeatureObject)


parseFeatureObject : GeoJson -> Maybe FeatureObject
parseFeatureObject geojson =
  case (fst geojson) of
    Geometry g ->
      Nothing

    Feature f ->
      Just f

    FeatureCollection fc ->
      Nothing


parseGeometry : GeoJson -> Maybe Geometry
parseGeometry geojson =
  case (fst geojson) of
    Geometry g ->
      Just g

    Feature f ->
      Nothing

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


parsePolygonCoordinates : Geometry -> Maybe (List (List ( Float, Float )))
parsePolygonCoordinates poly =
  case poly of
    Point p ->
      case p of
        ( one, two, three ) ->
          Just [ [ ( one, two ) ] ]

    MultiPoint p ->
      Just [ (List.map (\( one, two, three ) -> ( one, two )) p) ]

    LineString p ->
      Just [ (List.map (\( one, two, three ) -> ( one, two )) p) ]

    MultiLineString p ->
      Just
        [ (List.head p
        |> Maybe.withDefault []
        |> List.map (\( one, two, three ) -> ( one, two )))
        ]

    Polygon p ->
      Just [(List.head p
        |> Maybe.withDefault []
        |> List.map (\(one, two, three) -> (one, two)))]

    MultiPolygon p ->
      Just
        (List.map (\polygon ->
          List.head polygon
            |> Maybe.withDefault []
            |> List.map (\( one, two, three ) -> ( one, two ) ) ) p )

    GeometryCollection p ->
      Just (List.concat (List.filterMap (\subGeo -> parsePolygonCoordinates subGeo) p))


generatePolygonStrings : GeoJson -> List String
generatePolygonStrings geojson =
  parsePolygonCoordinates
    ((geojson
    |> parseFeatureCollection
    |> Maybe.withDefault []
    |> List.head
    |> Maybe.withDefault { geometry = Nothing, properties = Json.Encode.string "", id = Nothing }
    ).geometry
      |> Maybe.withDefault (Point ( 0, 0, [] )))
        |> Maybe.withDefault []
        |> List.map (\points -> List.map (\( p1, p2 ) -> (String.concat [ (toString p1), ",", (toString p2) ])) points)
        |> List.map (\polygon -> (String.concat (List.intersperse " " polygon)))
