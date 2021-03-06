module Tests exposing (..)


import Tuple exposing (first, second)
import GeoJson exposing (..)
import GeoJsonParsers exposing (..)
import Test exposing (..)
import Json.Encode
import Expect


all : Test
all =
  describe "The GeoJson Helpers"
    [ describe "Parse elements from geojson"
      [ test "can parse geometry from GeoJson" <|
          \() ->
            let
              testGeoJson = (Geometry (Point (1,3,0)), Nothing)
            in
              Expect.equal (parseGeometry testGeoJson) (Just (Point (1, 3, 0)))

      , test "can parse feature collection from GeoJson" <|
          \() ->
            let
              featureObject =
                { geometry = Just (Point(1, 3, 0))
                , properties = Json.Encode.string ""
                , id = Nothing
                }
              testGeoJson = (FeatureCollection([ featureObject ]) , Nothing)
            in
              Expect.equal
                (parseFeatureCollection testGeoJson)
                (Just [ featureObject ])

      , test "can parse polygon coordinates from geometry with a single polygon" <|
          \() ->
            let
              coordinates1 = (10.23, 88.33)
              coordinates2 = (18.29, 73.02)
              geometry = Polygon([
                [ (first coordinates1, second coordinates1, 3)
                , (first coordinates2, second coordinates2, 0)
                ]
              ])
            in
              Expect.equal
                (parsePolygonCoordinates geometry)
                ([ [ normalizeCoordinate coordinates1
                   , normalizeCoordinate coordinates2 ] ])

      , test "can parse polygon coordinates from geometry with multiple polygons" <|
          \() ->
            let
              coordinates1 = (10.45, 8.33)
              coordinates2 = (22.85, 93.11)
              coordinates3 = (74.29, 123.09)
              geometry = MultiPolygon(
                [ [ [ (first coordinates1, second coordinates1, 0) ] ]
                , [ [ (first coordinates2, second coordinates2, 0) ] ]
                , [ [ (first coordinates3, second coordinates3, 0) ] ]
                ]
              )
            in
              Expect.equal
                (parsePolygonCoordinates geometry)
                ([ [ normalizeCoordinate coordinates1 ]
                 , [ normalizeCoordinate coordinates2 ]
                 , [ normalizeCoordinate coordinates3 ] ])

      , test "can generate a single-polygon string for rendering from GeoJson" <|
          \() ->
            let
              coordinates1 = (10.45, 8.33)
              coordinates2 = (22.85, 93.11)
              coordinates3 = (74.29, 123.09)
              geometry = Polygon([
                [ (first coordinates1, second coordinates1, 0)
                , (first coordinates2, second coordinates2, 0)
                , (first coordinates3, second coordinates3, 0)
                ]
              ])
              featureObject =
                { geometry = Just (geometry)
                , properties = Json.Encode.string ""
                , id = Nothing
                }
              testGeoJson = ( FeatureCollection( [ featureObject ] ) , Nothing)
            in
              Expect.equal
                (generatePolygonStrings testGeoJson)
                [ "190.45,81.67 202.85,-3.1099999999999994 254.29000000000002,-33.09" ]

      , test "can generate a multi-polygon strings for rendering from GeoJson" <|
          \() ->
            let
              coordinates1_1 = (10.45, 8.33)
              coordinates1_2 = (22.85, 93.11)
              coordinates2_1 = (74.29, 123.09)
              coordinates2_2 = (34.20, 171.89)
              geometry = MultiPolygon([
                [ [ (first coordinates1_1, second coordinates1_1, 0)
                ,   (first coordinates1_2, second coordinates1_2, 0)
                ] ]
              , [ [ (first coordinates2_1, second coordinates2_1, 0)
                ,   (first coordinates2_2, second coordinates2_2, 0)
                ] ]
              ])
              featureObject =
                { geometry = Just (geometry)
                , properties = Json.Encode.string ""
                , id = Nothing
                }
              testGeoJson = (FeatureCollection([ featureObject ]), Nothing)
            in
              Expect.equal
                (generatePolygonStrings testGeoJson)
                [ "190.45,81.67 202.85,-3.1099999999999994"
                ,"254.29000000000002,-33.09 214.2,-81.88999999999999" ]
      ]
    ]
