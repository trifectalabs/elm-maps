module Main exposing (..)

import String exposing (append)
import Maybe exposing (andThen)
import Json.Encode
import Task exposing (perform)
import Http
import Html.App as App
import Svg exposing (polygon)
import Html exposing (Html)
import Svg.Attributes exposing (version, x, y, viewBox, fill, points)
import GeoJson exposing
  ( GeoJson, GeoJsonObject(..), Geometry(..), FeatureObject
  , decoder
  )
import GeoJsonHelpers exposing (..)


sampleUrl : String
sampleUrl =
    "http://localhost:3000/sample.json"


main : Program Never
main =
    App.program
        { init = init , view = view , update = update , subscriptions = subscriptions }


-- Subscriptions

subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


-- Model

type alias Model = GeoJson

init : ( Model, Cmd Msg )
init =
    ( ( (Geometry(Point(0,0,[]) ) ), Nothing ) , fetchPerform sampleUrl )


-- Update

type Msg
    = FetchSuccess GeoJson
    | FetchFail Http.Error

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
    FetchSuccess geoJson ->
      (geoJson, Cmd.none)

    FetchFail err ->
      (model, Cmd.none)


-- View

view : Model -> Html Msg
view model =
  let
    svgPolys = generatePolygonStrings model
      |> List.map (\p -> polygon [ fill "#F0AD00", points p ] [])
      |> Svg.svg []
  in
    Svg.svg
      [ version "1.1" , x "0" , y "0" , viewBox "0 0 180 180" ] [ svgPolys ]


fetchPerform : String -> Cmd Msg
fetchPerform url =
  Task.perform FetchFail FetchSuccess (Http.get decoder url)

