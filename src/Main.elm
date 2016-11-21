module Main exposing (..)

import String exposing (append)
import Maybe exposing (andThen)
import Task exposing (perform)
import Json.Encode
import Json.Decode as Json exposing (field, at)
import Task exposing (perform)
import Tuple exposing (first, second)
import Http
import Mouse
import Html
import Html exposing (Html, div, span, text)
import Html.Events exposing (onWithOptions, onMouseDown, onMouseUp)
import Html.Attributes exposing (style)
import Svg exposing (polygon)
import Svg.Attributes exposing (version, x, y, viewBox, fill, points)
import GeoJsonParsers exposing (..)
import CanonicalTypes exposing (..)
import GeoJson exposing (GeoJson, decoder)
import Debug exposing (log)


sampleUrl : String
sampleUrl =
    "http://localhost:3000/sample.json"


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


-- Subscriptions

subscriptions : Model -> Sub Msg
subscriptions model =
  Mouse.moves (\{x, y} -> Position x y)


-- Model

type alias Model =
  { map: Map
  , mousePosition : (Int, Int)
  , prevPosition : (Int, Int)
  , dragging : Bool
  , topLeft : Coordinate
  , topRight : Coordinate
  , bottomLeft : Coordinate
  , bottomRight : Coordinate
  , zoomLevel : Float
  }

type alias Coordinate =
  { lat : Int
  , lng : Int
  }


type alias WheelEvent =
  { deltaX : Float
  , deltaY : Float
  , deltaZ : Float
  }


init : ( Model, Cmd Msg )
init =
    ( { map = []
      , mousePosition = (0, 0)
      , prevPosition = (0, 0)
      , dragging = False
      , topLeft = { lat = 0, lng = 0 }
      , topRight = { lat = 10, lng = 0 }
      , bottomLeft = { lat = 0, lng = 10 }
      , bottomRight = { lat = 10, lng = 10 }
      , zoomLevel = 1
      }
    , fetchPerform sampleUrl
    )


-- Update

type Msg
    = FetchGeoJson (Result Http.Error GeoJson)
    | StartDrag
    | StopDrag
    | Wheel WheelEvent
    | Position Int Int


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
    FetchGeoJson (Err _) ->
      (model, Cmd.none)
    FetchGeoJson (Ok geoJson) ->
      ({ model | map = parseToCanonicalModel geoJson }, Cmd.none)
    StartDrag ->
      ({ model | dragging = True }, Cmd.none)
    StopDrag ->
      ({ model | dragging = False }, Cmd.none)
    Position x y ->
      case model.dragging of
        False ->
          let
            newModel =
              { model
              | mousePosition = (x, y)
              , prevPosition = model.mousePosition
              }
          in
            (newModel, Cmd.none)
        True ->
          let
            moveLat = first model.mousePosition - first model.prevPosition
            moveLng = second model.prevPosition - second model.mousePosition
            newModel =
              { model
              | mousePosition = (x, y)
              , prevPosition = model.mousePosition
              , topLeft =
                { lat = model.topLeft.lat + moveLat
                , lng = model.topLeft.lng - moveLng
                }
              , topRight =
                { lat = model.topRight.lat + moveLat
                , lng = model.topRight.lng - moveLng
                }
              , bottomLeft =
                { lat = model.bottomLeft.lat + moveLat
                , lng = model.bottomLeft.lng - moveLng
                }
              , bottomRight =
                { lat = model.bottomRight.lat + moveLat
                , lng = model.bottomRight.lng - moveLng
                }
              }
          in
            (newModel, Cmd.none)
    Wheel event ->
      let
        updatedModel = zoom (normalizeZoom model (negate event.deltaY)) (negate event.deltaY)
      in
        (updatedModel, Cmd.none)


-- View

view : Model -> Html Msg
view model =
  let
    map = generateSvg model
    wheelOptions =
      { stopPropagation = False
      , preventDefault = True
      }
  in
    div
      [ onMouseDown StartDrag
      , onMouseUp StopDrag
      , onWithOptions "wheel" wheelOptions (Json.map Wheel <| wheelEventDecoder)
      , style [ ("height", "100vh"), ("width", "100vw") ]
      ]
      [ span
        [ style [ ("position", "absolute"), ("top", "0"), ("left", "0") ] ]
        [ text (toString model.topLeft) ]
      , span
        [ style [ ("position", "absolute"), ("top", "0"), ("left", "200px") ] ]
        [ text <| String.concat [ "Mouse Position: ", (toString model.mousePosition) ] ]
      , span
        [ style [ ("position", "absolute"), ("top", "0"), ("right", "0") ] ]
        [ text (toString model.topRight) ]
      , span
        [ style [ ("position", "absolute"), ("bottom", "0"), ("left", "0") ] ]
        [ text (toString model.bottomLeft) ]
      , span
        [ style [ ] ]
        [ map ]
      , span
        [ style [ ("position", "absolute"), ("bottom", "0"), ("right", "0") ] ]
        [ text (toString model.bottomRight) ]
      , span
        [ style [ ("position", "absolute"), ("top", "0"), ("left", "400px") ] ]
        [ text <| String.concat [ "Zoom level:", (toString model.zoomLevel) ] ]
      ]

zoom : Model -> Float -> Model
zoom model delta =
  let
    zoomLevel = model.zoomLevel
    mousePos = model.mousePosition
    latDistanceFromMouse = (\lat -> toFloat <| abs <| round lat - (first mousePos))
    lngDistanceFromMouse = (\lng -> toFloat <| abs <| round lng - (second mousePos))
    shiftLat = (\lat ->
      if (delta < 0) then
        if (lat < 0) then
          lat - negate ( (zoomLevel - 1) * latDistanceFromMouse lat )
        else
          lat - (zoomLevel - 1) * latDistanceFromMouse lat
      else
        if (lat < 0) then
          lat + negate ( (zoomLevel - 1) * latDistanceFromMouse lat )
        else
          lat + (zoomLevel - 1) * latDistanceFromMouse lat
    )
    shiftLng = (\lng ->
      if (delta < 0) then
        lng - (zoomLevel - 1) * lngDistanceFromMouse lng
      else
        lng + (zoomLevel - 1) * lngDistanceFromMouse lng
    )


    zoomedMap = model.map
      |> List.map (\polygonPoints ->
           List.map (\(lat, lng) -> ( shiftLat lat , shiftLng lng)
         ) polygonPoints )
  in
    { model | map = zoomedMap }



generateSvg : Model -> Html Msg
generateSvg model =
  let
    polygonStrings = printPolygonStrings (movePolygonPosition model)
    svgPolys = polygonStrings
      |> List.map (\p -> polygon [ fill "#F0AD00", points p] [])
      |> Svg.svg []
  in
    Svg.svg
      [ version "1.1" , x "0" , y "0" , viewBox "0 0 180 180" ] [ svgPolys ]


fetchPerform : String -> Cmd Msg
fetchPerform url =
  Http.send FetchGeoJson <| Http.get url decoder

wheelEventDecoder : Json.Decoder WheelEvent
wheelEventDecoder =
  Json.map3 WheelEvent
    (at ["deltaX"] Json.float)
    (at ["deltaY"] Json.float)
    (at ["deltaZ"] Json.float)

movePolygonPosition : Model -> Model
movePolygonPosition model =
  let
    xShift = toFloat model.topLeft.lat/10
    yShift = toFloat model.topLeft.lng/10
    shiftedPolygons = model.map
      |> List.map (\points ->
         List.map (\( p1, p2 ) -> (p1 + xShift, p2+yShift)) points)
  in
    { model | map = shiftedPolygons}

normalizeZoom : Model -> Float -> Model
normalizeZoom model zoom =
  let
    scaledZoom = zoom/1000
    normalizedZoom =
      if (model.zoomLevel + scaledZoom < 1) then
        1
      else
        model.zoomLevel + scaledZoom
  in
    { model | zoomLevel = normalizedZoom }

isNegative : Float -> Bool
isNegative num =
  if (num < 0) then
    True
  else
    False

printPolygonStrings : Model -> List String
printPolygonStrings model =
  let
    polygonList = model.map
    stringifyPoint =
      (\(x, y) -> String.concat [ (toString x), ",", (toString y) ])
    spaceSeperate =
      (\polygon -> String.join " " polygon)
  in
    polygonList
      |> List.map (\point -> List.map stringifyPoint point)
      |> List.map spaceSeperate
