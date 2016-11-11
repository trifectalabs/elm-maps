module Main exposing (..)

import String exposing (append)
import Maybe exposing (andThen)
import Json.Encode
import Json.Decode as Json exposing ((:=))
import Task exposing (perform)
import Http
import Mouse
import Html.App as App
import Html exposing (Html, div, span, text)
import Html.Events exposing (onWithOptions, onMouseDown, onMouseUp)
import Html.Attributes exposing (style)
import Svg exposing (polygon)
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
  { map: GeoJson
  , mousePosition : (Int, Int)
  , prevPosition : (Int, Int)
  , dragging : Bool
  , topLeft : Coordinate
  , topRight : Coordinate
  , bottomLeft : Coordinate
  , bottomRight : Coordinate
  , zoomLevel : Int
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
    ( { map = ((Geometry (Point(0,0,[]))), Nothing )
      , mousePosition = (0, 0)
      , prevPosition = (0, 0)
      , dragging = False
      , topLeft = { lat = 0, lng = 0 }
      , topRight = { lat = 10, lng = 0 }
      , bottomLeft = { lat = 0, lng = 10 }
      , bottomRight = { lat = 10, lng = 10 }
      , zoomLevel = 0
      }
    , fetchPerform sampleUrl
    )


-- Update

type Msg
    = FetchSuccess GeoJson
    | FetchFail Http.Error
    | StartDrag
    | StopDrag
    | Wheel WheelEvent
    | Position Int Int


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
    FetchSuccess geoJson ->
      ({ model | map = geoJson }, Cmd.none)
    FetchFail err ->
      (model, Cmd.none)
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
            moveLat = fst model.mousePosition - fst model.prevPosition
            moveLng = snd model.prevPosition - snd model.mousePosition
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
      ({ model | zoomLevel = model.zoomLevel + round event.deltaY }, Cmd.none)


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
        [ style [ ("position", "absolute"), ("top", "0"), ("left", "200px") ] ]
        [ text (toString model.zoomLevel) ]
      ]

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
  Task.perform FetchFail FetchSuccess (Http.get decoder url)

wheelEventDecoder : Json.Decoder WheelEvent
wheelEventDecoder =
  Json.object3 WheelEvent
    ("deltaX" := Json.float)
    ("deltaY" := Json.float)
    ("deltaZ" := Json.float)

movePolygonPosition : Model -> List (List (Float, Float))
movePolygonPosition model =
  let
    xShift = toFloat model.topLeft.lat/10
    yShift = toFloat model.topLeft.lng/10
    geometry = model.map
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
    shiftedPolygons = parsePolygonCoordinates geometry
      |> List.map (\points ->
        List.map (\( p1, p2 ) -> (p1 + xShift, p2+yShift)) points)
  in
    shiftedPolygons

printPolygonStrings : List (List (Float, Float)) -> List String
printPolygonStrings polygonList =
  let
    stringifyPoint =
      (\(x, y) -> String.concat [ (toString x), ",", (toString y) ])
    spaceSeperate =
      (\polygon -> String.concat (List.intersperse " " polygon))
  in
    polygonList
      |> List.map (\point -> List.map stringifyPoint point)
      |> List.map spaceSeperate
