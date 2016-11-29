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

baseUrl: String
baseUrl =
  "http://localhost:8080/osm"

initUrl : String
initUrl =
    String.concat [baseUrl, "/4/4/6.json"]


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
  , zoomLevel : Int
  }

type alias Coordinate =
  { lng : Int
  , lat : Int
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
      , topLeft = { lng = 0, lat = 0 }
      , topRight = { lng = 10, lat = 0 }
      , bottomLeft = { lng = 0, lat = 10 }
      , bottomRight = { lng = 10, lat = 10 }
      , zoomLevel = 0
      }
    , fetchPerform initUrl
    )


-- Update

type Msg
    = FetchGeoJson (Result Http.Error GeoJson)
    | NewTile (Result Http.Error GeoJson)
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
      ({ model | map = [parseToTile geoJson] }, Cmd.none)
    NewTile (Err _) ->
      (model, Cmd.none)
    NewTile (Ok geoJson) ->
      ({ model | map = [parseToTile geoJson] }, Cmd.none)
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
            newTiles = fetchViewableTiles model
            moveLng = first model.prevPosition - first model.mousePosition
            moveLat = second model.mousePosition - second model.prevPosition
            newModel =
              { model
              | mousePosition = (x, y)
              , prevPosition = model.mousePosition
              , topLeft =
                { lng = model.topLeft.lng - moveLng
                , lat = model.topLeft.lat + moveLat
                }
              , topRight =
                { lng = model.topRight.lng - moveLng
                , lat = model.topRight.lat + moveLat
                }
              , bottomLeft =
                { lng = model.bottomLeft.lng - moveLng
                , lat = model.bottomLeft.lat + moveLat
                }
              , bottomRight =
                { lng = model.bottomRight.lng - moveLng
                , lat = model.bottomRight.lat + moveLat
                }
              }
          in
            (newModel, Cmd.none)
    Wheel event ->
      let
        newTiles = fetchViewableTiles model
      in
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
    xShift = toFloat model.topLeft.lng/10
    yShift = toFloat model.topLeft.lat/10
    tiles = model.map
      |> List.map (\tile ->
           List.map (\points ->
             List.map (\( p1, p2 ) -> (p1 + xShift, p2+yShift)) points
           ) tile
         )
   in
    { model | map = tiles}

printPolygonStrings : Model -> List String
printPolygonStrings model =
  let
    tiles = model.map
    stringifyPoint =
      (\(x, y) -> String.concat [ (toString x), ",", (toString y) ])
    spaceSeperate =
      (\polygon -> String.join " " polygon)
  in
    tiles
      |> List.map (\polygons ->
        List.map (\point -> List.map stringifyPoint point) polygons
          |> List.map spaceSeperate
      )
      |> List.concat

fetchTile : (Float, Float) -> Cmd Msg
fetchTile (lng, lat) =
  let
    url = ""
  in
    Http.send NewTile <| (Http.get url decoder)

fetchViewableTiles : Model -> List Tile
fetchViewableTiles model =
  let
    zoomLevel = model.zoomLevel
    max = calculateMaxRowCol zoomLevel
    path = String.concat [toString zoomLevel, toString max]
  in
    if (False) then
      Debug.log "would fetch"
      model.map
    else
      Debug.log path
      model.map

calculateMaxRowCol : Int -> Int
calculateMaxRowCol zoomLevel =
  if (zoomLevel == 0) then 0
  else (calculateMaxRowCol <| zoomLevel - 1) * 2 + 1
