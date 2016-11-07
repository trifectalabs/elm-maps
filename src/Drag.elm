module Drag exposing (..)


import Html exposing (Html, div, span, text)
import Html.App
import Html.Attributes exposing (style)
import Html.Events exposing (onMouseDown, onMouseUp)
import Mouse


main : Program Never
main =
  Html.App.program
      { init = init
      , view = view
      , update = update
      , subscriptions = subscriptions
      }


subscriptions : Model -> Sub Msg
subscriptions model =
  Mouse.moves (\{x, y} -> Position x y)


type alias Model =
  { mousePosition : (Int, Int)
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


init : (Model, Cmd Msg)
init =
  let
    model =
      { mousePosition = (0, 0)
      , prevPosition = (0, 0)
      , dragging = False
      , topLeft = { lat = 0, lng = 0 }
      , topRight = { lat = 10, lng = 0 }
      , bottomLeft = { lat = 0, lng = 10 }
      , bottomRight = { lat = 10, lng = 10 }
      , zoomLevel = 0
      }
  in
    (model, Cmd.none)


type Msg
  = StartDrag
  | StopDrag
  | Position Int Int


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
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
                , lng = model.topLeft.lng + moveLng
                }
              , topRight =
                { lat = model.topRight.lat + moveLat
                , lng = model.topRight.lng + moveLng
                }
              , bottomLeft =
                { lat = model.bottomLeft.lat + moveLat
                , lng = model.bottomLeft.lng + moveLng
                }
              , bottomRight =
                { lat = model.bottomRight.lat + moveLat
                , lng = model.bottomRight.lng + moveLng
                }
              }
          in
            (newModel, Cmd.none)


view : Model -> Html Msg
view model =
  div
    [ onMouseDown StartDrag
    , onMouseUp StopDrag
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
      [ style [ ("position", "absolute"), ("bottom", "0"), ("right", "0") ] ]
      [ text (toString model.bottomRight) ]
    ]
