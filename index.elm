-- Read more about this program in the official Elm guide:
-- https://guide.elm-lang.org/architecture/user_input/buttons.html


--module Main exposing (..)

import Html exposing (Html, div, span, text, program)
import Html.Events exposing (onClick)
import Mouse
import Keyboard
import Html.Attributes exposing (..)


-- MODEL


type alias Model =
    { first : Pile, second : Pile, third : Pile, brick : Int }

type alias Pile =
    List Int


init : ( Model, Cmd Msg )
init =
    ( (Model [1,2,3,4] [] [] 0), Cmd.none )



-- MESSAGES


type Msg
    = Click Int



-- VIEW


view : Model -> Html Msg
view model =
    div [] [
        div [ style [("width", "300px"), ("display", "flex"), ("justify-content", "center")]] [ div [ brickStyle model.brick ] [] ]
        , div [ pilesStyle ]
        [ span [ onClick (Click 1) ] [ pilefy model.first ]
        , span [ onClick (Click 2) ] [ pilefy model.second ]
        , span [ onClick (Click 3) ] [ pilefy model.third ]
        ]
    ]


pilefy : Pile -> Html Msg
pilefy pile =
    div [ pileStyle ] (List.map (\b -> div [ outerBrickStyle ] [ div [ brickStyle b ] [] ]) (List.reverse pile))

pilesStyle =
    style
        [ ("display", "flex")]

pileStyle =
    style
        [ ("width", "100px")
        , ("height", "100px")
        , ("border", "1px solid black")
        , ("transform", "scaleY(-1)")
        ]

outerBrickStyle =
    style
        [ ("display", "flex")
        , ("justify-content", "center")
        ]

brickStyle w =
  style
    [ ("height", "9px")
    , ("margin-bottom", "1px")
    , ("background-color", "brown")
    , ("width", (toString (w * 20)) ++ "px")
    ]

-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Click pile ->
            case model.brick of
                0 ->
                    case pile of
                        1 ->
                            case model.first of
                                h::t -> 
                                    ( { model | first = t, brick = h }, Cmd.none )
                                [] ->
                                    ( model, Cmd.none )
                        2 ->
                            case model.second of
                                h::t -> 
                                    ( { model | second = t, brick = h }, Cmd.none )
                                [] ->
                                    ( model, Cmd.none )
                        3 ->
                            case model.third of
                                h::t -> 
                                    ( { model | third = t, brick = h }, Cmd.none )
                                [] ->
                                    ( model, Cmd.none )
                        _ ->
                            (model, Cmd.none)
                x ->
                    case pile of
                        1 ->
                            case model.first of
                                h::t ->
                                    case x < h of
                                        True ->
                                            ( { model | first = x::model.first, brick = 0}, Cmd.none)
                                        False ->
                                            ( model, Cmd.none )
                                [] ->
                                    ( { model | first = [x], brick = 0 }, Cmd.none )
                        2 ->
                            case model.second of
                                h::t ->
                                    case x < h of
                                        True ->
                                            ( { model | second = x::model.second, brick = 0}, Cmd.none)
                                        False ->
                                            ( model, Cmd.none )
                                [] ->
                                    ( { model | second = [x], brick = 0 }, Cmd.none )
                        3 ->
                            case model.third of
                                h::t ->
                                    case x < h of
                                        True ->
                                            ( { model | third = x::model.third, brick = 0}, Cmd.none)
                                        False ->
                                            ( model, Cmd.none )
                                [] ->
                                    ( { model | third = [x], brick = 0 }, Cmd.none )
                        _ ->
                            ( model, Cmd.none)



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- MAIN


main : Program Never Model Msg
main =
    program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }