import Html exposing (Html, div, span, text, program, select, option, button)
import Html.Events exposing (onClick)
import Html.Attributes exposing (..)
import Array

-- MODEL

type alias Model =
    { piles : Array.Array Pile
    , brick : Int
    , count : Int
    , numberOfBricks : Int
    }

type alias Pile =
    List Int

initialModel = Model (Array.fromList [[1,2,3],[],[]]) 0 0 3

init : ( Model, Cmd Msg )
init =
    ( initialModel, Cmd.none )



-- MESSAGES

type Msg
    = Click Int
    | Difficulty Int


-- VIEW

view : Model -> Html Msg
view model =
    div [] [
        div []
            (List.map (\n -> button [ onClick (Difficulty n) ] [ text (toString n) ]) [3,4,5] )
        , div [ style [("width", "300px"), ("display", "flex"), ("justify-content", "center")]] [ div [ brickStyle model.brick ] [] ]
        , div [ pilesStyle ]
        [ span [ onClick (Click 0) ] [ pilefy (Maybe.withDefault [] (Array.get 0 model.piles)) ]
        , span [ onClick (Click 1) ] [ pilefy (Maybe.withDefault [] (Array.get 1 model.piles)) ]
        , span [ onClick (Click 2) ] [ pilefy (Maybe.withDefault [] (Array.get 2 model.piles)) ]
        ]
        , div [] [ text ((toString model.count) ++ "/" ++ (toString (2^model.numberOfBricks - 1)))]
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
    , ("border-radius", "4px")
    ]


-- UPDATE

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Click pileIndex ->
            case model.brick of
                0 ->
                    case Array.get pileIndex model.piles of
                        Just pile ->
                            case pile of
                                h::t ->
                                    ( { model | piles = Array.set pileIndex t model.piles, brick = h, count = model.count + 1 }, Cmd.none )
                                [] ->
                                    ( model, Cmd.none )
                        _ ->
                            (model, Cmd.none)
                value ->
                    case Array.get pileIndex model.piles of
                        Just pile ->
                            case pile of
                                h::t ->
                                    case value < h of
                                        True ->
                                            ( { model | piles = Array.set pileIndex (value::h::t) model.piles, brick = 0 }, Cmd.none)
                                        False ->
                                            ( model, Cmd.none )
                                [] ->
                                    ( { model | piles = Array.set pileIndex [value] model.piles, brick = 0 }, Cmd.none )
                        _ ->
                            (model, Cmd.none)
        Difficulty amount ->
            (  Model (Array.fromList [(List.indexedMap (\i _ -> i+1) (List.repeat amount 0)),[],[]]) 0 0 amount, Cmd.none )


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