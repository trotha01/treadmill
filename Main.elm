module Main exposing (..)

import Html exposing (Html)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Item
import Random exposing (Generator)
import Task
import Time
import TouchEvents as Touch exposing (..)
import Treadmill
import Tuple
import Window
import Zipper as Zipper exposing (..)


{-
   TODO:
   - Add more words/Imgs
   - get secondary images to appear
   - make image unselectable: http://stackoverflow.com/a/12906840
-}
{-
   sound from: http://www.pacdv.com/sounds/interface_sounds-2.html
-}
-- MODEL


type alias Model =
    { items : Zipper (Item.Model Msg)
    , treadmill : Treadmill.Belt Msg
    , seed : Random.Seed
    , windowSize : Window.Size
    , points : Int
    , notice : String
    , level : Int
    , game : Game
    }


type Game
    = SplashScreen
    | ClassicTreadmill
    | MakeACake Status


type Status
    = Starting
    | Running


init : ( Model, Cmd Msg )
init =
    ( { items = Item.initItems
      , treadmill = Treadmill.init
      , seed = Random.initialSeed 0
      , windowSize = { width = 500, height = 500 }
      , points = 0
      , notice = ""
      , level = 1
      , game = SplashScreen
      }
    , Task.perform Resize (Window.size)
    )



-- UPDATE


type Msg
    = TreadmillMsg Treadmill.Msg
    | Start
    | Done Int (Item.Img Msg)
    | Resize Window.Size
    | Tick Time.Time
    | NewWord Time.Time
    | ItemClicked Int (Item.Model Msg)
    | ItemTouched Int (Item.Model Msg) Touch


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Resize newSize ->
            ( { model | windowSize = newSize }, Cmd.none )

        _ ->
            case model.game of
                SplashScreen ->
                    updateSplashScreen msg model

                MakeACake Starting ->
                    startMakeACake msg model

                MakeACake Running ->
                    updateMakeACake msg model

                ClassicTreadmill ->
                    updateClassicGame msg model


updateSplashScreen : Msg -> Model -> ( Model, Cmd Msg )
updateSplashScreen msg model =
    case msg of
        Start ->
            ( { model | game = ClassicTreadmill }, Cmd.none )

        TreadmillMsg msg ->
            let
                ( newTreadmill, cmd ) =
                    Treadmill.update msg model.treadmill
            in
                ( { model | treadmill = newTreadmill }, cmd )

        _ ->
            ( model, Cmd.none )


startMakeACake : Msg -> Model -> ( Model, Cmd Msg )
startMakeACake msg model =
    ( addBowl model, Cmd.none )


updateMakeACake : Msg -> Model -> ( Model, Cmd Msg )
updateMakeACake msg model =
    case msg of
        TreadmillMsg msg ->
            let
                ( newTreadmill, cmd ) =
                    Treadmill.update msg model.treadmill
            in
                ( { model | treadmill = newTreadmill }, cmd )

        Resize newSize ->
            ( { model | windowSize = newSize }, Cmd.none )

        Done id img ->
            ( { model | treadmill = Treadmill.removeItem id model.treadmill }, Cmd.none )

        _ ->
            ( model, Cmd.none )


updateClassicGame : Msg -> Model -> ( Model, Cmd Msg )
updateClassicGame msg model =
    case msg of
        Start ->
            ( model, Cmd.none )

        NewWord _ ->
            ( nextWord model, Cmd.none )

        Tick _ ->
            ( addItem model, Cmd.none )

        ItemTouched id clickedItem _ ->
            updateClassicGame (ItemClicked id clickedItem) model

        ItemClicked id clickedItem ->
            let
                currentItem =
                    Zipper.current model.items

                ( correct, notice, points ) =
                    if clickedItem.word == currentItem.word then
                        ( True, "Yes!", model.points + 10 )
                    else
                        ( False, "Try Again!", model.points )

                ( game, level ) =
                    if correct && points /= 0 && (points % 50 == 0) then
                        -- Level Up
                        ( SplashScreen, model.level + 1 )
                    else
                        ( ClassicTreadmill, model.level )

                treadmill =
                    if correct then
                        Treadmill.removeItem id model.treadmill
                    else
                        model.treadmill
            in
                ( { model
                    | notice = notice
                    , points = points
                    , game = game
                    , level = level
                    , treadmill = treadmill
                  }
                , Cmd.none
                )

        TreadmillMsg msg ->
            let
                ( newTreadmill, cmd ) =
                    Treadmill.update msg model.treadmill
            in
                ( { model | treadmill = newTreadmill }, cmd )

        Done id img ->
            ( { model | treadmill = Treadmill.removeItem id model.treadmill }, Cmd.none )

        _ ->
            ( model, Cmd.none )


nextWord : Model -> Model
nextWord model =
    { model
        | items =
            Zipper.next model.items
                |> Maybe.withDefault (Zipper.first model.items)
    }


addBowl : Model -> Model
addBowl model =
    { model | game = MakeACake Running, treadmill = Treadmill.addItem model.windowSize.width Done model.treadmill Item.bowl }


addItem : Model -> Model
addItem model =
    let
        ( newItem, newSeed ) =
            Random.step (Item.randItem model.items) model.seed

        newBelt =
            Maybe.map (Treadmill.addItem model.windowSize.width Done model.treadmill) newItem
    in
        case newBelt of
            Nothing ->
                { model | seed = newSeed }

            Just belt ->
                { model | seed = newSeed, treadmill = belt }



-- VIEW


view : Model -> Html Msg
view model =
    case model.game of
        SplashScreen ->
            splashScreenView model

        MakeACake _ ->
            Html.div
                []
                [ Html.span
                    [ style [ ( "text-align", "right" ), ( "padding", "50px" ) ] ]
                    [ pointsView model ]
                , Treadmill.view model.windowSize.width ItemClicked ItemTouched model.treadmill
                ]

        ClassicTreadmill ->
            Html.div
                []
                [ Html.span
                    [ style [ ( "text-align", "right" ), ( "padding", "50px" ) ] ]
                    [ pointsView model ]
                , word model
                , notice model
                , Treadmill.view model.windowSize.width ItemClicked ItemTouched model.treadmill
                ]


splashScreenView : Model -> Html Msg
splashScreenView model =
    let
        points =
            if model.level == 1 then
                []
            else
                [ Html.div [ style [ ( "margin", "10px" ) ] ] [ pointsView model ] ]
    in
        Html.div
            [ style
                [ ( "text-align", "center" )
                , ( "background-color", "lightblue" )
                , ( "height", (toString model.windowSize.height) ++ "px" )
                , ( "padding", "50px" )
                , ( "border", "6px solid black" )
                ]
            ]
            ([ Html.h1 [] [ Html.text ("Level " ++ (toString model.level)) ]
             ]
                ++ points
                ++ [ startButton ]
            )


pointsView : Model -> Html Msg
pointsView model =
    Html.span [] [ Html.text <| "Points: " ++ (toString model.points) ]


notice : Model -> Html Msg
notice model =
    Html.h4 [ style [ ( "text-align", "center" ) ] ] [ Html.text model.notice ]


word : Model -> Html Msg
word model =
    let
        currentWord =
            model.items
                |> Zipper.current
                |> .word
    in
        Html.h1 [ style [ ( "text-align", "center" ) ] ] [ Html.text currentWord ]


buttonStyle : Html.Attribute Msg
buttonStyle =
    style
        [ ( "background-color", "#041d25" )
        , ( "border", "none" )
        , ( "color", "white" )
        , ( "padding", "15px 32px" )
        , ( "text-align", "center" )
        , ( "text-decoration", "none" )
        , ( "display", "inline-block" )
        , ( "font-size", "16px" )
        ]


startButton : Html Msg
startButton =
    Html.div [ onClick Start ]
        [ Html.button [ buttonStyle, onClick Start ] [ Html.text "Start" ] ]



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    case model.game of
        SplashScreen ->
            Window.resizes Resize

        MakeACake _ ->
            Sub.batch
                [ Treadmill.subscription TreadmillMsg model.treadmill
                , Window.resizes Resize
                ]

        _ ->
            Sub.batch
                [ Treadmill.subscription TreadmillMsg model.treadmill
                , Time.every Time.second Tick
                , Time.every (Time.second * 5) NewWord
                , Window.resizes Resize
                ]



--MAIN


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
