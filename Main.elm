module Main exposing (..)

import AnimationFrame
import Bowl
import Html exposing (Html)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Item
import Json.Decode as Decode
import Keyboard
import Math.Vector2 as Vector2 exposing (Vec2, getX, getY, vec2)
import Mouse
import Random exposing (Generator)
import Task
import Time
import TouchEvents as Touch exposing (..)
import Treadmill
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
   images from: https://pixabay.com/
-}
-- MODEL


type alias Model =
    { items : Zipper Item.Model
    , treadmill : Treadmill.Belt
    , seed : Random.Seed
    , windowSize : Window.Size
    , points : Int
    , notice : String
    , level : Int
    , game : Game
    , bowl : Bowl.Model
    , cakeOptions : Zipper CakeOption
    , win : Bool
    , pause : Bool
    }


type alias CakeOption =
    { item : Item.Model
    , id : Int
    , position : Vec2
    , clicked : Bool
    , dragging : Bool
    , inBowl : Bool
    , cakeIngredient : Bool
    }


type Game
    = SplashScreen
    | ClassicTreadmill
    | MakeACake


init : ( Model, Cmd Msg )
init =
    ( { items = Item.initItems
      , treadmill = Treadmill.init
      , seed = Random.initialSeed 0
      , windowSize = { width = 500, height = 500 }
      , points = 0
      , notice = ""
      , level = 1
      , bowl = Bowl.init 100
      , game = SplashScreen
      , win = False
      , pause = False
      , cakeOptions =
            Item.initItems
                |> Zipper.indexedMap initCakeOption
      }
    , Task.perform Resize Window.size
    )


{-| cakeOptionWidth and cakeOptionMargin are in rem
-}
( cakeOptionWidth, cakeOptionMargin ) =
    ( 7, 2 )


initCakeOption : Int -> Item.Model -> CakeOption
initCakeOption i item =
    { item = item
    , id = i
    , position = vec2 (i * (cakeOptionWidth + cakeOptionMargin) + cakeOptionWidth |> toFloat) 5
    , clicked = False
    , dragging = False
    , inBowl = False
    , cakeIngredient =
        (item.word == "leche")
            || (item.word == "huevo")
            || (item.word == "harina")
            || (item.word == "azúcar")
    }



-- UPDATE


type Msg
    = TreadmillMsg Treadmill.Msg
    | Start Game
    | Done Int Item.Img
    | Resize Window.Size
    | InitialWindowSize Window.Size
    | Tick Time.Time
    | NewWord Time.Time
    | ItemClicked Int Item.Model
    | ItemTouched Int Item.Model Touch
    | DragStart String Mouse.Position
    | DragAt Mouse.Position
    | DragEnd Mouse.Position
    | MoveBowl Time.Time
    | KeyPress Keyboard.KeyCode


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        InitialWindowSize size ->
            ( { model
                | windowSize = size
                , bowl = Bowl.init (toFloat size.width / 10 + 4)
              }
            , Cmd.none
            )

        Resize newSize ->
            ( { model | windowSize = newSize }, Cmd.none )

        _ ->
            case model.game of
                SplashScreen ->
                    updateSplashScreen msg model

                MakeACake ->
                    updateMakeACake msg model

                ClassicTreadmill ->
                    updateClassicGame msg model


updateSplashScreen : Msg -> Model -> ( Model, Cmd Msg )
updateSplashScreen msg model =
    case msg of
        Start game ->
            ( { model
                | game = game
                , treadmill = Treadmill.init
              }
            , Cmd.none
            )

        TreadmillMsg msg ->
            let
                ( newTreadmill, cmd ) =
                    Treadmill.update msg model.treadmill
            in
            ( { model | treadmill = newTreadmill }, cmd )

        _ ->
            ( model, Cmd.none )


updateMakeACake : Msg -> Model -> ( Model, Cmd Msg )
updateMakeACake msg model =
    case msg of
        KeyPress _ ->
            ( { model | pause = not model.pause }, Cmd.none )

        MoveBowl time ->
            let
                inBowl opt =
                    { opt | inBowl = Bowl.contains opt.position model.bowl }

                cakeOptions =
                    Zipper.mapCurrent inBowl model.cakeOptions
            in
            ( { model | bowl = Bowl.step time model.bowl, cakeOptions = cakeOptions }, Cmd.none )

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

        DragStart word pos ->
            let
                cakeOptions =
                    Maybe.withDefault model.cakeOptions
                        (Zipper.first model.cakeOptions
                            |> Zipper.find (\option -> option.item.word == word)
                        )
                        |> Zipper.mapCurrent (\option -> { option | dragging = True })
            in
            ( { model | cakeOptions = cakeOptions }, Cmd.none )

        DragAt pos ->
            let
                remPos =
                    vec2 (toFloat pos.x / 10) (toFloat pos.y / 10)

                dragItem item =
                    if item.dragging then
                        { item | position = remPos }
                    else
                        item

                inBowl item =
                    { item | inBowl = Bowl.contains item.position model.bowl }

                cakeOptions =
                    Zipper.mapCurrent dragItem model.cakeOptions

                cakeOptions2 =
                    Zipper.mapCurrent inBowl cakeOptions
            in
            ( { model | cakeOptions = cakeOptions2 }, Cmd.none )

        DragEnd _ ->
            let
                bowl =
                    model.bowl

                currentOption =
                    Zipper.current model.cakeOptions

                ( newBowl, newOptions ) =
                    if currentOption.cakeIngredient && currentOption.inBowl then
                        ( { bowl | items = currentOption.item :: bowl.items }
                        , Zipper.delete model.cakeOptions |> Maybe.withDefault model.cakeOptions
                        )
                    else
                        ( bowl
                        , Zipper.mapCurrent (\opt -> initCakeOption opt.id opt.item) model.cakeOptions
                        )

                win =
                    Bowl.full newBowl
            in
            ( { model | cakeOptions = newOptions, bowl = newBowl, win = win }, Cmd.none )

        _ ->
            ( model, Cmd.none )


updateClassicGame : Msg -> Model -> ( Model, Cmd Msg )
updateClassicGame msg model =
    case msg of
        Start _ ->
            ( model, Cmd.none )

        KeyPress _ ->
            ( { model | pause = not model.pause }, Cmd.none )

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


addItem : Model -> Model
addItem model =
    let
        ( newItem, newSeed ) =
            Random.step (Item.randItem model.windowSize model.items) model.seed

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

        MakeACake ->
            Html.div
                []
                ([ viewGameInfo model
                 , cakeWords model
                 , Bowl.view model.bowl
                 , Treadmill.view model.windowSize.width ItemClicked ItemTouched model.treadmill
                 ]
                    ++ winPopup model
                )

        ClassicTreadmill ->
            Html.div
                []
                [ Html.span
                    [ style [ ( "text-align", "right" ), ( "padding", "50px" ) ] ]
                    [ viewPoints model ]
                , word model
                , notice model
                , Treadmill.view model.windowSize.width ItemClicked ItemTouched model.treadmill
                ]


viewGameInfo : Model -> Html Msg
viewGameInfo model =
    Html.div [ class "gameinfo" ]
        [ viewPoints model
        , viewDistance model
        ]


viewDistance : Model -> Html Msg
viewDistance model =
    Html.span [ class "gameinfo-item distance" ] [ Html.text <| Bowl.distance model.bowl ++ " feet" ]


viewPoints : Model -> Html Msg
viewPoints model =
    Html.span [ class "gameinfo-item points" ] [ Html.text <| "Points: " ++ toString model.points ]


winPopup : Model -> List (Html Msg)
winPopup model =
    if model.win then
        [ Html.div
            [ style
                [ ( "display", "flex" )
                , ( "justify-content", "center" )
                , ( "align-items", "center" )
                ]
            ]
            [ Html.div
                [ class "win-frame"
                ]
                [ Html.span [] [ Html.text <| "CONGRATULATIONS!!!" ]
                , Html.span [] [ Html.text <| "YOU MADE A CAKE IN " ++ Bowl.distance model.bowl ++ " FEET!!!" ]
                ]
            ]
        ]
    else
        []


onMouseDown : String -> Html.Attribute Msg
onMouseDown word =
    on "mousedown" (Decode.map (DragStart word) Mouse.position)


positionCakeWord : Vec2 -> Html.Attribute Msg
positionCakeWord pos =
    style
        [ ( "position", "absolute" )
        , ( "left", (getX pos |> toString) ++ "rem" )
        , ( "top", (getY pos |> toString) ++ "rem" )
        , ( "transform", "translate(-50%, -50%)" )
        ]


cakeImgStyle : Vec2 -> Html.Attribute Msg
cakeImgStyle pos =
    style
        [ ( "position", "absolute" )
        , ( "left", (getX pos |> toString) ++ "rem" )
        , ( "top", (getY pos |> toString) ++ "rem" )
        ]


cakeWords : Model -> Html Msg
cakeWords model =
    Html.div [ class "bowl-options" ]
        (Zipper.toList model.cakeOptions
            |> List.map cakeWord
        )


cakeWord : CakeOption -> Html Msg
cakeWord cakeOption =
    if cakeOption.inBowl then
        Html.div
            [ positionCakeWord cakeOption.position
            , onMouseDown cakeOption.item.word
            ]
            [ Html.img
                [ class "bowl-item"
                , src (cakeOption.item.imgs |> Zipper.current |> .src)
                ]
                []
            ]
    else
        Html.div
            [ positionCakeWord cakeOption.position
            , onMouseDown cakeOption.item.word
            , class "button bowl-item"
            ]
            [ Html.span [] [ Html.text cakeOption.item.word ] ]


splashScreenView : Model -> Html Msg
splashScreenView model =
    let
        points =
            if model.level == 1 then
                []
            else
                [ Html.div [ style [ ( "margin", "10px" ) ] ] [ viewPoints model ] ]
    in
    Html.div
        [ class "splashscreen-wrap" ]
        ([ Html.h1 [] [ Html.text ("Level " ++ toString model.level) ]
         ]
            ++ points
            ++ [ startCakeButton, startTreadmillButton ]
        )


notice : Model -> Html Msg
notice model =
    Html.h4 [ class "treadmill-notice" ] [ Html.text model.notice ]


word : Model -> Html Msg
word model =
    let
        currentWord =
            model.items
                |> Zipper.current
                |> .word
    in
    Html.h1 [ style [ ( "text-align", "center" ) ] ] [ Html.text currentWord ]


startCakeButton : Html Msg
startCakeButton =
    Html.div [ onClick (Start MakeACake) ]
        [ Html.button [ class "button", onClick (Start MakeACake) ] [ Html.text "Make A Cake" ] ]


startTreadmillButton : Html Msg
startTreadmillButton =
    Html.div [ onClick (Start ClassicTreadmill) ]
        [ Html.button [ class "button", onClick (Start ClassicTreadmill) ] [ Html.text "Treadmill" ] ]



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    case model.game of
        SplashScreen ->
            Window.resizes Resize

        MakeACake ->
            if model.win then
                Window.resizes Resize
            else if model.pause then
                Sub.batch
                    [ Window.resizes Resize
                    , Keyboard.presses KeyPress
                    ]
            else if Bowl.done model.bowl then
                -- stop moving the bowl if it has everything it needs
                Sub.batch
                    [ Window.resizes Resize
                    , Mouse.moves DragAt
                    , Mouse.ups DragEnd
                    , Keyboard.presses KeyPress
                    ]
            else
                Sub.batch
                    [ Window.resizes Resize
                    , Mouse.moves DragAt
                    , Mouse.ups DragEnd
                    , AnimationFrame.diffs MoveBowl
                    , Keyboard.presses KeyPress
                    ]

        _ ->
            if model.pause then
                Sub.batch
                    [ Window.resizes Resize
                    , Keyboard.presses KeyPress
                    ]
            else
                Sub.batch
                    [ Treadmill.subscription TreadmillMsg model.treadmill
                    , Time.every Time.second Tick
                    , Time.every (Time.second * 5) NewWord
                    , Keyboard.presses KeyPress
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
