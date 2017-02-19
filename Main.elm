module Main exposing (..)

import Animation exposing (px)
import Animation.Messenger
import Ease
import Html exposing (Html)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Item
import Random exposing (Generator)
import Svg exposing (Svg, svg)
import Svg.Attributes exposing (fill, x, y, rx, ry)
import Task
import Time
import TouchEvents as Touch exposing (..)
import Window
import Zipper as Zipper exposing (..)


{-
   TODO:
   - Add levels
   - Add more words/Imgs
   - get secondary images to appear
   - make image unselectable: http://stackoverflow.com/a/12906840
   - Delete item when clicked
-}
-- MODEL


type alias Model =
    { items : Zipper (Item.Model Msg)
    , treadmill : List (Item.Model Msg)
    , seed : Random.Seed
    , windowSize : Window.Size
    , points : Int
    , notice : String
    , level : Int
    , splashScreen : Bool
    }


init : ( Model, Cmd Msg )
init =
    ( { items = Item.initItems
      , treadmill = []
      , seed = Random.initialSeed 0
      , windowSize = { width = 500, height = 500 }
      , points = 0
      , notice = ""
      , level = 1
      , splashScreen = True
      }
    , Task.perform Resize (Window.size)
    )



-- UPDATE


type Msg
    = Animate Animation.Msg
    | Start
    | Done (Item.Img Msg)
    | Resize Window.Size
    | Tick Time.Time
    | NewWord Time.Time
    | ItemClicked (Item.Model Msg)
    | ItemTouched (Item.Model Msg) Touch


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    if model.splashScreen then
        case msg of
            Start ->
                ( { model | splashScreen = False }, Cmd.none )

            Resize newSize ->
                ( { model | windowSize = Debug.log "new size" newSize }, Cmd.none )

            Animate animMsg ->
                let
                    itemCmds =
                        List.map (Item.updateItemAnimation animMsg) model.treadmill

                    ( items, cmds ) =
                        List.unzip itemCmds
                in
                    ( { model | treadmill = items }, Cmd.batch cmds )

            _ ->
                ( model, Cmd.none )
    else
        case msg of
            Start ->
                ( model, Cmd.none )

            NewWord _ ->
                ( nextWord model, Cmd.none )

            Tick _ ->
                ( addItem model, Cmd.none )

            ItemTouched clickedItem _ ->
                update (ItemClicked clickedItem) model

            ItemClicked clickedItem ->
                let
                    currentItem =
                        Zipper.current model.items

                    ( correct, notice, points ) =
                        if clickedItem.word == currentItem.word then
                            ( True, "Yes!", model.points + 10 )
                        else
                            ( False, "Try Again!", model.points )

                    ( splashScreen, level ) =
                        if correct && points /= 0 && (points % 50 == 0) then
                            -- Level Up
                            ( True, model.level + 1 )
                        else
                            ( False, model.level )
                in
                    ( { model
                        | notice = notice
                        , points = points
                        , splashScreen = splashScreen
                        , level = level
                      }
                    , Cmd.none
                    )

            Resize newSize ->
                ( { model | windowSize = Debug.log "new size" newSize }, Cmd.none )

            Animate animMsg ->
                let
                    itemCmds =
                        List.map (Item.updateItemAnimation animMsg) model.treadmill

                    ( items, cmds ) =
                        List.unzip itemCmds
                in
                    ( { model | treadmill = items }, Cmd.batch cmds )

            Done img ->
                let
                    newTreadmill =
                        case model.treadmill of
                            item :: items ->
                                items

                            items ->
                                items

                    _ =
                        Debug.log "done" img
                in
                    ( { model | treadmill = newTreadmill }, Cmd.none )


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
            Random.step (Item.randItem model.items) model.seed

        newAnimatedItem =
            Maybe.map (Item.startItemAnimation Done model.windowSize.width -100) newItem
    in
        case newAnimatedItem of
            Nothing ->
                { model | seed = newSeed }

            Just animatedItem ->
                { model | seed = newSeed, treadmill = model.treadmill ++ [ animatedItem ] }



-- VIEW


view : Model -> Html Msg
view model =
    if model.splashScreen then
        splashScreenView model
    else
        Html.div
            []
            [ Html.span
                [ style [ ( "text-align", "right" ), ( "padding", "50px" ) ] ]
                [ pointsView model ]
            , word model
            , notice model
            , treadmill model
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
            , onClick Start
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



{-
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

-}


startButton : Html Msg
startButton =
    Html.div []
        [ Html.text "Click Anywhere to Start" ]


treadmill : Model -> Html Msg
treadmill model =
    let
        items =
            (model.treadmill
                |> List.map (Item.viewItem ItemClicked ItemTouched)
            )

        belt =
            svg
                [ style
                    [ ( "position", "absolute" )
                    , ( "top", "100px" )
                    , ( "width", (toString model.windowSize.width) ++ "px" )
                    , ( "left", "0px" )
                    ]
                ]
                [ Svg.rect [ fill "black", x "0", y "0", rx "3", ry "3", Svg.Attributes.width "100%", Svg.Attributes.height "4" ] [] ]
    in
        Html.div
            [ style
                [ ( "position", "absolute" )
                , ( "overflow", "hidden" )
                , ( "top", "300px" )
                , ( "height", "110px" )
                , ( "width", (toString model.windowSize.width) ++ "px" )
                ]
            ]
            (belt :: items)



-- SUBSCRIPTIONS


{-| TODO: move these to Item module?
-}
imgStyles : Zipper (Item.Img Msg) -> List (Animation.Messenger.State Msg)
imgStyles imgs =
    imgs
        |> Zipper.map .style
        |> Zipper.toList


itemStyles : List (Item.Model Msg) -> List (Animation.Messenger.State Msg)
itemStyles items =
    items
        |> List.map (.imgs >> imgStyles)
        |> List.concat


subscriptions : Model -> Sub Msg
subscriptions model =
    if model.splashScreen then
        Window.resizes Resize
    else
        Sub.batch
            [ Animation.subscription Animate
                (itemStyles model.treadmill)
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
