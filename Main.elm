module Main exposing (..)

import Html exposing (Html)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Svg exposing (Svg, svg)
import Svg.Attributes exposing (fill, x, y, rx, ry)
import Animation exposing (px)
import Animation.Messenger
import Zipper as Zipper exposing (..)
import Random exposing (Generator)
import Task
import Time
import Ease
import Window
import Item


{-
   TODO:
   - Add more words/Imgs
   - Add grace period between words
   - get secondary images to appear
   - make an item more likely to appear if it's the current word
   - make image unselectable: http://stackoverflow.com/a/12906840
-}
-- MODEL


type alias Model =
    { items : Zipper (Item.Model Msg)
    , treadmill : List (Item.Model Msg)
    , seed : Random.Seed
    , windowSize : Window.Size
    , points : Int
    , notice : String
    , running : Bool
    }


init : ( Model, Cmd Msg )
init =
    ( { items = Item.initItems
      , treadmill = []
      , seed = Random.initialSeed 0
      , windowSize = { width = 500, height = 500 }
      , points = 0
      , notice = ""
      , running = True
      }
    , Task.perform Resize (Window.size)
    )



-- UPDATE


type Msg
    = Animate Animation.Msg
    | Start
    | Stop
    | Done (Item.Img Msg)
    | Resize Window.Size
    | Tick Time.Time
    | NewWord Time.Time
    | ItemClicked (Item.Model Msg)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NewWord _ ->
            ( { model | items = Zipper.next model.items |> Maybe.withDefault (Zipper.first model.items) }, Cmd.none )

        Tick _ ->
            if model.running then
                let
                    ( newItem, newSeed ) =
                        Random.step (Item.randItem model.items) model.seed

                    newAnimatedItem =
                        Maybe.map (Item.startItemAnimation Done model.windowSize.width -100) newItem

                    newModel =
                        case newAnimatedItem of
                            Nothing ->
                                { model | seed = newSeed }

                            Just animatedItem ->
                                { model | seed = newSeed, treadmill = model.treadmill ++ [ animatedItem ] }
                in
                    ( newModel, Cmd.none )
            else
                ( model, Cmd.none )

        ItemClicked clickedItem ->
            let
                currentItem =
                    Zipper.current model.items

                ( notice, points ) =
                    if clickedItem.word == currentItem.word then
                        ( "Yes!", model.points + 10 )
                    else
                        ( "Try Again!", model.points )
            in
                ( { model | notice = notice, points = points }, Cmd.none )

        Resize newSize ->
            ( { model | windowSize = newSize }, Cmd.none )

        Start ->
            ( { model | running = True }, Cmd.none )

        Stop ->
            ( { model | running = False }, Cmd.none )

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



-- VIEW


view : Model -> Html Msg
view model =
    Html.div []
        ([ startButton
         , stopButton
         , points model
         , word model
         , treadmill model
         , notice model
         ]
            ++ (model.treadmill
                    |> List.map (Item.viewItem ItemClicked)
               )
        )


points : Model -> Html Msg
points model =
    Html.h4 [] [ Html.text <| toString model.points ]


notice : Model -> Html Msg
notice model =
    Html.h4 [] [ Html.text model.notice ]


word : Model -> Html Msg
word model =
    let
        currentWord =
            model.items
                |> Zipper.current
                |> .word
    in
        Html.h1 [ style [ ( "text-align", "center" ) ] ] [ Html.text currentWord ]


stopButton : Html Msg
stopButton =
    Html.button [ onClick Stop ] [ Html.text "Stop" ]


startButton : Html Msg
startButton =
    Html.button [ onClick Start ] [ Html.text "Start" ]


treadmill : Model -> Html Msg
treadmill model =
    svg
        [ style
            [ ( "position", "absolute" )
            , ( "top", "400px" )
            , ( "width", (toString model.windowSize.width) ++ "px" )
            , ( "left", "0px" )
            ]
        ]
        [ Svg.rect [ fill "black", x "0", y "0", rx "3", ry "3", Svg.Attributes.width "100%", Svg.Attributes.height "4" ] [] ]



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
