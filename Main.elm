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


{-
   TODO:
   - treadmill matches window size
   - Add word of imgs to click on
   - Change imgs into buttons to click on
   - Check if img matches word
-}
-- MODEL


type alias Model =
    { items : Zipper Item
    , treadmill : List Item
    , seed : Random.Seed
    , windowSize : Window.Size
    , points : Int
    , notice : String
    , running : Bool
    }


type alias Item =
    { id : ID
    , word : String
    , imgs : Zipper Img
    }


type alias Img =
    { id : ID
    , src : String
    , style : Animation.Messenger.State Msg
    }


type alias ID =
    Int


init : ( Model, Cmd Msg )
init =
    ( { items = initItems
      , treadmill = []
      , seed = Random.initialSeed 0
      , windowSize = { width = 500, height = 500 }
      , points = 0
      , notice = ""
      , running = True
      }
    , Task.perform Resize (Window.size)
    )


initItems : Zipper Item
initItems =
    Zipper.singleton (initItem 0 "leche" "imgs/milk.jpg")
        |> Zipper.appendItem (initItem 1 "café" "imgs/coffee.png")


initItem : ID -> String -> String -> Item
initItem id word imgSrc =
    { id = id
    , word = word
    , imgs =
        Zipper.singleton (initImg 0 imgSrc)
            |> Zipper.appendList
                [ (initImg 1 imgSrc)
                , (initImg 2 imgSrc)
                ]
    }


initImg : ID -> String -> Img
initImg id imgSrc =
    { id = id
    , src = imgSrc
    , style =
        Animation.style
            [ Animation.left (px -500) ]
    }


randItem : Zipper Item -> Generator (Maybe Item)
randItem items =
    let
        itemList =
            Zipper.toList items

        len =
            List.length itemList

        i =
            Random.int 0 (len - 1)

        item =
            Random.map (listAt itemList) i
    in
        item


{-|
  listAt returns the item in the list at position n
-}
listAt : List a -> Int -> Maybe a
listAt items n =
    List.drop n items |> List.head



-- UPDATE


type Msg
    = Animate Animation.Msg
    | Start
    | Stop
    | Done Int
    | Resize Window.Size
    | Tick Time.Time
    | NewWord Time.Time
    | ItemClicked Item


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NewWord _ ->
            ( { model | items = Zipper.next model.items |> Maybe.withDefault (Zipper.first model.items) }, Cmd.none )

        Tick _ ->
            if model.running then
                let
                    ( newItem, newSeed ) =
                        Random.step (randItem model.items) model.seed

                    newAnimatedItem =
                        Maybe.map (startItemAnimation model.windowSize.width -100) newItem

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
                    if clickedItem.id == currentItem.id then
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
                    List.map (updateItemAnimation animMsg) model.treadmill

                ( items, cmds ) =
                    List.unzip itemCmds
            in
                ( { model | treadmill = items }, Cmd.batch cmds )

        Done doneId ->
            let
                newTreadmill =
                    case model.treadmill of
                        item :: items ->
                            items

                        items ->
                            items

                _ =
                    Debug.log "done" doneId
            in
                ( { model | treadmill = newTreadmill }, Cmd.none )


startItemAnimation : Int -> Int -> Item -> Item
startItemAnimation start end item =
    { item | imgs = Zipper.mapCurrent (startImgAnimation start end) item.imgs }


startImgAnimation : Int -> Int -> Img -> Img
startImgAnimation start end img =
    { img
        | style =
            Animation.interrupt
                [ Animation.set
                    [ Animation.left (px <| toFloat start)
                    ]
                , Animation.toWith (Animation.easing { duration = Time.second * 5, ease = Ease.linear })
                    [ Animation.left (px <| toFloat end)
                    ]
                , Animation.Messenger.send (Done img.id)
                ]
                img.style
    }


updateItemAnimation : Animation.Msg -> Item -> ( Item, Cmd Msg )
updateItemAnimation animMsg item =
    let
        ( newImg, cmd ) =
            item.imgs
                |> Zipper.current
                |> updateImgAnimation animMsg

        newImgs =
            Zipper.mapCurrent (\_ -> newImg) item.imgs
    in
        ( { item | imgs = newImgs }, cmd )


updateImgAnimation : Animation.Msg -> Img -> ( Img, Cmd Msg )
updateImgAnimation animMsg img =
    let
        ( style, cmd ) =
            Animation.Messenger.update animMsg img.style
    in
        ( { img | style = style }, cmd )



-- VIEW


view : Model -> Html Msg
view model =
    Html.div []
        ([ startButton
         , stopButton
         , word model
         , treadmill model
         , points model
         , notice model
         ]
            ++ (model.treadmill
                    |> List.map viewItem
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
        Html.h3 [] [ Html.text currentWord ]


stopButton : Html Msg
stopButton =
    Html.button [ onClick Stop ] [ Html.text "Stop" ]


startButton : Html Msg
startButton =
    Html.button [ onClick Start ] [ Html.text "Start" ]


viewItem : Item -> Html Msg
viewItem item =
    let
        img =
            Zipper.current item.imgs
    in
        Html.img
            ((Animation.render img.style)
                ++ [ src img.src
                   , onClick (ItemClicked item)
                   , width 100
                   , height 100
                   , style
                        [ ( "position", "absolute" )
                        , ( "top", "300px" )
                        ]
                   ]
            )
            []


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


imgStyles : Zipper Img -> List (Animation.Messenger.State Msg)
imgStyles imgs =
    imgs
        |> Zipper.map .style
        |> Zipper.toList


itemStyles : List Item -> List (Animation.Messenger.State Msg)
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
