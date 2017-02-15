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


{-
   TODO:
   - remove imgs from screen when off the left side
   - Add word of imgs to click on
   - Change imgs into buttons to click on
   - Check if img matches word
-}
-- MODEL


type alias Model =
    { items : Zipper Item
    , treadmill : List Item
    , seed : Random.Seed
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
      }
    , Cmd.none
    )


initItems : Zipper Item
initItems =
    Zipper.singleton (initItem 0 "milk" "imgs/milk.jpg")
        |> Zipper.appendItem (initItem 1 "coffee" "imgs/coffee.png")



{-
   Zipper.fromList
       [ (initItem 0 "milk" "imgs/milk.jpg")
       , (initItem 1 "coffee" "imgs/coffee.png")
       ]
-}


initItem : ID -> String -> String -> Item
initItem id word imgSrc =
    { id = id
    , word = word
    , imgs =
        Zipper.singleton (initImg 0 start imgSrc)
            |> Zipper.appendList
                [ (initImg 1 500 imgSrc)
                , (initImg 2 700 imgSrc)
                ]
    }


initImg : ID -> Float -> String -> Img
initImg id start imgSrc =
    { id = id
    , src = imgSrc
    , style =
        Animation.style
            [ Animation.left (px start) ]
    }


( start, finish ) =
    ( 300, 30 )


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
    | Done Int


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Start ->
            let
                ( newItem, newSeed ) =
                    Random.step (randItem model.items) model.seed

                newAnimatedItem =
                    Maybe.map startItemAnimation newItem

                newModel =
                    case (Debug.log "Adding" newAnimatedItem) of
                        Nothing ->
                            { model | seed = newSeed }

                        Just animatedItem ->
                            { model | seed = newSeed, treadmill = model.treadmill ++ [ animatedItem ] }
            in
                ( newModel, Cmd.none )

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


startItemAnimation : Item -> Item
startItemAnimation item =
    { item | imgs = Zipper.mapCurrent startImgAnimation item.imgs }


startImgAnimation : Img -> Img
startImgAnimation img =
    { img
        | style =
            Animation.interrupt
                [ Animation.to
                    [ Animation.left (px finish)
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
        ([ startButton, treadmill ]
            ++ (model.treadmill
                    |> List.map .imgs
                    |> List.map Zipper.current
                    |> List.map viewImg
               )
        )


startButton : Html Msg
startButton =
    Html.button [ onClick Start ] [ Html.text "Start" ]


viewImg : Img -> Html Msg
viewImg img =
    Html.img
        ((Animation.render img.style)
            ++ [ src img.src
               , width 100
               , height 100
               , style
                    [ ( "position", "absolute" )
                    , ( "top", "100px" )
                    ]
               ]
        )
        []


treadmill : Html Msg
treadmill =
    svg
        [ style
            [ ( "position", "absolute" )
            , ( "top", "200px" )
            ]
        ]
        [ Svg.rect [ fill "black", x "0", y "0", rx "15", ry "15", Svg.Attributes.width "500", Svg.Attributes.height "2" ] [] ]



-- SUBSCRIPTIONS


imgStyles : Zipper Img -> List (Animation.Messenger.State Msg)
imgStyles imgs =
    imgs
        |> Zipper.map .style
        |> Zipper.toList


listFromZipper : Maybe (Zipper a) -> List a
listFromZipper zipper =
    case zipper of
        Nothing ->
            []

        Just zipper ->
            Zipper.toList zipper


itemStyles : List Item -> List (Animation.Messenger.State Msg)
itemStyles items =
    List.map (.imgs >> imgStyles) items
        |> List.concat


subscriptions : Model -> Sub Msg
subscriptions model =
    Animation.subscription Animate (itemStyles model.treadmill)



--MAIN


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
