module Main exposing (..)

import Html exposing (Html)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Svg exposing (Svg, svg)
import Svg.Attributes exposing (fill, x, y, rx, ry)
import Animation exposing (px)
import Animation.Messenger
import List.Zipper as Zipper exposing (..)


{-
   TODO:
   - remove imgs from screen when off the left side
   - Add word of imgs to click on
   - Change imgs into buttons to click on
   - Check if img matches word
   - Have list of visible items, and list of all items
   - get zipper list working
-}
-- MODEL


type alias Model =
    { items : Maybe (Zipper Item)
    }


type alias ID =
    Int


type alias Item =
    { id : ID
    , word : String
    , imgs : List Img
    , treadmill : Bool
    }


type alias Img =
    { id : ID
    , src : String
    , style : Animation.Messenger.State Msg
    }


init : ( Model, Cmd Msg )
init =
    ( { items = initItems }
    , Cmd.none
    )


initItem : ID -> Item
initItem id =
    { id = id
    , word = "milk"
    , treadmill = False
    , imgs =
        [ (initImg 0 start)
        , (initImg 1 500)
        , (initImg 2 700)
        ]
    }


initImg : ID -> Float -> Img
initImg id start =
    { id = id
    , src = "imgs/milk.jpg"
    , style =
        Animation.style
            [ Animation.left (px start) ]
    }


( start, finish ) =
    ( 300, 30 )


initItems : Maybe (Zipper Item)
initItems =
    Zipper.fromList [ (initItem 0) ]



-- UPDATE


type Msg
    = Animate Animation.Msg
    | Start
    | Done Int


startItemAnimation : Item -> Item
startItemAnimation item =
    { item | imgs = List.map startImgAnimation item.imgs }


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
        imgCmds =
            List.map (updateImgAnimation animMsg) item.imgs

        ( imgs, cmds ) =
            List.unzip imgCmds
    in
        ( { item | imgs = imgs }, Cmd.batch cmds )


updateImgAnimation : Animation.Msg -> Img -> ( Img, Cmd Msg )
updateImgAnimation animMsg img =
    let
        ( style, cmd ) =
            Animation.Messenger.update animMsg img.style
    in
        ( { img | style = style }, cmd )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Start ->
            let
                items =
                    Maybe.map (Zipper.map startItemAnimation) model.items
            in
                ( { model | items = items }, Cmd.none )

        Animate animMsg ->
            let
                itemCmds =
                    Maybe.map (Zipper.map (updateItemAnimation animMsg)) model.items

                ( items, cmds ) =
                    List.unzip (listFromZipper itemCmds)
            in
                ( { model | items = Zipper.fromList items }, Cmd.batch cmds )

        Done doneId ->
            let
                {-
                   items =
                       List.filter (\item -> item.id == doneId) model.items
                -}
                _ =
                    Debug.log "done" doneId
            in
                ( model, Cmd.none )



-- VIEW


view : Model -> Html Msg
view model =
    Html.div []
        ([ startButton, treadmill ]
            ++ (List.concat (List.map (.imgs >> (List.map milk)) <| listFromZipper model.items))
        )


startButton : Html Msg
startButton =
    Html.button [ onClick Start ] [ Html.text "Start" ]


milk : Img -> Html Msg
milk img =
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


imgStyles : List Img -> List (Animation.Messenger.State Msg)
imgStyles imgs =
    List.map .style imgs


listFromZipper : Maybe (Zipper a) -> List a
listFromZipper zipper =
    case zipper of
        Nothing ->
            []

        Just zipper ->
            Zipper.toList zipper


itemStyles : Maybe (Zipper Item) -> List (Animation.Messenger.State Msg)
itemStyles items =
    (List.map (.imgs >> imgStyles) <| listFromZipper items)
        |> List.concat


subscriptions : Model -> Sub Msg
subscriptions model =
    Animation.subscription Animate (itemStyles model.items)



--MAIN


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
