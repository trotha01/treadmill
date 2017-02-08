module Main exposing (..)

import Html exposing (Html)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Svg exposing (Svg, svg)
import Svg.Attributes exposing (fill, x, y, rx, ry)
import Animation exposing (px)
import Animation.Messenger


{-
   TODO:
   - remove items from screen when off the left side
   - Add word of items to click on
   - Change items into buttons to click on
   - Check if item matches word
-}
-- MODEL


type alias Model =
    { items : List Item }


type alias ID =
    Int


type alias Item =
    { id : ID
    , style : Animation.Messenger.State Msg
    }


init : ( Model, Cmd Msg )
init =
    ( { items = initItems }, Cmd.none )


initItem : ID -> Float -> Item
initItem id start =
    { id = id
    , style =
        Animation.style
            [ Animation.left (px start) ]
    }


( start, finish ) =
    ( 300, 30 )


initItems : List Item
initItems =
    [ (initItem 0 start)
    , (initItem 1 500)
    , (initItem 2 700)
    ]



-- UPDATE


type Msg
    = Animate Animation.Msg
    | Start
    | Done Int


startItemAnimation : Item -> Item
startItemAnimation item =
    { item
        | style =
            Animation.interrupt
                [ Animation.to
                    [ Animation.left (px finish)
                    ]
                , Animation.Messenger.send (Done (Debug.log "start" item.id))
                ]
                item.style
    }


updateItemAnimation : Animation.Msg -> Item -> ( Item, Cmd Msg )
updateItemAnimation animMsg item =
    let
        ( style, cmd ) =
            Animation.Messenger.update animMsg item.style
    in
        ( { item | style = style }, cmd )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Start ->
            let
                items =
                    List.map startItemAnimation model.items
            in
                ( { model | items = items }, Cmd.none )

        Animate animMsg ->
            let
                itemCmds =
                    List.map (updateItemAnimation animMsg) model.items

                ( items, cmds ) =
                    List.unzip itemCmds
            in
                ( { model | items = items }, Cmd.batch cmds )

        Done doneId ->
            let
                items =
                    List.filter (\item -> item.id == doneId) model.items

                _ =
                    Debug.log "done" doneId
            in
                ( { model | items = items }, Cmd.none )



-- VIEW


view : Model -> Html Msg
view model =
    Html.div []
        ([ startButton, treadmill ]
            ++ (List.map milk model.items)
        )


startButton : Html Msg
startButton =
    Html.button [ onClick Start ] [ Html.text "Start" ]


milk : Item -> Html Msg
milk item =
    Html.img
        ((Animation.render item.style)
            ++ [ src "imgs/milk.jpg"
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


itemStyles : List Item -> List (Animation.Messenger.State Msg)
itemStyles items =
    List.map .style items


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
