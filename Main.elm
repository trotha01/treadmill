module Main exposing (..)

import Html exposing (Html)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Svg exposing (Svg, svg)
import Svg.Attributes exposing (fill, x, y, rx, ry)
import Animation exposing (px)


-- MODEL


type alias Model =
    { items : List ( Int, Item ) }


type alias Item =
    { style : Animation.State }


init : ( Model, Cmd Msg )
init =
    ( { items = initItems }, Cmd.none )


initItem : Item
initItem =
    { style =
        Animation.style
            [ Animation.left (px 300) ]
    }


initItems : List ( Int, Item )
initItems =
    [ ( 0, initItem ) ]



-- UPDATE


type Msg
    = Animate Animation.Msg
    | Start


startItemAnimation : Item -> Item
startItemAnimation item =
    { item
        | style =
            Animation.interrupt
                [ Animation.to
                    [ Animation.left (px 30)
                    ]
                ]
                item.style
    }


updateItemAnimation : Animation.Msg -> Item -> Item
updateItemAnimation animMsg item =
    { item | style = Animation.update animMsg item.style }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Start ->
            let
                items =
                    List.map
                        (\( id, item ) ->
                            ( id, startItemAnimation item )
                        )
                        model.items
            in
                ( { model | items = items }, Cmd.none )

        Animate animMsg ->
            let
                items =
                    List.map
                        (\( id, item ) ->
                            ( id, updateItemAnimation animMsg item )
                        )
                        model.items
            in
                ( { model | items = items }, Cmd.none )



-- VIEW


view : Model -> Html Msg
view model =
    Html.div []
        ([ start, treadmill ]
            ++ (List.map (\( _, item ) -> milk item) model.items)
        )


start : Html Msg
start =
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


subscriptions : Model -> Sub Msg
subscriptions model =
    Animation.subscription Animate (List.map (\( _, i ) -> i.style) model.items)



--MAIN


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
