module Treadmill exposing (..)

import AnimationFrame
import Html exposing (Html)
import Html.Attributes exposing (..)
import Item
import Svg exposing (Svg, svg)
import Svg.Attributes exposing (fill, rx, ry, x, y)
import Time exposing (..)
import TouchEvents as Touch exposing (..)


-- MODEL


type alias Belt =
    { length : Int
    , items : List ( Int, Item.Model )
    }


init : Belt
init =
    { length = 0
    , items = []
    }



-- UPDATE


type Msg
    = Tick Time.Time


update : Msg -> Belt -> ( Belt, Cmd msg )
update msg belt =
    case msg of
        Tick delta ->
            ( animate delta belt, Cmd.none )


addItem : Int -> (Int -> Item.Img -> msg) -> Belt -> Item.Model -> Belt
addItem windowWidth done belt item =
    let
        newID =
            belt.length

        animatedItem =
            -- (Item.startItemAnimation (done newID) windowWidth -100) item
            item
    in
    { belt | items = ( newID, animatedItem ) :: belt.items, length = belt.length + 1 }


removeItem : Int -> Belt -> Belt
removeItem id belt =
    { belt | items = List.filter (\( itemID, _ ) -> itemID /= id) belt.items }


animate : Time.Time -> Belt -> Belt
animate delta belt =
    let
        movedItems =
            List.map (\( id, item ) -> ( id, Item.move delta item )) belt.items
    in
    { belt | items = movedItems }



-- VIEW


type alias ClickMsg msg =
    Int -> Item.Model -> msg


type alias TouchMsg msg =
    Int -> Item.Model -> Touch -> msg


view : Int -> ClickMsg msg -> TouchMsg msg -> Belt -> Html msg
view windowWidth clickMsg touchMsg belt =
    let
        items =
            belt.items
                |> List.map (\( id, item ) -> Item.viewItem (clickMsg id) (touchMsg id) item)

        viewBelt =
            svg
                [ style
                    [ ( "position", "absolute" )
                    , ( "top", "100px" )
                    , ( "width", toString windowWidth ++ "px" )
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
            , ( "width", toString windowWidth ++ "px" )
            ]
        ]
        (viewBelt :: items)



-- SUBSCRIPTIONS

subscription : (Msg -> msg) -> Belt -> Sub msg
subscription msg belt =
    AnimationFrame.diffs (Tick >> msg)


-- HELPERS


zip : List a -> List b -> List ( a, b )
zip xs ys =
    zip_ xs ys []


zip_ : List a -> List b -> List ( a, b ) -> List ( a, b )
zip_ xs ys xys =
    case ( xs, ys ) of
        ( x :: xs, y :: ys ) ->
            zip_ xs ys (xys ++ [ ( x, y ) ])

        ( xs, ys ) ->
            xys
