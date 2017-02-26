module Bowl exposing (..)

import BoundingBox exposing (BoundingBox)
import Html exposing (Html)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Item
import Math.Vector2 as Vector2 exposing (Vec2, getX, getY, vec2)
import Time exposing (Time)
import Zipper


-- MODEL


{-| Model is simply the x axis of the bowl
-}
type alias Model msg =
    { x : Float
    , items : List (Item.Model msg)
    }


init : Float -> Model msg
init x =
    { x = x
    , items = []
    }


{-| Constants for the bowl
-}
( y, bowlWidth, bowlHeight ) =
    ( 300, 100, 100 )



-- HELPER FUNCTIONS


done : Model msg -> Bool
done bowl =
    bowl.x == 0


boundingBoxFromBowl : Model msg -> BoundingBox
boundingBoxFromBowl bowl =
    BoundingBox.fromCorners (vec2 bowl.x y) (vec2 (bowl.x + bowlWidth) (y + bowlHeight))


{-| contains checks if the point is inside the bowl
-}
contains : Vec2 -> Model msg -> Bool
contains point bowl =
    BoundingBox.contains point (boundingBoxFromBowl bowl)


{-| inside checks if the bounding box is inside the bowl
-}
inside : BoundingBox -> Model msg -> Bool
inside box bowl =
    BoundingBox.inside box (boundingBoxFromBowl bowl)



-- UPDATE


step : Time -> Model msg -> Model msg
step time bowl =
    let
        vel =
            5
    in
        { bowl | x = Basics.max 0 (bowl.x - vel * (time / 100)) }



-- VIEW


view : Model msg -> Html msg
view model =
    Html.div [ bowlStyle model.x ]
        [ Html.img
            [ bowlImgStyle
            , src "imgs/cake-bowl.png"
            ]
            []
        , viewItems model
        ]


viewItems : Model msg -> Html msg
viewItems model =
    Html.div [] (List.indexedMap (viewItem model.x) model.items)


viewItem : Float -> Int -> Item.Model msg -> Html msg
viewItem x i item =
    let
        img =
            Zipper.current item.imgs
    in
        Html.img
            [ src img.src
            , width 50
            , height 50
            , style
                [ ( "position", "absolute" )
                , ( "left", (i * 50 |> toString) ++ "px" )
                ]
            ]
            []


bowlImgStyle : Html.Attribute msg
bowlImgStyle =
    style
        [ ( "width", (toString bowlWidth) ++ "px" )
        , ( "height", (toString bowlHeight) ++ "px" )
        , ( "left", "0px" )
        , ( "position", "absolute" )
        ]


bowlStyle : Float -> Html.Attribute msg
bowlStyle x =
    style
        [ ( "position", "absolute" )
        , ( "left", (toString x) ++ "px" )
        , ( "top", (toString y) ++ "px" )
        , ( "width", (toString bowlWidth) ++ "px" )
        , ( "height", (toString bowlHeight) ++ "px" )
        , ( "text-align", "center" )
        , ( "vertical-align", "middle" )
        , ( "cursor", "pointer" )
        , ( "user-select", "none" )
        ]
