module Bowl exposing (..)

import BoundingBox exposing (BoundingBox)
import Html exposing (Html)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Item
import Math.Vector2 as Vector2 exposing (Vec2, getX, getY, vec2)
import Time exposing (Time)
import Round
import Zipper


-- MODEL


{-| Model is simply the x axis of the bowl
-}
type alias Model msg =
    { x : Float
    , items : List (Item.Model msg)
    , start : Float
    }


init : Float -> Model msg
init x =
    { x = x
    , items = []
    , start = x
    }


{-| Constants for the bowl
-}
( y, bowlWidth, bowlTopHeight, bowlTotalHeight ) =
    ( 245, 200, 50, 150 )



-- HELPER FUNCTIONS


done : Model msg -> Bool
done bowl =
    bowl.x == 0


distance : Model msg -> String
distance bowl =
    Round.round 2 <| (bowl.start - bowl.x) / 24


boundingBoxFromBowl : Model msg -> BoundingBox
boundingBoxFromBowl bowl =
    BoundingBox.fromCorners (vec2 bowl.x y) (vec2 (bowl.x + bowlWidth) (y + bowlTotalHeight))


{-| contains checks if the point is inside the bowl
-}
contains : Vec2 -> Model msg -> Bool
contains point bowl =
    BoundingBox.contains point (boundingBoxFromBowl bowl)


{-| inside checks if the bounding box is inside the bowl
-}
inside : BoundingBox -> Model msg -> Bool
inside box bowl =
    BoundingBox.intersects (boundingBoxFromBowl bowl) box


full : Model msg -> Bool
full model =
    (List.length model.items) == 4



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
        [ viewBowlTop
        , viewBowlBottom
        , viewItems model
        ]


viewBowlTop =
    Html.div []
        [ Html.img
            [ bowlTopImgStyle
            , src "imgs/cake-bowl-top.png"
            ]
            []
        ]


viewBowlBottom =
    Html.div []
        [ Html.img
            [ bowlImgStyle
            , src "imgs/cake-bowl-bottom.png"
            ]
            []
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
                , ( "top", "20px" )
                ]
            ]
            []


bowlTopImgStyle : Html.Attribute msg
bowlTopImgStyle =
    style
        [ ( "width", (toString bowlWidth) ++ "px" )
        , ( "height", (toString bowlTopHeight) ++ "px" )
        , ( "top", "5px" )
        , ( "left", "0px" )
        , ( "position", "relative" )
        ]


bowlImgStyle : Html.Attribute msg
bowlImgStyle =
    style
        [ ( "width", (toString bowlWidth) ++ "px" )
        , ( "height", "100px" )
        , ( "left", "0px" )
        , ( "top", "0px" )
        , ( "position", "relative" )
        ]


bowlStyle : Float -> Html.Attribute msg
bowlStyle x =
    style
        [ ( "position", "absolute" )
        , ( "left", (toString x) ++ "px" )
        , ( "top", (toString y) ++ "px" )
        , ( "width", (toString bowlWidth) ++ "px" )
        , ( "height", (toString bowlTotalHeight) ++ "px" )
        , ( "text-align", "center" )
        , ( "vertical-align", "middle" )
        , ( "cursor", "pointer" )
        , ( "user-select", "none" )
        ]
