module Bowl exposing (..)

import BoundingBox exposing (BoundingBox)
import Html exposing (Html)
import Html.Attributes exposing (..)
import Item
import Math.Vector2 as Vector2 exposing (Vec2, getX, getY, vec2)
import Round
import Time exposing (Time)
import Zipper


-- MODEL


{-| Model is simply the x axis of the bowl
-}
type alias Model =
    { x : Float

    -- items is a list of items that have been poured into the bowl
    , items : List Item.Model
    , start : Float
    }


init : Float -> Model
init x =
    { x = x
    , items = []
    , start = x
    }


{-| Constants for the bowl
-}
( y, bowlWidth, bowlHeight, bowlTopHeight ) =
    ( 30, 20, 15, 5 )



-- HELPER FUNCTIONS


done : Model -> Bool
done bowl =
    bowl.x == 0


distance : Model -> String
distance bowl =
    Round.round 2 <| (bowl.start - bowl.x) / 24


boundingBoxFromBowl : Model -> BoundingBox
boundingBoxFromBowl bowl =
    BoundingBox.fromCorners (vec2 bowl.x (y - bowlTopHeight)) (vec2 (bowl.x + bowlWidth) y)


{-| contains checks if the point is inside the bowl
-}
contains : Vec2 -> Model -> Bool
contains point bowl =
    BoundingBox.contains point (boundingBoxFromBowl bowl)


{-| inside checks if the bounding box is inside the bowl
-}
inside : BoundingBox -> Model -> Bool
inside box bowl =
    BoundingBox.inside box (boundingBoxFromBowl bowl)


full : Model -> Bool
full model =
    List.length model.items == 4



-- UPDATE


step : Time -> Model -> Model
step time bowl =
    let
        vel =
            0.01
    in
    { bowl | x = Basics.max 0 (bowl.x - vel * time) }



-- VIEW


view : Model -> Html msg
view model =
    Html.div [ bowlStyle model.x ]
        [ viewBowlTop
        , viewBowlBottom
        , viewItems model
        ]


viewBowlTop : Html msg
viewBowlTop =
    Html.div []
        [ Html.img
            [ bowlTopImgStyle
            , src "imgs/cake-bowl-top.png"
            ]
            []
        ]


viewBowlBottom : Html msg
viewBowlBottom =
    Html.div []
        [ Html.img
            [ bowlImgStyle
            , src "imgs/cake-bowl-bottom.png"
            ]
            []
        ]


viewItems : Model -> Html msg
viewItems model =
    Html.div []
        (List.indexedMap (viewItem model.x) model.items)


itemWidth : Int
itemWidth =
    200


viewItem : Float -> Int -> Item.Model -> Html msg
viewItem x i item =
    let
        img =
            Zipper.current item.imgs
    in
    Html.img
        [ src img.src
        , class "bowl-item"
        , style
            [ ( "position", "absolute" )
            , ( "left", (i * 50 |> toString) ++ "px" )
            ]
        ]
        []


bowlTopImgStyle : Html.Attribute msg
bowlTopImgStyle =
    style
        [ ( "width", toString bowlWidth ++ "rem" )
        , ( "height", "50px" )
        , ( "top", "-50px" )
        , ( "left", "0px" )
        , ( "position", "absolute" )
        ]


bowlImgStyle : Html.Attribute msg
bowlImgStyle =
    style
        [ ( "width", toString bowlWidth ++ "rem" )
        , ( "height", "100px" )
        , ( "left", "0px" )
        , ( "position", "absolute" )
        ]


bowlStyle : Float -> Html.Attribute msg
bowlStyle x =
    style
        [ ( "position", "absolute" )
        , ( "left", toString x ++ "rem" )
        , ( "top", toString y ++ "rem" )
        , ( "width", toString bowlWidth ++ "rem" )
        , ( "height", toString bowlHeight ++ "rem" )
        , ( "text-align", "center" )
        , ( "vertical-align", "middle" )
        , ( "cursor", "pointer" )
        , ( "user-select", "none" )
        ]
