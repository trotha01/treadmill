module Bowl exposing (..)

import BoundingBox exposing (BoundingBox)
import Html exposing (Html)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Math.Vector2 as Vector2 exposing (Vec2, getX, getY, vec2)


-- MODEL


{-| Model is simply the x axis of the bowl
-}
type alias Model =
    Float


{-| Constants for the bowl
-}
( y, bowlWidth, bowlHeight ) =
    ( 50, 100, 100 )



-- HELPER FUNCTIONS


boundingBoxFromBowl : Model -> BoundingBox
boundingBoxFromBowl x =
    BoundingBox.fromCorners (vec2 x y) (vec2 (x + bowlWidth) (y + bowlHeight))


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



-- VIEW


cakeImgStyle : Vec2 -> Html.Attribute msg
cakeImgStyle pos =
    style
        [ ( "position", "absolute" )
        , ( "left", ((getX pos |> toString) ++ "px") )
        , ( "top", ((getY pos |> toString) ++ "px") )
        , ( "width", "75px" )
        , ( "height", "75px" )
        , ( "text-align", "center" )
        , ( "vertical-align", "middle" )
        , ( "cursor", "pointer" )
        , ( "user-select", "none" )
        ]


view : Model -> Html msg
view model =
    Html.div
        [ cakeImgStyle (vec2 100 100)
        , style
            [ ( "display", "block" )
            , ( "position", "absolute" )
            , ( "left", (toString model) ++ "px" )
            ]
        ]
        [ Html.img
            [ src "imgs/cake-bowl.png"
            , width bowlWidth
            , height bowlHeight
            ]
            []
        ]
