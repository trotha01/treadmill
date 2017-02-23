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
    ( 300, 100, 100 )



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


view : Model -> Html msg
view model =
    Html.img
        [ src "imgs/cake-bowl.png"
        , bowlStyle model
        ]
        []


bowlStyle : Model -> Html.Attribute msg
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
