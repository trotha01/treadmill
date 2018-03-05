module Item exposing (..)

import Html exposing (Html)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Math.Vector2 exposing (..)
import Random exposing (Generator)
import Time
import TouchEvents as Touch exposing (..)
import Window
import Zipper as Zipper exposing (..)


-- MODEL


type alias Model =
    { word : String
    , imgs : Zipper Img
    , position : Vec2
    , velocity : Vec2
    }


type alias Img =
    { src : String
    }


speed : Float
speed =
    0.3


{-| Images from <https://pixabay.com>
-}
initItems : Zipper Model
initItems =
    Zipper.fromListWithFocus
        (initItem "leche" "imgs/milk.jpg" [])
        [ initItem "café" "imgs/coffee.png" []
        , initItem "silla" "imgs/chair-antique.png" [ "imgs/design-chair.jpg" ]
        , initItem "mesa" "imgs/table.png" [ "imgs/folding-table.png" ]
        , initItem "huevo" "imgs/egg.png" []
        , initItem "harina" "imgs/flour.png" []
        , initItem "azúcar" "imgs/sugar.png" []
        ]


initItem : String -> String -> List String -> Model
initItem word imgFocus imgSrcs =
    let
        imgs =
            Zipper.fromListWithFocus imgFocus imgSrcs

        initedImgs =
            Zipper.map initImg imgs
    in
    { word = word
    , imgs = initedImgs
    , position = vec2 0 0
    , velocity = vec2 -speed 0
    }


initImg : String -> Img
initImg imgSrc =
    { src = imgSrc
    }


bowl : Model
bowl =
    initItem "bowl" "imgs/cake-bowl.png" []



-- ANIMATION


move : Time.Time -> Model -> Model
move delta item =
    -- p1 = p0 + v*t
    { item | position = add item.position (scale delta item.velocity) }



-- VIEW


viewItem : (Model -> msg) -> (Model -> Touch -> msg) -> Model -> Html msg
viewItem clickMsg touchMsg item =
    let
        img =
            Zipper.current item.imgs
    in
    Html.img
        [ src img.src
         , class "treadmill-item"
         , onClick (clickMsg item)
         , Touch.onTouchStart (touchMsg item)
         , width 100
         , height 100
         , style
            [ ( "position", "absolute" )
            , ( "top", px <| getY item.position )
            , ( "left", px <| getX item.position )
            ]
         ]
        []


px : Float -> String
px f =
    toString f ++ "px"



-- HELPERS


{-| randItem returns the focus item 20% of the time
and other items 80% of the time
-}
randItem : Window.Size -> Zipper Model -> Generator (Maybe Model)
randItem window items =
    let
        movedItems =
            Zipper.map (\i -> { i | position = vec2 (toFloat (window.width + 100)) 0 }) items

        ( preferredItem, otherItems ) =
            ( Zipper.current movedItems, Zipper.before movedItems ++ Zipper.after movedItems )

        len =
            List.length otherItems

        i =
            Random.int 0 (len - 1)

        item =
            Random.int 1 5
                |> Random.andThen
                    (xPercentChance
                        (Random.map (\_ -> Just preferredItem) Random.bool)
                        (Random.map (listAt otherItems) i)
                    )
    in
    item


xPercentChance : a -> a -> Int -> a
xPercentChance a b x =
    case x of
        1 ->
            a

        _ ->
            b


{-| listAt returns the item in the list at position n
-}
listAt : List a -> Int -> Maybe a
listAt items n =
    List.drop n items |> List.head
