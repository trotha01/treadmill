module Item exposing (..)

import Animation exposing (px)
import Animation.Messenger
import Ease
import Html exposing (Html)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Time
import Random exposing (Generator)
import Zipper as Zipper exposing (..)


-- MODEL


type alias Model msg =
    { word : String
    , imgs : Zipper (Img msg)
    }


type alias Img msg =
    { src : String
    , style : Animation.Messenger.State msg
    }


{-| Images from https://pixabay.com
-}
initItems : Zipper (Model msg)
initItems =
    Zipper.fromListWithFocus
        (initItem "leche" "imgs/milk.jpg" [])
        [ (initItem "café" "imgs/coffee.png" [])
        , (initItem "silla" "imgs/chair-antique.png" [ "imgs/table-with-chairs.png" ])
        , (initItem "mesa" "imgs/table.png" [ "imgs/table-with-chairs.png" ])
        ]


initItem : String -> String -> List String -> Model msg
initItem word imgFocus imgSrcs =
    let
        imgs =
            Zipper.fromListWithFocus imgFocus imgSrcs

        initedImgs =
            Zipper.map initImg imgs
    in
        { word = word
        , imgs = initedImgs
        }


initImg : String -> Img msg
initImg imgSrc =
    { src = imgSrc
    , style =
        Animation.style
            [ Animation.left (px -500) ]
    }



-- ANIMATION


startItemAnimation : (Img msg -> msg) -> Int -> Int -> Model msg -> Model msg
startItemAnimation doneMsg start end item =
    { item | imgs = Zipper.mapCurrent (startImgAnimation doneMsg start end) item.imgs }


startImgAnimation : (Img msg -> msg) -> Int -> Int -> Img msg -> Img msg
startImgAnimation doneMsg start end img =
    { img
        | style =
            Animation.interrupt
                [ Animation.set
                    [ Animation.left (px <| toFloat start)
                    ]
                , Animation.toWith (Animation.easing { duration = Time.second * 5, ease = Ease.linear })
                    [ Animation.left (px <| toFloat end)
                    ]
                , Animation.Messenger.send (doneMsg img)
                ]
                img.style
    }


updateItemAnimation : Animation.Msg -> Model msg -> ( Model msg, Cmd msg )
updateItemAnimation animMsg item =
    let
        ( newImg, cmd ) =
            item.imgs
                |> Zipper.current
                |> updateImgAnimation animMsg

        newImgs =
            Zipper.mapCurrent (\_ -> newImg) item.imgs
    in
        ( { item | imgs = newImgs }, cmd )


updateImgAnimation : Animation.Msg -> Img msg -> ( Img msg, Cmd msg )
updateImgAnimation animMsg img =
    let
        ( style, cmd ) =
            Animation.Messenger.update animMsg img.style
    in
        ( { img | style = style }, cmd )



-- VIEW


viewItem : (Model msg -> msg) -> Model msg -> Html msg
viewItem clickMsg item =
    let
        img =
            Zipper.current item.imgs
    in
        Html.img
            ((Animation.render img.style)
                ++ [ src img.src
                   , onClick (clickMsg item)
                   , width 100
                   , height 100
                   , style
                        [ ( "position", "absolute" )
                        , ( "top", "300px" )
                        ]
                   ]
            )
            []



-- HELPERS


randItem : Zipper (Model msg) -> Generator (Maybe (Model msg))
randItem items =
    let
        itemList =
            Zipper.toList items

        len =
            List.length itemList

        i =
            Random.int 0 (len - 1)

        item =
            Random.map (listAt itemList) i
    in
        item


{-|
  listAt returns the item in the list at position n
-}
listAt : List a -> Int -> Maybe a
listAt items n =
    List.drop n items |> List.head