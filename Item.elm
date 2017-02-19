module Item exposing (..)

import Animation exposing (px)
import Animation.Messenger
import Ease
import Html exposing (Html)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Random exposing (Generator)
import Time
import TouchEvents as Touch exposing (..)
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
        , (initItem "silla" "imgs/chair-antique.png" [ "imgs/design-chair.jpg" ])
        , (initItem "mesa" "imgs/table.png" [ "imgs/folding-table.png" ])
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
    let
        newImgs =
            Zipper.safeNext
                item.imgs
    in
        { item | imgs = Zipper.mapCurrent (startImgAnimation doneMsg start end) newImgs }


startImgAnimation : (Img msg -> msg) -> Int -> Int -> Img msg -> Img msg
startImgAnimation doneMsg start end img =
    { img
        | style =
            Animation.interrupt
                [ Animation.set
                    [ Animation.left (px <| toFloat start)
                    ]
                , Animation.toWith (Animation.easing { duration = Time.second * 4, ease = Ease.linear })
                    [ Animation.left (px <| toFloat end)
                    ]
                , Animation.Messenger.send (doneMsg img)
                ]
                img.style
    }


stopItemAnimation : (Img msg -> msg) -> Int -> Int -> Model msg -> Model msg
stopItemAnimation doneMsg start end item =
    let
        newImgs =
            Zipper.safeNext
                item.imgs
    in
        { item | imgs = Zipper.mapCurrent (startImgAnimation doneMsg start end) newImgs }


stopImgAnimation : (Img msg -> msg) -> Int -> Int -> Img msg -> Img msg
stopImgAnimation doneMsg start end img =
    { img
        | style =
            Animation.interrupt
                [ Animation.set
                    [ Animation.left (px <| toFloat start)
                    ]
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


viewItem : (Model msg -> msg) -> (Model msg -> Touch -> msg) -> Model msg -> Html msg
viewItem clickMsg touchMsg item =
    let
        img =
            Zipper.current item.imgs
    in
        Html.img
            ((Animation.render img.style)
                ++ [ src img.src
                   , onClick (clickMsg item)
                   , Touch.onTouchStart (touchMsg item)
                   , width 100
                   , height 100
                   , style
                        [ ( "position", "absolute" ) ]
                   ]
            )
            []



-- HELPERS


{-| randItem returns the focus item 20% of the time
    and other items 80% of the time
-}
randItem : Zipper (Model msg) -> Generator (Maybe (Model msg))
randItem items =
    let
        ( preferredItem, otherItems ) =
            ( Zipper.current items, Zipper.before items ++ Zipper.after items )

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


{-|
  listAt returns the item in the list at position n
-}
listAt : List a -> Int -> Maybe a
listAt items n =
    List.drop n items |> List.head
