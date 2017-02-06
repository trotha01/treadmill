module Main exposing (..)

import Html exposing (Html)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Svg exposing (Svg, svg)
import Svg.Attributes exposing (fill, x, y, rx, ry)
import Animation exposing (px)


-- MODEL


type alias Model =
    { style : Animation.State }


init =
    ( { style =
            Animation.style
                [ Animation.left (px 300) ]
      }
    , Cmd.none
    )



-- UPDATE


type Msg
    = Animate Animation.Msg
    | Start


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Start ->
            ( { model
                | style =
                    Animation.interrupt
                        [ Animation.to
                            [ Animation.left (px 30)
                            ]
                        ]
                        model.style
              }
            , Cmd.none
            )

        Animate animMsg ->
            let
                newModel =
                    { model
                        | style = Animation.update animMsg model.style
                    }
            in
                ( newModel, Cmd.none )



-- VIEW


view : Model -> Html Msg
view model =
    -- model
    Html.div []
        [ start, treadmill, milk model ]


start : Html Msg
start =
    Html.button [ onClick Start ] [ Html.text "Start" ]


milk : Model -> Html Msg
milk model =
    Html.img
        (Animation.render model.style
            ++ [ src "imgs/milk.jpg"
               , width 100
               , height 100
               , style
                    [ ( "position", "absolute" )
                    ]
               ]
        )
        []


treadmill : Html Msg
treadmill =
    svg
        [ style
            [ ( "position", "absolute" )
            , ( "top", "100px" )
            ]
        ]
        [ Svg.rect [ fill "black", x "0", y "0", rx "15", ry "15", Svg.Attributes.width "500", Svg.Attributes.height "2" ] [] ]



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Animation.subscription Animate [ model.style ]



--MAIN


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
