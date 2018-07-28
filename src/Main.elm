module Main exposing (..)

import Browser
import Html exposing (Html)
import Html.Attributes as Attr
import Html.Events as Events
import Svg
import Svg.Attributes as SvgAttr
import Svg.Events
import Time


main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }


init : () -> ( Model, Cmd Msg )
init _ =
    ( SelectSets
    , Cmd.none
    )


subscriptions : Model -> Sub Msg
subscriptions model =
    case model of
        ShowTimer state ->
            if state.paused then
                Sub.none
            else
                Time.every 1000 (always Tick)

        _ ->
            Sub.none


type Model
    = SelectSets
    | ShowTimer TimerState


type alias TimerState =
    { nrOfSets : Int
    , setsRemaining : Int
    , second : Int
    , paused : Bool
    }


type Msg
    = Tick
    | PlayPause
    | SelectNrOfSets Int


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case model of
        SelectSets ->
            case msg of
                SelectNrOfSets sets ->
                    ( ShowTimer (TimerState sets sets 0 True)
                    , Cmd.none
                    )

                _ ->
                    ( model, Cmd.none )

        ShowTimer state ->
            case msg of
                Tick ->
                    ( if state.second == 59 then
                        ShowTimer { state | second = 0, setsRemaining = state.setsRemaining - 1 }
                      else
                        ShowTimer { state | second = state.second + 1 }
                    , Cmd.none
                    )

                PlayPause ->
                    ( ShowTimer { state | paused = not state.paused }
                    , Cmd.none
                    )

                _ ->
                    ( model, Cmd.none )


view : Model -> Html Msg
view model =
    case model of
        SelectSets ->
            Html.div
                []
                (List.range 1 30 |> List.map selectSets)

        ShowTimer state ->
            timerView state


selectSets : Int -> Html Msg
selectSets n =
    Html.h1
        [ Events.onClick (SelectNrOfSets n)
        ]
        [ Html.text (String.fromInt n)
        ]


timerView : TimerState -> Html Msg
timerView model =
    Html.div
        []
        [ Html.div
            [ Attr.style "width" "50vw"
            , Attr.style "margin" "50px auto 0 auto"
            , Attr.style "position" "relative"
            ]
            [ radialProgress model.second
            , Html.h1
                [ Attr.style "position" "absolute"
                , Attr.style "top" "50%"
                , Attr.style "left" "50%"
                , Attr.style "transform" "translateX(-50%)"
                , Attr.style "margin-top" "-50px"
                , Attr.style "line-height" "100px"
                ]
                [ Html.text (String.fromInt model.second)
                ]
            ]
        , Html.h1
            [ Attr.style "text-align" "center"
            ]
            [ Html.text (String.fromInt (model.nrOfSets - model.setsRemaining))
            , Html.text " / "
            , Html.text (String.fromInt model.nrOfSets)
            ]
        , Html.h1
            [ Attr.style "text-align" "center"
            , Events.onClick PlayPause
            ]
            [ if model.paused then
                Html.text "Start"
              else
                Html.text "Pause"
            ]
        ]


radialProgress : Int -> Html Msg
radialProgress progress =
    Svg.svg
        [ SvgAttr.viewBox "0 0 120 120"
        ]
        [ Svg.circle
            [ SvgAttr.r "54"
            , SvgAttr.cx "60"
            , SvgAttr.cy "60"
            , SvgAttr.fill "none"
            , SvgAttr.stroke "white"
            , SvgAttr.strokeWidth "12"
            , SvgAttr.strokeDasharray "339.292"
            , SvgAttr.transform "rotate(-90, 60, 60)"
            , progress
                |> toFloat
                |> (\x -> x / 60)
                |> (*) -339.292
                |> String.fromFloat
                |> SvgAttr.strokeDashoffset
            ]
            []
        ]
