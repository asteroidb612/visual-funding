module Main exposing (main)

import Browser
import Browser.Dom as Dom
import Browser.Events
import Color
import Css
import Css.Global
import Dict exposing (Dict)
import Html
import Html.Events
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (class, css, id, style)
import Html.Styled.Events exposing (onClick)
import List.Extra as List
import Set
import Tailwind.Utilities as Tw
import Task


type alias Model =
    { width : Float
    , height : Float
    , stageWidth : Float
    }


init : ( Model, Cmd Msg )
init =
    ( { height = 0
      , width = 0
      , stageWidth = 0
      }
    , Task.attempt GotViewport Dom.getViewport
    )


type Msg
    = GotResize Int Int
    | GotViewport (Result Dom.Error Dom.Viewport)
    | GotStageSize (Result Dom.Error Dom.Viewport)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotResize w h ->
            ( { model | width = toFloat w, height = toFloat h }
            , Task.attempt GotStageSize (Dom.getViewportOf "stage")
            )

        GotViewport result ->
            case result of
                Ok { viewport } ->
                    ( { model
                        | width = viewport.width
                        , height = viewport.height
                      }
                    , Task.attempt GotStageSize (Dom.getViewportOf "stage")
                    )

                Err _ ->
                    ( model, Cmd.none )

        GotStageSize result ->
            case result of
                Ok { viewport } ->
                    ( { model | stageWidth = viewport.width }, Cmd.none )

                Err _ ->
                    ( model, Cmd.none )


tailwindViewWrapper : Model -> Html.Html Msg
tailwindViewWrapper model =
    Html.Styled.toUnstyled <|
        div []
            [ Css.Global.global Tw.globalStyles
            , view model
            ]


view : Model -> Html Msg
view model =
    div
        [ css
            [ Tw.h_full
            , Tw.w_full
            , Tw.flex
            , Tw.justify_between
            , Tw.p_8
            ]
        ]
        [ div
            [ style "width" "50%"
            , style "position" "relative"
            , id "stage"
            ]
            [ stage model ]
        , div
            [ style "width" "50%"
            , style "height" "90vh"
            , css
                [ Tw.prose
                , Tw.overflow_scroll
                , Tw.p_8
                ]
            ]
            [ writing ]
        ]


spacer =
    div [ style "height" "20em" ] []


main : Program () Model Msg
main =
    Browser.element
        { init = always init
        , view = tailwindViewWrapper
        , update = update
        , subscriptions = \model -> Browser.Events.onResize GotResize
        }



{- Stage -}


stage model =
    div
        [ css [ Tw.ml_auto, Tw.mr_auto, Tw.absolute ]
        , style "top" "50%"
        , style "transform" "translateY(-50%)"
        ]
        [ drawDonations model.stageWidth ]


type alias Donor =
    { name : String
    , donationsByCause : Dict String Int
    , bonusByPassportSource : Dict String Float
    }


donors =
    [ { name = "Steve"
      , donationsByCause =
            Dict.fromList
                [ ( "Piano player", 5 )
                , ( "Piano podcast", 1 )
                , ( "MuseScore Open Source Project", 10 )
                , ( "Solar Coffee Research", 100 )
                ]
      , bonusByPassportSource = Dict.fromList []
      }
    , { name = "Rachael"
      , donationsByCause =
            Dict.fromList
                [ ( "Piano player", 5 )
                , ( "Piano podcast", 1 )
                , ( "MuseScore Open Source Project", 1 )
                , ( "Solar Coffee Research", 10 )
                ]
      , bonusByPassportSource = Dict.fromList []
      }
    , { name = "Sarah"
      , donationsByCause =
            Dict.fromList
                [ ( "Piano player", 5 )
                , ( "Piano podcast", 4 )
                , ( "Solar Coffee Research", 10 )
                ]
      , bonusByPassportSource = Dict.fromList []
      }
    ]


drawDonations : Float -> Html Msg
drawDonations width =
    let
        allCauses =
            donors
                |> List.map .donationsByCause
                |> List.concatMap Dict.keys
                |> Set.fromList
                |> Set.toList

        widthOfCause causeName =
            donors
                |> List.filterMap
                    (\donor ->
                        Dict.get causeName donor.donationsByCause
                    )
                |> List.map sqrt
                |> List.sum

        donations causeName =
            donors
                |> List.filterMap
                    (\donor ->
                        Dict.get causeName donor.donationsByCause
                    )

        totalWidth =
            List.map widthOfCause allCauses
                |> List.sum
    in
    allCauses
        |> List.map
            (\cause ->
                drawCause
                    cause
                    (widthOfCause cause / totalWidth * width)
                    (donations cause)
            )
        |> div [ css [ Tw.flex ], class "allCauses" ]


drawCause name width donations =
    let
        donationTotal =
            donations
                |> List.map sqrt
                |> List.sum

        sideSize amount =
            sqrt amount / donationTotal * width

        fmtSideSize amount =
            sideSize amount
                |> String.fromFloat
                |> (\x -> x ++ "px")

        drawDonation i amount =
            div
                [ style "width" (fmtSideSize amount)
                , style "height" (fmtSideSize amount)
                , class "donation"
                , List.getAt i colors
                    |> Maybe.withDefault Tw.bg_blue_200
                    |> List.singleton
                    |> css
                , css [ Tw.flex, Tw.justify_center, Tw.items_center, Tw.flex_nowrap ]
                ]
                [ text ("$" ++ String.fromFloat amount) ]

        matchBox =
            div
                [ css [ Tw.bg_green_100, Tw.border_l, Tw.border_r, Tw.border_black, Tw.flex, Tw.justify_center ]
                , style "height" "100px"
                ]
                [ text "Match" ]

        nameBox =
            div
                [ css
                    [ Tw.flex
                    , Tw.items_center
                    , Tw.justify_center
                    ]
                ]
                [ text name ]
    in
    List.indexedMap drawDonation donations
        |> div
            [ css
                [ Tw.flex
                , Tw.border_l
                , Tw.border_r
                , Tw.border_t_4
                , Tw.border_black
                ]
            , class "cause"
            ]
        |> (\squares -> [ matchBox, squares, nameBox ])
        |> div [ css [ Tw.flex, Tw.flex_col ] ]


colors =
    [ Tw.bg_blue_50
    , Tw.bg_blue_100
    , Tw.bg_blue_200
    , Tw.bg_blue_300
    , Tw.bg_blue_400
    , Tw.bg_blue_500
    ]


jar amount =
    div [ css [ Tw.flex, Tw.flex_col, Tw.items_center ] ]
        [ img
            [ Html.Styled.Attributes.src "jar.png"
            , style "height" "100px"
            , style "width" "100px"
            ]
            []
        , text (String.fromInt amount)
        ]



{- Writing -}


writing =
    div []
        [ spacer
        , h3 [] [ text "Piano Man donations (just giving)" ]
        , pre [] [ text "And they sit at the bar\nAnd put bread in my jar\nAnd say Man, what are you doing here?\n              -Billy Joel, 'The Piano Man'" ]
        , p [] [ text "This is the simplest form of giving. Billy Joel gets some money, and he buys some bread." ]
        , spacer
        , h3 [] [ text "Matching fund donations" ]
        , p [] [ text "This is a more complicated form of giving. Some larger donor puts up funds on the condition that others pitch in as well - their funds are multiplied by the first donors large donation. Maybe invented by Ben Franklin? Definiately used by Booker T Washington + Henry H Rogers." ]
        , spacer
        , h3 [] [ text "Funding Markets" ]
        , p [] [ text "Kickstrater, indiegogo, and others made a big splash with inernet money funding. Patreon makes recurring which is powerful in habit making." ]
        , spacer
        , h3 [] [ text "Proportional funding " ]
        , p [] [ text "A way to mix the matching funds with funding markets is to have the matching funds given to a whole market of projects in proportion to how they are funded. This is a thing in the UK." ]
        , spacer
        , h3 [] [ text "Quadratic Funding" ]
        , p [] [ text "Here you're matching donations by their square root proportions. That means 5 people giving one dollar get more matching than one person giving 5 dollars. However, this is vulnerable to people making multiple accounts and pretending to be 5 people!" ]
        , spacer
        , h3 [] [ text "Passported Quadratic Funding" ]
        , p [] [ text "Using the Gitcion Passport, we can scale the impact someone's donation has based on how sure we are they are not part of a sybil attack. Typically, a sybil attack looks like hacker1@gmail.com, hacker2@gmail.com etc... Here, by linking accounts to Twitter, BrightId, ProofOfHumanity, etc we can reward people for keeping the system honest with extra say in where funds go." ]
        ]
