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
    , donors : List Donor
    , match : Float
    }


init : ( Model, Cmd Msg )
init =
    ( { height = 0
      , width = 0
      , stageWidth = 0
      , donors = quadExample1Donors
      , match = 10
      }
    , Task.attempt GotViewport Dom.getViewport
    )


type Msg
    = GotResize Int Int
    | GotViewport (Result Dom.Error Dom.Viewport)
    | GotStageSize (Result Dom.Error Dom.Viewport)
    | NewViz (List Donor) Float


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

        NewViz donors match ->
            ( { model | donors = donors, match = match }, Cmd.none )


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


halfSpacer =
    div [ style "height" "7em" ] []


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
        [ drawRound model.stageWidth model.donors model.match
        , drawDonors model.donors
        ]


type alias Donor =
    { name : String
    , donationsByCause : Dict String Float
    , bonusByPassportSource : Dict String Float
    }


exampleDonors =
    [ { name = "Khadija"
      , donationsByCause =
            Dict.fromList
                [ ( "Piano player", 5 )
                , ( "Piano podcast", 1 )
                , ( "MuseScore Open Source Project", 10 )
                , ( "Solar Coffee Research", 100 )
                ]
      , bonusByPassportSource = Dict.fromList [ ( "Assumed", 1 ) ]
      }
    , { name = "Rachael"
      , donationsByCause =
            Dict.fromList
                [ ( "Piano player", 5 )
                , ( "Piano podcast", 1 )
                , ( "MuseScore Open Source Project", 1 )
                , ( "Solar Coffee Research", 10 )
                ]
      , bonusByPassportSource = Dict.fromList [ ( "Assumed", 1 ) ]
      }
    , { name = "Sarah"
      , donationsByCause =
            Dict.fromList
                [ ( "Piano player", 5 )
                , ( "Piano podcast", 4 )
                , ( "Solar Coffee Research", 10 )
                ]
      , bonusByPassportSource = Dict.fromList [ ( "Assumed", 1 ) ]
      }
    ]


type alias Donation =
    { bonusRatio : Float
    , color : String
    , amount : Float
    }


drawRound widthOfRound donors totalMatch =
    let
        pixelsPerRootDollar =
            widthOfRound / weightedRootSumAllDonations

        allCauses =
            donors
                |> List.map .donationsByCause
                |> List.concatMap Dict.keys
                |> Set.fromList
                |> Set.toList

        weightedRootSumOfDonations causeName =
            {- Weights b/c passport bonus scores
               root b/c quadratic
               sum b/c matching
            -}
            donors
                |> List.filterMap
                    (\donor ->
                        Dict.get causeName donor.donationsByCause
                            |> Maybe.map sqrt
                            |> Maybe.map2 (*) (donorBonus donor)
                    )
                |> List.sum

        donations causeName =
            donors
                |> List.indexedMap
                    (\i donor ->
                        Dict.get causeName donor.donationsByCause
                            |> Maybe.map3 Donation (donorBonus donor) (List.getAt i colors)
                    )
                |> List.filterMap identity

        donorBonus donor =
            donor.bonusByPassportSource
                |> Dict.toList
                |> List.map Tuple.second
                |> List.sum
                |> Just

        weightedRootSumAllDonations =
            List.map weightedRootSumOfDonations allCauses
                |> List.sum
    in
    allCauses
        |> List.map
            (\cause ->
                drawCause
                    { name = cause
                    , pixelsPerRootDollar = pixelsPerRootDollar
                    , causeMatch = weightedRootSumOfDonations cause / weightedRootSumAllDonations * totalMatch
                    , donations = donations cause
                    }
            )
        |> div [ css [ Tw.flex ], class "allCauses" ]


type alias CauseArguments =
    { name : String
    , causeMatch : Float
    , pixelsPerRootDollar : Float
    , donations : List Donation
    }


drawCause : CauseArguments -> Html Msg
drawCause { name, pixelsPerRootDollar, donations, causeMatch } =
    let
        {- Views -}
        drawDonation i donation =
            div
                [ style "width" (fmtWidthSize donation)
                , style "height" (fmtHeightSize donation)
                , class "donation"
                , css
                    [ Tw.flex
                    , Tw.justify_center
                    , Tw.items_center
                    , Tw.flex_nowrap
                    ]
                , style "background-color" donation.color
                ]
                [ text ("$" ++ String.fromFloat donation.amount) ]

        matchBox =
            if causeMatch > 0 then
                div
                    [ css [ Tw.bg_green_200, Tw.border_l, Tw.border_r, Tw.border_black, Tw.flex, Tw.justify_center ]
                    , style "height" (String.fromFloat matchHeight ++ "px")
                    ]
                    [ text ("Match: $" ++ String.fromInt (round causeMatch)) ]

            else
                text ""

        nameBox =
            div
                [ css
                    [ Tw.flex
                    , Tw.items_center
                    , Tw.justify_center
                    ]
                ]
                [ text name ]

        totalBox =
            div
                [ css
                    [ Tw.flex
                    , Tw.items_center
                    , Tw.justify_center
                    , Tw.font_bold
                    , Tw.pb_2
                    ]
                ]
                [ text ("Total: $" ++ String.fromInt totalFunding) ]

        fmtWidthSize donation =
            widthSizeInPixels donation
                |> String.fromFloat
                |> (\x -> x ++ "px")

        fmtHeightSize donation =
            heightSizeInPixels donation
                |> String.fromFloat
                |> (\x -> x ++ "px")

        {- Math -}
        widthSizeInPixels { bonusRatio, amount } =
            sqrt amount * bonusRatio * pixelsPerRootDollar

        heightSizeInPixels { bonusRatio, amount } =
            sqrt amount / bonusRatio * pixelsPerRootDollar

        matchHeight =
            causeMatch / donationTotal * pixelsPerRootDollar

        donationTotal =
            List.map2 Tuple.pair
                (List.map .amount donations)
                (List.map .bonusRatio donations)
                |> List.map (\( amount, bonus ) -> sqrt amount * bonus)
                |> List.sum

        totalFunding =
            round (List.sum (List.map .amount donations) + causeMatch)

        maxHeight =
            400
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
        |> (\squares -> [ totalBox, matchBox, squares, nameBox ])
        |> div [ css [ Tw.flex, Tw.flex_col ] ]


colors =
    -- Thanks,  http://vrl.cs.brown.edu/color !
    [ "rgb(197,213,240)"
    , "rgb(65,201,220)"
    , "rgb(208,168,249)"
    , "rgb(254,183,134)"
    , "rgb(148,166,253)"
    , "rgb(250,65,199)"
    , "rgb(226,209,203)"
    , "rgb(253,89,37)"
    , "rgb(251,159,168)"
    , "rgb(250,85,122)"
    , "rgb(228,135,57)"
    , "rgb(154,142,145)"
    ]
        ++ [ "rgb(197,213,240)"
           , "rgb(65,201,220)"
           , "rgb(208,168,249)"
           , "rgb(254,183,134)"
           , "rgb(148,166,253)"
           , "rgb(250,65,199)"
           , "rgb(226,209,203)"
           , "rgb(253,89,37)"
           , "rgb(251,159,168)"
           , "rgb(250,85,122)"
           , "rgb(228,135,57)"
           , "rgb(154,142,145)"
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


drawDonors donors =
    let
        drawDonor i donor =
            List.getAt i colors
                |> Maybe.map
                    (\color ->
                        div
                            [ css
                                [ Tw.flex
                                , Tw.justify_center
                                , Tw.items_center
                                , Tw.p_4
                                ]
                            ]
                            [ div
                                [ css
                                    [ Tw.w_4
                                    , Tw.h_4
                                    , Tw.mr_2
                                    ]
                                , style "background-color" color
                                ]
                                []
                            , text donor.name
                            ]
                    )
                |> Maybe.withDefault (text "")
    in
    div [ css [ Tw.prose ] ]
        [ h2 [] [ text "Donors" ]
        , div [ css [ Tw.flex, Tw.flex_wrap ] ]
            (List.indexedMap drawDonor donors)
        ]



{- Writing -}


writing =
    div []
        [ h1 [] [ text "A Visual Guide to Funding Mechanisms" ]
        , halfSpacer
        , h2 [] [ text "How Anti-Sybil Quadratic Funding saved a small town from having to watch Hamlet *again*" ]
        , p []
            [ text "Daniel works for small town's City Council deciding how to spend their art budget. Every year this town has a theater festival in the park, and it's Daniel's job to keep everyone happy." ]
        , p [] [ text "For years, the budget went to free food for people who watched the shows. A wealthy donor in the community was happy to fund the actors and costumes and musicians as long as they put on his favorite play - Shakespeare's Hamlet. Other shows happened too, but they rarely could pay their actors or afford costumes." ]

        --, drawRound 500 hamletDonors 0
        , halfSpacer
        , pre [] [ text "⚠️Unmatched funding visualization unfinished⚠️" ]
        , button [ css [ Tw.border, Tw.p_1, Tw.rounded, Tw.bg_gray_50 ] ] [ text "<- Replace interactive viz " ]
        , halfSpacer

        -- Why not vote for which shows get funds?
        -- , p [] [ text "Daniel began to hear complaints about the festival, and so he began looking for improvements. His first idea was to have people vote on which shows would get funding. " ]
        , p [] [ text "Daniel began to hear complaints about the festival, and so he began looking for improvements. Inspired by websites like kickstarter and indiegogo, he made a website for his town where people could donate to upcoming shows. This helped a bit - some people gave money now that it was easier to hear about the shows in one place and donate. But most of the shows still didn't have much funding. People were so sick of Hamlet that they weren't even showing up for the free food anymore." ]
        , halfSpacer
        , pre [] [ text "⚠️Unmatched funding visualization unfinished⚠️" ]
        , button [ css [ Tw.border, Tw.p_1, Tw.rounded, Tw.bg_gray_50 ] ] [ text "<- Replace interactive viz " ]
        , halfSpacer
        , p [] [ text "The next year, Daniel decided some of the city's funds would match those of donors on the website. This generated a lot of interest! Many new projects were added to the website, and some seemed like they would have enough funding to hire good musicians." ]
        , p [] [ text "When the results came in though, it turned out the wealthy donor had taken most of the funds since his donation to Hamlet outweighed all other donations. Many townsfolk were angry, and they told Daniel to not give any funds to the Hamlet productio. He looked into this, but it would have broken city laws about showing no preference between opinions of different citizens." ]

        -- Why not cap linear donations?
        , halfSpacer
        , pre [] [ text "⚠️Linear matched funding visualization unfinished⚠️" ]
        , button [ css [ Tw.border, Tw.p_1, Tw.rounded, Tw.bg_gray_50 ] ] [ text "<- Replace interactive viz " ]
        , halfSpacer
        , p [] [ text "The next year, Daniel researched credibly neutral ways to blend democratic structures and free markets. He read about Quadratic Funding, where every donation is matched, but not directly. If one person donated 4 dollars, it would get matched half as much as 4 people donating 1 dollar." ]
        , halfSpacer
        , drawRound 400 quadExample1Donors 10
        , button [ css [ Tw.border, Tw.p_1, Tw.rounded, Tw.bg_gray_50 ], onClick (NewViz quadExample1Donors 10) ] [ text "<- Replace interactive viz " ]
        , p [] [ text "He had to draw pictures to explain this to people, but they got the idea pretty quickly and were happy with the results." ]
        , halfSpacer
        , p [] [ text "The next year, Daniel noticed a problem. There were more people donating than lived in his city, and the projects they donated to were from out of town. Some out-of-town projects were getting the lion's share of the matched funding, with many more participants than other projects. Daniel looked closely at the donors and found they all had very similar emails - catsFan1@gmail.com, catsFan2@gmail.com, catsFan3@gmail.com... There were hundreds of accounts just like this." ]
        , halfSpacer
        , drawRound 400 quadExample2Donors 100
        , button [ css [ Tw.border, Tw.p_1, Tw.rounded, Tw.bg_gray_50 ], onClick (NewViz quadExample2Donors 100) ] [ text "<- Replace interactive viz " ]
        , p [] [ text "Daniel made rules that kept out email addresses that were too similar, but he found the 'catsFan' character could always come up with more emails. He tried to require a Social Security number to use the website, but got push back from users who said that was too invasive." ]
        , halfSpacer
        , p [] [ text "Through more research, Daniel heard of the Gitcoin Passport project, which would prevent this kind of abuse. Donors like 'catsFan' would still be able to donate, but if they wanted access to the matching funds, they would have to link their account to others which increased the likelihood they were just one person." ]
        , drawRound 400 quadExample3Donors 50
        , button [ css [ Tw.border, Tw.p_1, Tw.rounded, Tw.bg_gray_50 ], onClick (NewViz quadExample3Donors 5) ] [ text "<- Replace interactive viz " ]
        , p [] [ text "By adding options for social media accounts which have expensive teams to prevent multiple accounts, most people could still use the platform. 'CatsFan' was limited to their one account, however. And they were even able to give a bonus to users who were verified residents of the city, rather than outsiders." ]
        ]


writing2 =
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
        , p [] [ text "Here you're matching donations by their square root proportions. That means four people giving one dollar each to project A get more matching funds than one person giving four dollars to project B. " ]
        , drawRound 300 hamiltonDonors 10
        , p [] [ text "However, this is vulnerable to people making multiple accounts and pretending to be 4 people!" ]
        , spacer
        , h3 [] [ text "Passported Quadratic Funding" ]
        , p [] [ text "Using the Gitcion Passport, we can scale the impact someone's donation has based on how sure we are they are not part of a sybil attack. Typically, a sybil attack looks like hacker1@gmail.com, hacker2@gmail.com etc... Here, by linking accounts to Twitter, BrightId, ProofOfHumanity, etc we can reward people for keeping the system honest with extra say in where funds go." ]
        ]


hamiltonDonors =
    [ { name = "d6"
      , donationsByCause = Dict.fromList [ ( "Opera", 4 ) ]
      , bonusByPassportSource = Dict.fromList [ ( "assumed", 1 ) ]
      }
    , { name = "d5"
      , donationsByCause = Dict.fromList [ ( "Basic Music Education", 1 ) ]
      , bonusByPassportSource = Dict.fromList [ ( "assumed", 1 ) ]
      }
    , { name = "d4"
      , donationsByCause = Dict.fromList [ ( "Basic Music Education", 1 ) ]
      , bonusByPassportSource = Dict.fromList [ ( "assumed", 1 ) ]
      }
    , { name = "d3"
      , donationsByCause = Dict.fromList [ ( "Basic Music Education", 1 ) ]
      , bonusByPassportSource = Dict.fromList [ ( "assumed", 1 ) ]
      }
    , { name = "d1"
      , donationsByCause = Dict.fromList [ ( "Basic Music Education", 1 ) ]
      , bonusByPassportSource = Dict.fromList [ ( "assumed", 1 ) ]
      }
    ]


hamletDonors =
    [ { name = "d6"
      , donationsByCause = Dict.fromList [ ( "Hamlet", 1000 ) ]
      , bonusByPassportSource = Dict.fromList [ ( "assumed", 1 ) ]
      }
    , { name = "d3"
      , donationsByCause = Dict.fromList [ ( "School of Rock", 50 ) ]
      , bonusByPassportSource = Dict.fromList [ ( "assumed", 1 ) ]
      }
    , { name = "d1"
      , donationsByCause = Dict.fromList [ ( "Hamilton", 75 ) ]
      , bonusByPassportSource = Dict.fromList [ ( "assumed", 1 ) ]
      }
    ]


quadExample1Donors =
    [ { name = "d6"
      , donationsByCause = Dict.fromList [ ( "Hamlet", 4 ) ]
      , bonusByPassportSource = Dict.fromList [ ( "assumed", 1 ) ]
      }
    , { name = "d5"
      , donationsByCause = Dict.fromList [ ( "Hamilton", 1 ) ]
      , bonusByPassportSource = Dict.fromList [ ( "assumed", 1 ) ]
      }
    , { name = "d4"
      , donationsByCause = Dict.fromList [ ( "Hamilton", 1 ) ]
      , bonusByPassportSource = Dict.fromList [ ( "assumed", 1 ) ]
      }
    , { name = "d3"
      , donationsByCause = Dict.fromList [ ( "Hamilton", 1 ) ]
      , bonusByPassportSource = Dict.fromList [ ( "assumed", 1 ) ]
      }
    , { name = "d1"
      , donationsByCause = Dict.fromList [ ( "Hamilton", 1 ) ]
      , bonusByPassportSource = Dict.fromList [ ( "assumed", 1 ) ]
      }
    ]


quadExample2Donors =
    [ { name = "16"
      , donationsByCause = Dict.fromList [ ( "Cats", 1 ) ]
      , bonusByPassportSource = Dict.fromList [ ( "assumed", 1 ) ]
      }
    , { name = "26"
      , donationsByCause = Dict.fromList [ ( "Cats", 1 ) ]
      , bonusByPassportSource = Dict.fromList [ ( "assumed", 1 ) ]
      }
    , { name = "36"
      , donationsByCause = Dict.fromList [ ( "Cats", 1 ) ]
      , bonusByPassportSource = Dict.fromList [ ( "assumed", 1 ) ]
      }
    , { name = "46"
      , donationsByCause = Dict.fromList [ ( "Cats", 1 ) ]
      , bonusByPassportSource = Dict.fromList [ ( "assumed", 1 ) ]
      }
    , { name = "5a"
      , donationsByCause = Dict.fromList [ ( "Cats", 1 ) ]
      , bonusByPassportSource = Dict.fromList [ ( "assumed", 1 ) ]
      }
    , { name = "5b"
      , donationsByCause = Dict.fromList [ ( "Cats", 1 ) ]
      , bonusByPassportSource = Dict.fromList [ ( "assumed", 1 ) ]
      }
    , { name = "5c"
      , donationsByCause = Dict.fromList [ ( "Cats", 1 ) ]
      , bonusByPassportSource = Dict.fromList [ ( "assumed", 1 ) ]
      }
    , { name = "5d"
      , donationsByCause = Dict.fromList [ ( "Cats", 1 ) ]
      , bonusByPassportSource = Dict.fromList [ ( "assumed", 1 ) ]
      }
    , { name = "5e"
      , donationsByCause = Dict.fromList [ ( "Cats", 1 ) ]
      , bonusByPassportSource = Dict.fromList [ ( "assumed", 1 ) ]
      }
    , { name = "5f"
      , donationsByCause = Dict.fromList [ ( "Cats", 1 ) ]
      , bonusByPassportSource = Dict.fromList [ ( "assumed", 1 ) ]
      }
    , { name = "d6"
      , donationsByCause = Dict.fromList [ ( "Cats", 1 ) ]
      , bonusByPassportSource = Dict.fromList [ ( "assumed", 1 ) ]
      }
    , { name = "d5"
      , donationsByCause = Dict.fromList [ ( "Hamilton", 1 ) ]
      , bonusByPassportSource = Dict.fromList [ ( "assumed", 1 ) ]
      }
    , { name = "d4"
      , donationsByCause = Dict.fromList [ ( "Hamilton", 1 ) ]
      , bonusByPassportSource = Dict.fromList [ ( "assumed", 1 ) ]
      }
    , { name = "d3"
      , donationsByCause = Dict.fromList [ ( "Hamilton", 1 ) ]
      , bonusByPassportSource = Dict.fromList [ ( "assumed", 1 ) ]
      }
    , { name = "d1"
      , donationsByCause = Dict.fromList [ ( "Hamilton", 1 ) ]
      , bonusByPassportSource = Dict.fromList [ ( "assumed", 1 ) ]
      }
    ]


quadExample3Donors =
    [ { name = "catsFan1@gmail.com"
      , donationsByCause = Dict.fromList [ ( "Cats", 1 ) ]
      , bonusByPassportSource = Dict.fromList [ ( "assumed", 0.5 ) ]
      }
    , { name = "catsFan2@gmail.com"
      , donationsByCause = Dict.fromList [ ( "Cats", 1 ) ]
      , bonusByPassportSource = Dict.fromList [ ( "assumed", 0.5 ) ]
      }
    , { name = "catsFan3@gmail.com"
      , donationsByCause = Dict.fromList [ ( "Cats", 1 ) ]
      , bonusByPassportSource = Dict.fromList [ ( "assumed", 0.5 ) ]
      }
    , { name = "catsfan5@gmail.com"
      , donationsByCause = Dict.fromList [ ( "Cats", 1 ) ]
      , bonusByPassportSource = Dict.fromList [ ( "assumed", 0.5 ) ]
      }
    , { name = "catsfan6@gmail.com"
      , donationsByCause = Dict.fromList [ ( "Cats", 1 ) ]
      , bonusByPassportSource = Dict.fromList [ ( "assumed", 0.5 ) ]
      }
    , { name = "catsfan7@gmail.com"
      , donationsByCause = Dict.fromList [ ( "Cats", 1 ) ]
      , bonusByPassportSource = Dict.fromList [ ( "assumed", 0.5 ) ]
      }
    , { name = "catsfan8@gmail.com"
      , donationsByCause = Dict.fromList [ ( "Cats", 1 ) ]
      , bonusByPassportSource = Dict.fromList [ ( "assumed", 0.5 ) ]
      }
    , { name = "catsfan9@gmail.com"
      , donationsByCause = Dict.fromList [ ( "Cats", 1 ) ]
      , bonusByPassportSource = Dict.fromList [ ( "assumed", 0.5 ) ]
      }
    , { name = "catsfan10@gmail.com"
      , donationsByCause = Dict.fromList [ ( "Cats", 1 ) ]
      , bonusByPassportSource = Dict.fromList [ ( "assumed", 0.5 ) ]
      }
    , { name = "catsfan11@gmail.com"
      , donationsByCause = Dict.fromList [ ( "Cats", 1 ) ]
      , bonusByPassportSource = Dict.fromList [ ( "assumed", 0.5 ) ]
      }
    , { name = "catsfan12@gmail.com"
      , donationsByCause = Dict.fromList [ ( "Cats", 1 ) ]
      , bonusByPassportSource = Dict.fromList [ ( "assumed", 0.5 ) ]
      }
    , { name = "citizen1"
      , donationsByCause = Dict.fromList [ ( "Hamilton", 1 ) ]
      , bonusByPassportSource = Dict.fromList [ ( "assumed", 0.5 ) ]
      }
    , { name = "citizen2"
      , donationsByCause = Dict.fromList [ ( "Hamilton", 1 ) ]
      , bonusByPassportSource = Dict.fromList [ ( "assumed", 0.5 ), ( "person", 0.9 ) ]
      }
    , { name = "citizen4"
      , donationsByCause = Dict.fromList [ ( "Hamilton", 1 ) ]
      , bonusByPassportSource = Dict.fromList [ ( "assumed", 0.5 ), ( "person", 0.9 ), ( "citizen", 0.15 ) ]
      }
    , { name = "citizen3"
      , donationsByCause = Dict.fromList [ ( "Hamilton", 1 ) ]
      , bonusByPassportSource = Dict.fromList [ ( "assumed", 0.5 ) ]
      }
    ]
