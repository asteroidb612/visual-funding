module Main exposing (main)

import Browser
import Browser.Events
import Color
import Css
import Css.Global
import Html
import Html.Events
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (css, style)
import Html.Styled.Events exposing (onClick)
import List.Extra as List
import Tailwind.Utilities as Tw


type alias Model =
    { count : Int
    , width : Float
    }


init : ( Model, Cmd Msg )
init =
    ( { count = 0
      , width = 500
      }
    , Cmd.none
    )


type Msg
    = Increment
    | Decrement


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Increment ->
            ( { model | count = model.count + 1 }, Cmd.none )

        Decrement ->
            ( { model | count = model.count - 1 }, Cmd.none )


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
            ]
            [ ractive model ]
        , div
            [ style "width" "50%"
            , style "height" "90vh"
            , css
                [ Tw.prose
                , Tw.overflow_scroll
                ]
            ]
            [ p [] [ text "And they sit at the bar" ]
            , p [] [ text "And put bread in my jar" ]
            , p [] [ text "And say Man, what are you doing here?" ]
            , p [] [ text """
County judge[edit]

Brady moved to El Paso in 1915.[3] He joined the law firm of Stafford and Mahan, which renamed itself Stafford, Brady & Mahan and moved its offices to the First National Bank building.[160][161][note 7] In 1917 the El Paso County Court at Law was created, and Brady was chosen by Governor James E. Ferguson from among three candidates for the judgeship.[165][166][note 8] The court had jurisdiction over civil and criminal matters; the preexisting County Court of El Paso County retained jurisdiction as a juvenile and probate court, among other matters.[170][171][note 9] Brady inherited a $2,500 salary (equivalent to $67,000 in 2021) and a voluminous docket, with some 3,000 civil and 800 criminal cases pending.[178][179] The first session of the court lasted the first four weeks of July,[180] and saw numerous cases: among them for selling intoxicating liquors – Brady's first jury case as a judge[181] – false imprisonment,[182] carrying a pistol, nonsupport,[183] stealing lumber,[184] and negligent homicide.[185][note 10] Brady was given two weeks vacation by the county commissioners at the end,[188][189] spending it at Cloudcroft, New Mexico;[190] he placed his newly built home to let until September,[191] when the fall term convened.[192] The new docket included some 50 liquor cases.[193][note 11] Brady also heard cases in November and December 1917.[207][208][note 12]

Brady was up for election in 1918,[212] the first time his office would appear on the ballot. During the race he voiced his support for prohibition,[213] and took out a half-page newspaper advertisement, along with county attorney William H. Fryer[214] and judge Walter D. Howe[215] of the thirty-fourth judicial district, calling it a lie to declare that licensed liquor dealers were uniformly law abiding.[216][217] Brady introduced Fryer, county judge Edward B. McClintock,[218] judge Dan M. Jackson,[219] and Tom Lea at one event,[220][221] and at another appeared in support of Texas representative R. Ewing Thomason, a Democrat.[222] Though he spent at least $430 (equivalent to $7,700 in 2021) campaigning,[223] Brady appeared unopposed on the primary ballot[224] – part of an engineering feat by the local Democratic Party, which aimed to reduce distractions by lining up the ticket ahead of time and supporting existing officeholders.[225][226]

Meanwhile, Brady continued hearing cases throughout 1918.[227][228] Early into the January 1918 term, in a case which the El Paso Herald declared attracted "great interest", Anna Reum was convicted of practicing medicine without a license, fined $250, and sentenced to three months in the county jail;[229][230][note 13] the conviction was later reversed due to a legal error by Brady.[246][247][note 14] Brady spent part of the first week of February in Pecos, where he served as special judge for a trial.[249][note 15] He returned in time to oversee a second trial for Reum, who had been rearrested on the same charges while Brady was away;[252] this time she was fined $100 and sentenced to 15 days in jail.[253]

In March 1918 the Texas Legislature passed a bill, introduced by Thomason, giving the El Paso County Court at Law and the County Court of El Paso County concurrent jurisdiction over criminal matters.[254][255] The bill was intended to make the operation of the courts more efficient, with the County Court hearing criminal cases when the County Court at Law was busy.[256] Brady thus continued presiding over some criminal matters,[257] including a March trial in which a police surgeon, John A. Hardy,[258] was found not guilty of hitting a hotel proprietor with a pistol.[259][note 16] In another case, he certified arrest warrants against Theodore Combest[262] and John Martin, respectively the sheriff and county attorney of Cottle County, for conspiring to murder the star witness in an ongoing murder trial.[263] By September, however, following a visit to Los Angeles,[264][265] he and McClintock agreed that Brady would handle civil matters and McClintock criminal;[266] in such a case heard that month, Brady issued a directed verdict in a lawsuit over possession of real property.[267] Brady shut his courtroom during the second week of October, as did all state and federal judges in El Paso, in an effort to slow the spread of the Spanish flu.[268] In December he denied a writ of habeas corpus to Charles Holman, who, at his trial for traffic violations, was held in contempt and ordered confined for 24 hours for using the word "damn" in the courtroom.[269] Brady also had an active docket in June 1919, with at least nine jury trials.[270][271]

Black and white photograph of Brady and others
Oil[edit]

In early 1919 Brady visited Pecos, where he still owned land; upon his return, he told the Herald that all indications suggested oil and gas would be found there.[272] On June 9, 1919, Brady was one of the incorporators of the Cruces Oil Corporation in New Mexico.[273][274][275][276] Toward the middle of the year he began spending time in California, with his wife and children relocating for the summer.[277] Beginning with the July term,[278] temporary judges, including J. G. Highnote[279] and Edgar Williams,[280] were named to take his place.[281][282][283] Brady sold his house for $5,000 in August,[284] returned to El Paso on Monday, September 29, and resigned his judgeship that Friday,[285] in order to take a position in Los Angeles with the Sunshine Oil Corporation,[286][287] a partner of the Cruces Oil Corporation.[288][289][290][note 17] Brady worked as an attorney and director for the company,[296][297] and was eventually joined there by his brother David.[298] Within weeks, Will Brady and Sunshine were advertising leases in the Pecos Valley oil field in California papers, stating that the Laura well in Pecos was "expected to come in with big flow of oil any time".[296]

By November 1919 the Sunshine Oil Corporation claimed 170,000 acres of oil rights, mostly in the Pecos valley, although it held some interests in the Montebello oil fields in California, and in Big Spring, Texas.[299][300] It had at least three wells in various stages of drilling, including the Laura well, and in Ward County the Victory and Leeman wells.[299][300] The company claimed to have raised some $80,000 from the sales of its leases.[299]

Brady was admitted to the California bar in 1925,[301] and soon thereafter represented the Rhoads Oil Producing Company in a lawsuit against the oil producer Barnett Rosenberg; Rhoads claimed damages of $100,000, relating to Rosenberg's abandonment of five wells in Huntington Beach.[302] By 1929, Brady was the secretary of the M. K. & T. Oil Company in Ventura, California.[303][304][note 18] At points in his career Brady also served as an agent for the National Petroleum Finance Corporation,[308] and worked in Santa Barbara as an oil attorney.[4]

Black and white photograph of Brady

1929 photograph of Brady published in The Austin American

In November 1929 Brady's brother John was arrested and tried for drunkenly murdering his mistress in Austin.[309] Brady headed there from California the following day,[309] the first time in four years he had seen his brother.[310] His wife and sister Helen were also present in the courtroom.[311] Brady joined a team of nine lawyers participating in John Brady's defense; with four of the lawyers leading the defense, Brady and the four others served in a more advisory role.[312] Brady suffered a heart attack in early May 1930, and was confined to his hotel room for several days.[313] He was nonetheless present on 20 May, when his brother was convicted and sentenced to three years in prison.[314]

In 1931 federal judge William P. James appointed Brady the receiver for the Yalemont Oil Company, which operated near Santa Barbara.[315]
Later years[edit]
            """ ]
            ]
        ]


main : Program () Model Msg
main =
    Browser.element
        { init = always init
        , view = tailwindViewWrapper
        , update = update
        , subscriptions = always Sub.none
        }



{- Ractive -}


ractive model =
    let
        drawDonation i amount =
            div
                [ style "width" "50px"
                , style "height" "50px"
                , List.getAt i colors
                    |> Maybe.withDefault Tw.bg_blue_200
                    |> List.singleton
                    |> css
                ]
                []

        donations =
            List.indexedMap drawDonation [ 1, 1, 1, 5 ]
    in
    div
        [ css [ Tw.ml_auto, Tw.mr_auto, Tw.absolute ]
        , style "top" "50%"
        , style "transform" "translateY(-50%)"
        ]
        donations


colors =
    [ Tw.bg_blue_50
    , Tw.bg_blue_100
    , Tw.bg_blue_200
    , Tw.bg_blue_300
    , Tw.bg_blue_400
    , Tw.bg_blue_500
    ]
