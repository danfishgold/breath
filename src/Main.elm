module Main exposing (main)

import BreathDuration exposing (BreathDuration, Duration(..))
import Browser
import Browser.Events
import Html exposing (Html, button, div, h1, p, span, text)
import Html.Attributes exposing (style)
import Html.Events exposing (onMouseDown, onMouseUp)


main : Program Flags Model Msg
main =
    Browser.document
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }


type alias Flags =
    {}


type alias Model =
    { deviceType : InputDevice
    , currentBreath : Maybe Breath
    , breathDuration : BreathDuration
    , training : Training
    , animationStart : Float
    , animationEnds : { in_ : Float, out_ : Float }
    }


type alias AnimationBounds =
    { start : Float, end : Float }


type Training
    = NotTraining
    | TrainingIn
    | PotentiallyTrainingOut


type Breath
    = In Float
    | Out Float


type InputDevice
    = Touch
    | Mouse


type Msg
    = Tick Float
    | ButtonHoldStarted
    | ButtonHoldStopped


init : Flags -> ( Model, Cmd Msg )
init _ =
    ( { deviceType = Mouse
      , currentBreath = Nothing
      , breathDuration = BreathDuration.initial 3000 3000
      , training = NotTraining
      , animationStart = 200
      , animationEnds = { in_ = 500, out_ = 200 }
      }
    , Cmd.none
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Tick dt ->
            case model.currentBreath of
                Nothing ->
                    ( model, Cmd.none )

                Just currentBreath ->
                    let
                        newBreath =
                            addTimeDelta model.training model.breathDuration dt currentBreath
                    in
                    ( { model
                        | currentBreath = Just newBreath
                        , training =
                            if model.training == PotentiallyTrainingOut then
                                let
                                    isStillPotentiallyTrainingOut =
                                        case ( currentBreath, newBreath ) of
                                            ( Out _, Out _ ) ->
                                                True

                                            ( Out _, In duration ) ->
                                                duration < 1000

                                            ( In _, In duration ) ->
                                                duration < 1000

                                            _ ->
                                                False
                                in
                                if isStillPotentiallyTrainingOut then
                                    PotentiallyTrainingOut

                                else
                                    NotTraining

                            else
                                model.training
                      }
                        |> updateAnimationBoundsIfNeeded model
                    , Cmd.none
                    )

        ButtonHoldStarted ->
            let
                maybeAddOutTraining =
                    case model.currentBreath of
                        Just (Out duration) ->
                            if model.training == PotentiallyTrainingOut then
                                BreathDuration.addTrainingOut duration model.breathDuration

                            else
                                model.breathDuration

                        Just (In duration) ->
                            if model.training == PotentiallyTrainingOut then
                                case BreathDuration.durationOut model.breathDuration of
                                    Known outDuration ->
                                        BreathDuration.addTrainingOut (outDuration + duration) model.breathDuration

                                    Unknown _ ->
                                        model.breathDuration

                            else
                                model.breathDuration

                        _ ->
                            model.breathDuration
            in
            ( { model
                | currentBreath = Just (In 0)
                , training = TrainingIn
                , breathDuration =
                    maybeAddOutTraining
                        |> BreathDuration.temporarilyUnsetDurationIn
              }
                |> updateAnimationBoundsIfNeeded model
            , Cmd.none
            )

        ButtonHoldStopped ->
            ( { model
                | currentBreath = Just (Out 0)
                , training = PotentiallyTrainingOut
                , breathDuration =
                    case model.currentBreath of
                        Just (In duration) ->
                            BreathDuration.addTrainingIn duration model.breathDuration

                        _ ->
                            model.breathDuration
              }
                |> updateAnimationBoundsIfNeeded model
            , Cmd.none
            )


updateAnimationBoundsIfNeeded : Model -> Model -> Model
updateAnimationBoundsIfNeeded oldModel newModel =
    let
        shouldUpdate =
            case ( oldModel.currentBreath, newModel.currentBreath ) of
                ( Just (In _), Just (Out _) ) ->
                    True

                ( Just (Out _), Just (In _) ) ->
                    True

                _ ->
                    (newModel.training == TrainingIn)
                        && (oldModel.training /= TrainingIn)
    in
    if shouldUpdate then
        { newModel | animationStart = animationValue oldModel }

    else
        newModel


addTimeDelta : Training -> BreathDuration -> Float -> Breath -> Breath
addTimeDelta training breathDuration dt currentBreath =
    case currentBreath of
        In duration ->
            case BreathDuration.durationIn breathDuration of
                Unknown _ ->
                    In (duration + dt)

                Known maxDuration ->
                    if duration + dt > maxDuration && training /= TrainingIn then
                        Out (duration + dt - maxDuration)

                    else
                        In (duration + dt)

        Out duration ->
            case BreathDuration.durationOut breathDuration of
                Unknown _ ->
                    Out (duration + dt)

                Known maxDuration ->
                    if duration + dt > maxDuration then
                        In (duration + dt - maxDuration)

                    else
                        Out (duration + dt)


view : Model -> Browser.Document Msg
view model =
    { title = "Please Breath"
    , body = body model
    }


body : Model -> List (Html Msg)
body model =
    let
        width =
            animationValue model
    in
    [ h1 [] [ text "Hello. Please breath" ]
    , p []
        [ text "Click and hold the button as you inhale. "
        , text "Release it as you breath out. "
        , text "Breathe as deep as you like. "
        , text "After the fourth breath the system will learn your breathing pattern."
        ]
    , div []
        [ button
            [ onMouseDown ButtonHoldStarted
            , onMouseUp ButtonHoldStopped
            ]
            [ text "boop" ]
        ]
    , div
        [ style "height" "200px"
        , style "width" (String.fromFloat width ++ "px")
        , style "background-color" "lightblue"
        ]
        []
    ]


animationValue : Model -> Float
animationValue model =
    case model.currentBreath of
        Nothing ->
            model.animationStart

        Just (In duration) ->
            interpolateDuration
                model.animationStart
                model.animationEnds.in_
                (BreathDuration.durationIn model.breathDuration)
                duration

        Just (Out duration) ->
            interpolateDuration
                model.animationStart
                model.animationEnds.out_
                (BreathDuration.durationOut model.breathDuration)
                duration


interpolateDuration : Float -> Float -> Duration -> Float -> Float
interpolateDuration from to duration currentDuration =
    let
        f =
            case duration of
                Known totalDuration ->
                    (1 - cos (currentDuration / totalDuration * pi)) / 2

                Unknown halfLife ->
                    (1 - e ^ (-3 * currentDuration / halfLife)) ^ 3
    in
    from + (to - from) * f


subscriptions : Model -> Sub Msg
subscriptions model =
    if model.currentBreath /= Nothing then
        Browser.Events.onAnimationFrameDelta Tick

    else
        Sub.none
