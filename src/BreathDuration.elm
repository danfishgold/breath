module BreathDuration exposing
    ( BreathDuration
    , Duration(..)
    , addTrainingIn
    , addTrainingOut
    , doneTraining
    , durationIn
    , durationOut
    , initial
    , temporarilyUnsetDurationIn
    )


type Duration
    = Known Float
    | Unknown Float


type BreathDuration
    = BreathDuration
        { trainingIn : List Float
        , trainingOut : List Float
        , durationIn : Duration
        , durationOut : Duration
        }


initial : Float -> Float -> BreathDuration
initial inHalfLife outHalfLife =
    BreathDuration
        { trainingIn = []
        , trainingOut = []
        , durationIn = Unknown inHalfLife
        , durationOut = Unknown outHalfLife
        }


doneTraining : BreathDuration -> Bool
doneTraining (BreathDuration bd) =
    case ( bd.durationIn, bd.durationOut ) of
        ( Known _, Known _ ) ->
            True

        _ ->
            False


durationIn : BreathDuration -> Duration
durationIn (BreathDuration bd) =
    bd.durationIn


durationOut : BreathDuration -> Duration
durationOut (BreathDuration bd) =
    bd.durationOut


addTrainingIn : Float -> BreathDuration -> BreathDuration
addTrainingIn duration (BreathDuration bd) =
    let
        newTraining =
            duration :: bd.trainingIn

        averageDuration =
            calculateAverageDuration newTraining
    in
    BreathDuration
        { bd
            | trainingIn = newTraining
            , durationIn =
                if List.length newTraining < 4 then
                    Unknown averageDuration

                else
                    Known averageDuration
        }


temporarilyUnsetDurationIn : BreathDuration -> BreathDuration
temporarilyUnsetDurationIn (BreathDuration bd) =
    BreathDuration
        { bd
            | durationIn =
                case bd.durationIn of
                    Known duration ->
                        Unknown duration

                    Unknown duration ->
                        Unknown duration
        }


addTrainingOut : Float -> BreathDuration -> BreathDuration
addTrainingOut duration (BreathDuration bd) =
    let
        newTraining =
            duration :: bd.trainingOut

        averageDuration =
            calculateAverageDuration newTraining
    in
    BreathDuration
        { bd
            | trainingOut = newTraining
            , durationOut =
                if List.length newTraining < 3 then
                    Unknown averageDuration

                else
                    Known averageDuration
        }


calculateAverageDuration : List Float -> Float
calculateAverageDuration durations =
    weightedAverage durations


weightedAverage : List Float -> Float
weightedAverage items =
    let
        n =
            List.length items

        weight idx =
            toFloat (n - idx) / toFloat n

        weights =
            List.map weight (List.range 0 n)
    in
    List.sum (List.map2 (*) items weights) / List.sum weights
