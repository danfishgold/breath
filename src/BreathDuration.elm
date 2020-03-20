module BreathDuration exposing
    ( BreathDuration
    , Duration(..)
    , addTrainingIn
    , addTrainingOut
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
    in
    BreathDuration
        { bd
            | trainingIn = newTraining
            , durationIn =
                case calculateDuration 4 newTraining of
                    Nothing ->
                        bd.durationIn

                    Just averageDuration ->
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
    in
    BreathDuration
        { bd
            | trainingOut = newTraining
            , durationOut =
                case calculateDuration 3 newTraining of
                    Nothing ->
                        bd.durationOut

                    Just averageDuration ->
                        Known averageDuration
        }


calculateDuration : Int -> List Float -> Maybe Float
calculateDuration minimumTrainings durations =
    if List.length durations < minimumTrainings then
        Nothing

    else
        Just (weightedAverage durations)


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