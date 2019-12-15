module Main exposing (..)

import Animation
import Animation.Messenger
import Browser
import Browser.Dom as Dom
import Dict exposing (Dict)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Set exposing (..)
import Task
import Time exposing (Posix, millisToPosix, posixToMillis)


type alias Anim =
    Animation.Messenger.State Msg


main : Program Flags Model Msg
main =
    Browser.document
        { init = init
        , view = \model -> { title = "Capital Clicker", body = [ view model ] }
        , update = update
        , subscriptions = subscriptions
        }


init : Flags -> ( Model, Cmd Msg )
init flags =
    ( { capital = Dict.empty
      , workers = Dict.empty
      , money = startMoney
      , capitalElements = Dict.empty
      , day = Sunday
      , automationLevel = 1
      }
    , Cmd.none
    )



-- MODEL


type alias Flags =
    { time : Int }


type alias WorkerId =
    Int


type alias CapitalId =
    Int


type alias Model =
    { capital : Dict CapitalId Capital
    , workers : Dict WorkerId Worker
    , money : Int
    , capitalElements : Dict CapitalId Dom.Element
    , day : WeekDay
    , automationLevel : Int
    }


type alias Capital =
    { style : Anim
    , clickable : Bool
    }


type alias Worker =
    { style : Anim
    , canClick : Bool
    , target : Maybe CapitalId
    }


type WeekDay
    = Sunday
    | Monday
    | Tuesday
    | Wednesday
    | Thursday
    | Friday
    | Saturday


nextDay : WeekDay -> WeekDay
nextDay day =
    case day of
        Sunday ->
            Monday

        Monday ->
            Tuesday

        Tuesday ->
            Wednesday

        Wednesday ->
            Thursday

        Thursday ->
            Friday

        Friday ->
            Saturday

        Saturday ->
            Sunday


capitalCost : Model -> Int
capitalCost model =
    20 + 5 * Dict.size model.capital


capitalEarn : Model -> Int
capitalEarn model =
    model.automationLevel * automationAdd model


automationAdd : Model -> Int
automationAdd model =
    10


capitalWait : Model -> Int
capitalWait model =
    750


workerWait : Model -> Int
workerWait model =
    200


workerCost : Model -> Int
workerCost model =
    80 + 5 * Dict.size model.workers


workerUpkeep : Model -> Int
workerUpkeep model =
    10 + Dict.size model.workers


startMoney : Int
startMoney =
    100


automationCost : Model -> Int
automationCost model =
    1000 * model.automationLevel



-- SUBSCRIPTIONS


animationSubscription : Model -> Sub Msg
animationSubscription model =
    let
        capitalStyles =
            Dict.values model.capital
                |> List.map .style

        workerStyles =
            Dict.values model.workers
                |> List.map .style
    in
    Animation.subscription Animate (List.concat [ capitalStyles, workerStyles ])


timeSubscription : Model -> Sub Msg
timeSubscription model =
    Time.every 5000 Tick


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch [ timeSubscription model, animationSubscription model ]


capitalDomId : CapitalId -> String
capitalDomId capitalId =
    "capital-" ++ String.fromInt capitalId


elementPositionCmd : Model -> Cmd Msg
elementPositionCmd model =
    let
        capitalIds =
            Dict.keys model.capital
    in
    Task.sequence
        ((List.map capitalDomId capitalIds
            |> List.map Dom.getElement
         )
            |> List.map2
                (\c t ->
                    t |> Task.andThen (\r -> Task.succeed ( c, r ))
                )
                capitalIds
        )
        |> Task.attempt ElementPositions



-- UPDATE


type Msg
    = Animate Animation.Msg
    | Tick Posix
    | WorkerAvailable WorkerId
    | CapitalReady CapitalId
    | CapitalClick CapitalId
    | AddCapital
    | AddWorker
    | ElementPositions (Result Dom.Error (List ( CapitalId, Dom.Element )))
    | AssignWorker WorkerId
    | AddAutomation



{-
   Need message for getting all worker AND CAPITAL positions
   after getting we then make workers chase the capital

   FIRST LETS DISPLAY WORKER ON SCREEN
-}


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Animate animMsg ->
            let
                updateFn thing =
                    -- Gives us (style, cmd)
                    Animation.Messenger.update animMsg thing.style

                capitalStyles =
                    -- Gives us int -> (style, cmd)
                    Dict.map (\k -> updateFn) model.capital

                updatedCapital =
                    Dict.map
                        (\k c ->
                            { c
                                | style =
                                    Dict.get k capitalStyles
                                        |> Maybe.map Tuple.first
                                        |> Maybe.withDefault c.style
                            }
                        )
                        model.capital

                workerStyles =
                    -- Gives us int -> (style, cmd)
                    Dict.map (\k -> updateFn) model.workers

                updatedWorkers =
                    Dict.map
                        (\k c ->
                            { c
                                | style =
                                    Dict.get k workerStyles
                                        |> Maybe.map Tuple.first
                                        |> Maybe.withDefault c.style
                            }
                        )
                        model.workers
            in
            ( { model
                | capital = updatedCapital
                , workers = updatedWorkers
              }
            , Cmd.batch
                (List.concat
                    [ Dict.values capitalStyles |> List.map Tuple.second
                    , Dict.values workerStyles |> List.map Tuple.second
                    ]
                )
            )

        Tick time ->
            let
                newDay =
                    nextDay model.day

                newMoney =
                    if model.day == Friday then
                        model.money - (Dict.size model.workers * workerUpkeep model)

                    else
                        model.money
            in
            ( { model
                | day = newDay
                , money = newMoney
              }
            , Cmd.none
            )

        WorkerAvailable id ->
            ( { model
                | workers =
                    Dict.update
                        id
                        (Maybe.map (\worker -> { worker | canClick = True }))
                        model.workers
              }
            , Cmd.none
            )

        CapitalReady id ->
            ( { model
                | capital =
                    Dict.update
                        id
                        (Maybe.map (\capital -> { capital | clickable = True }))
                        model.capital
                , money = model.money + capitalEarn model
              }
            , Cmd.none
            )

        CapitalClick id ->
            ( { model
                | capital =
                    Dict.update
                        id
                        (Maybe.map
                            (\capital ->
                                { capital
                                    | clickable = False
                                    , style = capitalClickStyle id capital.style
                                }
                            )
                        )
                        model.capital
              }
            , Cmd.none
            )

        AddCapital ->
            let
                newId =
                    Maybe.withDefault 0 (List.maximum (Dict.keys model.capital)) + 1

                newCapital =
                    { clickable = True, style = initialCapitalStyle }

                newMoney =
                    model.money - capitalCost model

                updatedModel =
                    { model
                        | capital = Dict.insert newId newCapital model.capital
                        , money = newMoney
                    }
            in
            ( { model
                | capital = Dict.insert newId newCapital model.capital
                , money = newMoney
              }
            , elementPositionCmd updatedModel
            )

        AddWorker ->
            let
                newId =
                    Maybe.withDefault 0 (List.maximum (Dict.keys model.workers)) + 1

                newWorker =
                    { style = initialWorkerStyle
                    , canClick = True
                    , target = Nothing
                    }

                newMoney =
                    model.money - workerCost model

                updatedModel =
                    { model
                        | workers = Dict.insert newId newWorker model.workers
                        , money = newMoney
                    }
            in
            ( assignWorker updatedModel newId
            , Cmd.none
            )

        ElementPositions (Ok els) ->
            ( { model
                | capitalElements = Dict.fromList els
              }
            , Cmd.none
            )

        ElementPositions _ ->
            ( model, Cmd.none )

        AssignWorker workerId ->
            ( assignWorker model workerId, Cmd.none )

        AddAutomation ->
            ( { model
                | automationLevel = model.automationLevel + 1
                , money = model.money - automationCost model
              }
            , Cmd.none
            )


updateWorker : Model -> CapitalId -> WorkerId -> Worker -> Worker
updateWorker model capitalId workerId worker =
    let
        capitalElement =
            Dict.get capitalId model.capitalElements
    in
    case capitalElement of
        Nothing ->
            { worker
                | style = workerLazyStyle model workerId worker.style
                , target = Nothing
            }

        Just element ->
            { worker
                | target = Just capitalId
                , style = workerClickStyle model capitalId element workerId worker.style
            }


maybeHasValue : Maybe a -> Bool
maybeHasValue m =
    case m of
        Nothing ->
            False

        Just _ ->
            True


assignWorker : Model -> WorkerId -> Model
assignWorker model workerId =
    let
        { workers, capital, capitalElements } =
            model

        -- There's gotta be a better way to do this
        targetedCapital =
            Dict.filter (\k worker -> maybeHasValue worker.target) workers
                |> Dict.values
                |> List.map .target
                |> List.map (Maybe.withDefault 0)
                |> Set.fromList

        availableCapital =
            Dict.filter (\k v -> v.clickable && not (Set.member k targetedCapital)) model.capital
                |> Dict.keys

        workerUpdate =
            Dict.update
                workerId
                (if not (List.isEmpty availableCapital) && model.day /= Sunday && model.day /= Saturday then
                    Maybe.map2
                        (\capitalId worker -> updateWorker model capitalId workerId worker)
                        (List.head availableCapital)

                 else
                    Maybe.map
                        (\worker ->
                            { worker
                                | style = workerLazyStyle model workerId worker.style
                                , target = Nothing
                            }
                        )
                )
                workers
    in
    { model | workers = workerUpdate }



-- Worker Styles


initialWorkerStyle : Anim
initialWorkerStyle =
    Animation.style
        [ Animation.top (Animation.px 0)
        , Animation.left (Animation.px 0)
        , Animation.scale 1
        ]


workerClickStyle : Model -> CapitalId -> Dom.Element -> WorkerId -> Anim -> Anim
workerClickStyle model id el workerId style =
    let
        targetTop =
            el.element.y + (el.element.height / 2)

        targetLeft =
            el.element.x + (el.element.width / 2)
    in
    Animation.interrupt
        [ Animation.to
            [ Animation.top (Animation.px targetTop)
            , Animation.left (Animation.px targetLeft)
            , Animation.scale 1
            ]
        , Animation.Messenger.send (CapitalClick id)
        , Animation.wait (millisToPosix (workerWait model))
        , Animation.Messenger.send (AssignWorker workerId)
        ]
        style


pos : Int -> a -> List a -> Int
pos p a list =
    case list of
        b :: xs ->
            if b == a then
                p

            else
                pos (p + 1) a xs

        [] ->
            p


lazyPos : Model -> WorkerId -> Int
lazyPos model id =
    Dict.filter (\i worker -> not (maybeHasValue worker.target)) model.workers
        |> Dict.keys
        |> pos 0 id


workerLazyStyle : Model -> WorkerId -> Anim -> Anim
workerLazyStyle model id style =
    Animation.interrupt
        [ Animation.to
            [ Animation.top (Animation.px (40 + toFloat (lazyPos model id * 20)))
            , Animation.left (Animation.px 320)
            ]
        , Animation.loop
            [ Animation.to
                [ Animation.scale 1.1
                ]
            , Animation.to
                [ Animation.scale 0.9
                ]
            , Animation.Messenger.send (AssignWorker id)
            ]
        ]
        style



-- Capital Styles


initialCapitalStyle : Anim
initialCapitalStyle =
    Animation.style [ Animation.width (Animation.percent 100) ]


capitalClickStyle : CapitalId -> Anim -> Anim
capitalClickStyle id style =
    Animation.interrupt
        [ Animation.to [ Animation.width (Animation.percent 0) ]
        , Animation.to [ Animation.width (Animation.percent 100) ]
        , Animation.Messenger.send (CapitalReady id)
        ]
        style



-- VIEW


view : Model -> Html Msg
view model =
    div
        []
        [ div
            []
            [ div [] [ text ("₮" ++ String.fromInt model.money) ]
            , div [] [ text ("+₮" ++ String.fromInt (capitalEarn model) ++ " per click") ]
            ]
        , viewButtons model
        , viewDay model
        , viewAllCapital model
        , viewWorkers model
        ]


dayClass : WeekDay -> WeekDay -> Attribute Msg
dayClass modelDay selectedDay =
    if modelDay == selectedDay then
        class "selected"

    else
        class "not-selected"


viewDay : Model -> Html Msg
viewDay model =
    div
        [ class "calander" ]
        [ div
            [ class "day"
            , dayClass model.day Sunday
            ]
            [ text "Sun" ]
        , div
            [ class "day"
            , dayClass model.day Monday
            ]
            [ text "Mon" ]
        , div
            [ class "day"
            , dayClass model.day Tuesday
            ]
            [ text "Tue" ]
        , div
            [ class "day"
            , dayClass model.day Wednesday
            ]
            [ text "Wed" ]
        , div
            [ class "day"
            , dayClass model.day Thursday
            ]
            [ text "Thu" ]
        , div
            [ class "day"
            , dayClass model.day Friday
            , class "payday"
            ]
            [ text "Fri" ]
        , div
            [ class "day"
            , dayClass model.day Saturday
            ]
            [ text "Sat" ]
        ]


viewWorkers : Model -> Html Msg
viewWorkers model =
    div
        [ class "workers-div" ]
        (Dict.map (viewWorker model) model.workers
            |> Dict.values
        )


viewAllCapital : Model -> Html Msg
viewAllCapital model =
    div [ class "factory" ]
        (Dict.map
            (viewCapital model)
            model.capital
            |> Dict.values
        )


viewButtons : Model -> Html Msg
viewButtons model =
    div
        [ class "buttons" ]
        [ div
            []
            [ button
                [ disabled (model.money < capitalCost model), onClick AddCapital ]
                [ text ("Buy Capital (₮" ++ String.fromInt (capitalCost model) ++ ")") ]
            ]
        , div
            []
            [ button
                [ disabled (model.money < workerCost model), onClick AddWorker ]
                [ text ("Hire worker (₮" ++ String.fromInt (workerCost model) ++ " + ₮" ++ String.fromInt (workerUpkeep model) ++ "p/w)") ]
            ]
        , div
            []
            [ button
                [ disabled (model.money < automationCost model), onClick AddAutomation ]
                [ text ("Automation (₮" ++ String.fromInt (automationCost model) ++ ", + ₮" ++ String.fromInt (automationAdd model) ++ " per click)") ]
            ]
        ]


viewWorker : Model -> WorkerId -> Worker -> Html Msg
viewWorker model workerId worker =
    div
        (class "worker"
            :: Animation.render worker.style
        )
        [ div []
            [ if maybeHasValue worker.target then
                text "😃"

              else
                text "😴"
            ]
        ]


viewCapital : Model -> CapitalId -> Capital -> Html Msg
viewCapital model capitalId capital =
    div
        [ id (capitalDomId capitalId)
        , onClick (CapitalClick capitalId)
        , class "wurk"
        ]
        [ div
            (Animation.render capital.style)
            [ text "Wurk" ]
        ]
