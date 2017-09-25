module Main exposing (main)

import Html
import Html.Attributes
import Html.Events
import Json.Decode as Json
import Maestro exposing (..)
import Maestro.Note exposing (..)
import Maestro.Scale exposing (..)
import Maestro.Tone exposing (..)
import Piano
import Set


safeNth xs n =
    List.head (List.drop n xs)


selectFrom : (a -> String) -> (a -> Msg) -> List a -> a -> Html.Html Msg
selectFrom makeString makeMessage values defaultValue =
    Html.select
        [ Html.Events.on "change"
            (Json.map (\s -> String.toInt s |> Result.withDefault 0 |> safeNth values |> Maybe.withDefault defaultValue |> makeMessage)
                Html.Events.targetValue
            )
        ]
        (List.indexedMap (\index value -> Html.option [ Html.Attributes.value (toString index) ] [ Html.text (makeString value) ])
            values
        )


radioButtonsFrom : String -> (a -> String) -> (a -> Msg) -> List a -> a -> Html.Html Msg
radioButtonsFrom groupName makeString makeMessage values defaultValue =
    let
        makeRadio index value =
            let
                elemId =
                    groupName ++ "-" ++ toString index
            in
            Html.div []
                [ Html.input
                    [ Html.Attributes.type_ "radio"
                    , Html.Attributes.id elemId
                    , Html.Attributes.name groupName
                    , Html.Attributes.checked (value == defaultValue)
                    , Html.Events.on "change" (Json.map (\_ -> makeMessage value) Html.Events.targetValue)
                    ]
                    []
                , Html.label [ Html.Attributes.for elemId ] [ Html.text (makeString value) ]
                ]
    in
    Html.div [] (List.indexedMap makeRadio values)


adjustmentToString : Adjustment -> String
adjustmentToString adj =
    case adj of
        Natural ->
            ""

        Sharp ->
            "#"

        Flat ->
            "b"

        SharpSharp ->
            "##"

        FlatFlat ->
            "bb"


toneToString : Tone -> String
toneToString tone =
    (tone.key |> toString) ++ (tone.adjustment |> adjustmentToString)


scaleToString rootTone mode =
    String.join " " (List.map toneToString (scale rootTone mode))



--


type Msg
    = PianoEvent Piano.Msg
    | SelectMode Mode
    | SelectRootTone Tone


type alias Model =
    { mode : Mode
    , rootTone : Tone
    }


octave =
    3


modes =
    [ Ionian
    , Dorian
    , Phrygian
    , Lydian
    , Mixolydian
    , Aeolian
    , Locrian
    ]


rootTones =
    chromaticTones Natural


defaultMode =
    Ionian


defaultRootTone =
    newTone C Natural


defaultModel =
    { mode = defaultMode, rootTone = defaultRootTone }


update : Msg -> Model -> Model
update msg model =
    case msg of
        PianoEvent _ ->
            model

        SelectMode mode ->
            { model | mode = mode }

        SelectRootTone rootTone ->
            { model | rootTone = rootTone }


midiPitches : Model -> Set.Set Int
midiPitches model =
    Set.fromList
        (List.map (\tone -> newNote tone.key tone.adjustment octave |> noteToIndex)
            (scale model.rootTone model.mode)
        )


view : Model -> Html.Html Msg
view model =
    Html.div []
        [ Html.map PianoEvent
            (Piano.view
                { notes = midiPitches model
                , noteRange = Piano.keyboard49Keys
                , interactive = False
                , showSizeSelector = False
                , debugNotes = False
                }
            )
        , Html.h2 [] [ Html.text "Mode" ]
        , radioButtonsFrom "a" toString (\mode -> SelectMode mode) modes defaultMode
        , Html.h2 [] [ Html.text "Root" ]
        , radioButtonsFrom "b" toneToString (\rootTone -> SelectRootTone rootTone) rootTones defaultRootTone
        , Html.div [] [ Html.text (scaleToString model.rootTone model.mode) ]
        ]


main =
    Html.beginnerProgram { model = defaultModel, update = update, view = view }
