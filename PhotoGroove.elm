port module PhotoGroove exposing (..)

import Html exposing (..)
import Html.Attributes as Attr exposing (id, class, classList, src, name, type_, title, checked, max)
import Json.Decode exposing (string, int, list, Decoder, at)
import Json.Decode.Pipeline exposing (decode, required, optional)
import Html.Events exposing (onClick, on)
import Array exposing (Array)
import Random
import Http


type alias Photo =
    { url : String
    , size : Int
    , title : String
    }


photoDecoder : Decoder Photo
photoDecoder =
    decode Photo
        |> required "url" string
        |> required "size" int
        |> optional "title" string "(untitled)"


type alias Model =
    { photos : List Photo
    , selectedUrl : Maybe String
    , loadingError : Maybe String
    , chosenSize : ThumbnailSize
    , hue : Int
    , ripple : Int
    , noise : Int
    , status : String
    }


port setFilters : FilterOptions -> Cmd msg


port statusChanges : (String -> msg) -> Sub msg


type alias FilterOptions =
    { url : String
    , filters : List { name : String, amount : Float }
    }


initialModel : Model
initialModel =
    { photos = []
    , selectedUrl = Nothing
    , loadingError = Nothing
    , chosenSize = Medium
    , hue = 0
    , ripple = 0
    , noise = 0
    , status = ""
    }


type Msg
    = SelectByUrl String
    | SurpriseMe
    | SetSize ThumbnailSize
    | SelectByIndex Int
    | LoadPhotos (Result Http.Error (List Photo))
    | SetHue Int
    | SetRipple Int
    | SetNoise Int
    | SetStatus String


type ThumbnailSize
    = Small
    | Medium
    | Large


urlPrefix : String
urlPrefix =
    "http://elm-in-action.com/"


viewOrError : Model -> Html Msg
viewOrError model =
    case model.loadingError of
        Nothing ->
            view model

        Just errorMessage ->
            div [ class "error-message" ]
                [ h1 [] [ text "Photo Groove" ]
                , p [] [ text errorMessage ]
                ]


view : Model -> Html Msg
view model =
    div [ class "content" ]
        [ h1 [] [ text "Photo Groove" ]
        , button
            [ onClick SurpriseMe ]
            [ text "Surprise Me!" ]
        , div [ class "status" ] [ text model.status ]
        , div [ class "filters" ]
            [ viewFilter "Hue" SetHue model.hue
            , viewFilter "Ripple" SetRipple model.ripple
            , viewFilter "Noise" SetNoise model.noise
            ]
        , h3 [] [ text "Thumbnail Size:" ]
        , div [ id "choose-size" ] <|
            List.map (viewSizeChooser model.chosenSize) [ Small, Medium, Large ]
        , div [ id "thumbnails", class <| sizeToClass model.chosenSize ] <|
            List.map (viewThumbnail model.selectedUrl) model.photos
        , viewLarge model.selectedUrl
        ]


viewFilter : String -> (Int -> Msg) -> Int -> Html Msg
viewFilter name toMsg magnitude =
    div [ class "filter-slider" ]
        [ label [] [ text name ]
        , paperSlider [ Attr.max "11", onImmediateValueChange toMsg ] []
        , label [] [ text (toString magnitude) ]
        ]


viewThumbnail : Maybe String -> Photo -> Html Msg
viewThumbnail selectedUrl thumbnail =
    img
        [ src (urlPrefix ++ thumbnail.url)
        , title (thumbnail.title ++ " [" ++ toString thumbnail.size ++ " KB]")
        , classList [ ( "selected", selectedUrl == Just thumbnail.url ) ]
        , onClick (SelectByUrl thumbnail.url)
        ]
        []


viewSizeChooser : ThumbnailSize -> ThumbnailSize -> Html Msg
viewSizeChooser chosenSize size =
    label []
        [ input
            [ type_ "radio"
            , name "size"
            , onClick <| SetSize size
            , checked <| chosenSize == size
            ]
            []
        , text <| sizeToString size
        ]


sizeToString : ThumbnailSize -> String
sizeToString size =
    case size of
        Small ->
            "small"

        Medium ->
            "medium"

        Large ->
            "large"


sizeToClass : ThumbnailSize -> String
sizeToClass size =
    case size of
        Small ->
            "small"

        Medium ->
            "med"

        Large ->
            "large"


viewLarge : Maybe String -> Html Msg
viewLarge selectedUrl =
    case selectedUrl of
        Nothing ->
            text ""

        Just url ->
            canvas
                [ id "main-canvas"
                , class "large"
                ]
                []


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SelectByUrl selectedUrl ->
            applyFilters { model | selectedUrl = Just selectedUrl }

        SurpriseMe ->
            let
                randomPhotoPicker =
                    Random.int 0 (List.length model.photos - 1)
            in
                ( model, Random.generate SelectByIndex randomPhotoPicker )

        SetSize size ->
            ( { model | chosenSize = size }, Cmd.none )

        SelectByIndex index ->
            let
                newSelectedUrl : Maybe String
                newSelectedUrl =
                    model.photos
                        |> Array.fromList
                        |> Array.get index
                        |> Maybe.map .url
            in
                applyFilters { model | selectedUrl = newSelectedUrl }

        LoadPhotos (Ok photos) ->
            applyFilters
                { model
                    | photos = photos
                    , selectedUrl = Maybe.map .url (List.head photos)
                }

        LoadPhotos (Err _) ->
            ( { model
                | loadingError = Just "Error (Try turning it off and on again.)"
              }
            , Cmd.none
            )

        SetHue hue ->
            applyFilters { model | hue = hue }

        SetRipple ripple ->
            applyFilters { model | ripple = ripple }

        SetNoise noise ->
            applyFilters { model | noise = noise }

        SetStatus status ->
            { model | status = status } ! []


applyFilters : Model -> ( Model, Cmd Msg )
applyFilters model =
    case model.selectedUrl of
        Just selectedUrl ->
            let
                filters =
                    [ { name = "Hue", amount = convertToPercent model.hue }
                    , { name = "Ripple", amount = convertToPercent model.ripple }
                    , { name = "Noise", amount = convertToPercent model.noise }
                    ]

                url =
                    urlPrefix ++ "large/" ++ selectedUrl
            in
                ( model, setFilters { url = url, filters = filters } )

        Nothing ->
            model ! []


convertToPercent : Int -> Float
convertToPercent amount =
    toFloat amount / 11


paperSlider : List (Attribute msg) -> List (Html msg) -> Html msg
paperSlider =
    node "paper-slider"


onImmediateValueChange : (Int -> msg) -> Attribute msg
onImmediateValueChange toMsg =
    at [ "target", "immediateValue" ] int
        |> Json.Decode.map toMsg
        |> on "immediate-value-changed"


initialCmd : Cmd Msg
initialCmd =
    list photoDecoder
        |> Http.get "http://elm-in-action.com/photos/list.json"
        |> Http.send LoadPhotos


init : Float -> ( Model, Cmd Msg )
init flags =
    let
        status =
            "Initializing Pasta v" ++ toString flags
    in
        { initialModel | status = status } ! [ initialCmd ]


main : Program Float Model Msg
main =
    Html.programWithFlags
        { init = init
        , view = viewOrError
        , update = update
        , subscriptions = (\_ -> statusChanges SetStatus)
        }
