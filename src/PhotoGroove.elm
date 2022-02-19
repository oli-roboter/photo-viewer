module PhotoGroove exposing (main)

import Browser
import Html exposing (..)
import Html.Attributes as Attr exposing( class, classList, id, name, src, title, type_ )
import Html.Events exposing(on, onClick)
import Random
import Http
import Json.Encode as Encode
import Json.Decode exposing (Decoder, at, int, list, string, succeed)
import Json.Decode.Pipeline exposing (optional, required)
import Debug exposing (log)


urlPrefix: String
urlPrefix =
  "http://elm-in-action.com/"



type Msg 
  = ClickedPhoto String
  | GotRandomPhoto Photo
  | ClickedSize ThumbnailSize
  | ClickedSurpriseMe
  | GotPhotos (Result Http.Error (List Photo))
  | SlidHue Int
  | SlidRipple Int
  | SlidNoise Int


view: Model -> Html Msg
view model =
  div [ class "content" ]
    (case model.status of
      Loaded photos selectedUrl ->
        viewLoaded photos selectedUrl model
      Loading -> []
      Errored errorMessage ->
        [ text ("Error: " ++ errorMessage )]
    )

viewFilter : (Int -> Msg) -> String -> Int -> Html Msg
viewFilter toMsg name magnitude =
  div [ class "filter-slider" ]
      [ label [] [ text name ]
      , rangeSlider
          [ Attr.max "11"
          , Attr.property "val" (Encode.int magnitude)
          , onSlide toMsg
          ]
          []
      , label [] [ text (String.fromInt magnitude) ]
      ]

viewLoaded: List Photo -> String -> Model -> List (Html Msg)
viewLoaded  photos selectedUrl model =
  [ h1 [] [ text "Photo Groove" ]
  , button 
      [ onClick ClickedSurpriseMe ]
      [ text "Surprise me" ]
  , div [ class "filters" ]
    [ viewFilter SlidHue "Hue" model.hue
    , viewFilter SlidRipple "Ripple" model.ripple
    , viewFilter SlidNoise "Noise" model.noise
    ]
  , h3 [][ text "Thumbnail Size:" ]
  , div 
    [ id "choose-size" ] <|
    List.map viewSizeChooser [ Small, Medium, Large ]
  , div 
    [ id "thumbnails", class (sizeToString model.chosenSize)] <|
    List.map (viewThumbnail selectedUrl) photos -- -> Generates a list of 3 images
  , img 
    [ class "large"
    , src (urlPrefix ++ "large/" ++ selectedUrl)]
    []
  ]


viewThumbnail: String -> Photo -> Html Msg
viewThumbnail selectedUrl thumb =
    img 
      [ src (urlPrefix ++ thumb.url)
      , title (thumb.title ++ " [" ++String.fromInt thumb.size ++ "KB]")
      , classList [ ( "selected", selectedUrl == thumb.url )]
      , onClick ( ClickedPhoto thumb.url )
      ][]

viewSizeChooser: ThumbnailSize -> Html Msg
viewSizeChooser size =
  label []
    [ input [ type_ "radio", name "size", onClick ( ClickedSize size ) ][]
    , text (sizeToString size)
    ]
    

sizeToString: ThumbnailSize -> String
sizeToString size =
  case size of
    Small ->
      "small"
    Medium ->
      "med"
    Large ->
      "large"


type ThumbnailSize 
  = Small
  | Medium
  | Large


type alias Photo =
  { url: String
  , size: Int
  , title: String
  }


photoDecoder: Decoder Photo
photoDecoder =
  -- map3
    -- (\url size title -> { url = url, size = size, title = title })
    --     (field "url" string)
    --     (field "size" int)
    --     (field "title" string)
  succeed Photo
      |> required "url" string
      |> required "size" int
      |> optional "title" string "(untitled)"



type Status 
  = Loading
  | Loaded (List Photo) String
  | Errored String



-- MODEL --
type alias Model =
  { status: Status
  , chosenSize: ThumbnailSize
  , hue: Int
  , ripple: Int
  , noise: Int
  }


initialModel: Model
initialModel =
  { status = Loading
    , chosenSize = Medium
    , hue = 5
    , ripple = 5
    , noise = 5
  }


update: Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
    GotRandomPhoto photo ->
      ({ model | status = selectUrl photo.url model.status}, Cmd.none )
    ClickedPhoto url ->
      ({ model | status = selectUrl url model.status }, Cmd.none )
    ClickedSize size ->
      ({ model | chosenSize = size }, Cmd.none )
    ClickedSurpriseMe ->
      case model.status of
        Loaded (firstPhoto :: otherPhotos) _ ->
          -- ( model, Random.generate GotRandomPhoto (Random.uniform firstPhoto otherPhotos )) --same as below
          Random.uniform firstPhoto otherPhotos
            |> Random.generate GotRandomPhoto
            |> Tuple.pair model
        Loaded [] _ ->
           ( model, Cmd.none )
        Loading ->
          ( model, Cmd.none )
        Errored errorMessage ->
          ( model, Cmd.none )
    GotPhotos (Ok photos) ->
      let
        serverResp = log "JSONResp" photos
      in
      case photos of
        first :: rest ->
          let
            photosLog = log "photos" photos
            firstUrlLog = log "Firsrtphoto" first
          in
          ({ model | status = Loaded photos first.url }, Cmd.none )
        [] ->
          ({ model | status = Errored "0 photos found" }, Cmd.none )
    GotPhotos (Err httpError )->
      ({ model | status = Errored "Server error!" }, Cmd.none ) 
    SlidHue hue ->
      ( { model | hue = hue }, Cmd.none )
    SlidRipple ripple ->
        ( { model | ripple = ripple }, Cmd.none )
    SlidNoise noise ->
        ( { model | noise = noise }, Cmd.none )  
 

selectUrl : String -> Status -> Status
selectUrl url status =
    case status of
        Loaded photos _ ->
            Loaded photos url
        Loading ->
            status
        Errored errorMessage ->
            status


initialCmd: Cmd Msg
initialCmd = 
  Http.get
    { url = "http://elm-in-action.com/photos/list.json"
    -- , expect = Http.expectString (\result -> GotPhotos result) same as libe below
    , expect = Http.expectJson GotPhotos (list photoDecoder)
    }



--MAIN---
main: Program() Model Msg
main =
  Browser.element
    { init = \_ -> ( initialModel, initialCmd )
    , view = view
    , update = update
    , subscriptions = \_ -> Sub.none
    }

--Javascript integration--

rangeSlider : List (Attribute msg) -> List (Html msg) -> Html msg
rangeSlider attributes children =
  node "range-slider" attributes children

onSlide: (Int -> msg) -> Attribute msg
-- onSlide toMsg =
--   let
--     detailUserSlidTo : Decoder Int
--     detailUserSlidTo =
--       at [ "detail", "userSlidTo" ] int

--     msgDecoder : Decoder msg
--     msgDecoder =
--       Json.Decode.map toMsg detailUserSlidTo
--   in
--   on "slide" msgDecoder
onSlide toMsg =
  at [ "detail", "userSlidTo" ] int -- Decode method that parses tghrough fields "detail: { userSlidTo: value }"
    |> Json.Decode.map toMsg -- transforms the int into a message
    |> on "slide" 