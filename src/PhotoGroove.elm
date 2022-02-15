module PhotoGroove exposing (main)

import Array exposing (Array)
import Browser
import Html exposing (..)
import Html.Attributes exposing(..)
import Html.Events exposing(onClick)
import Random

type alias Photo =
  { url: String }

type alias Model =
  { status: Status
  , chosenSize: ThumbnailSize
  }

type Status 
  = Loading
  | Loaded (List Photo) String
  | Errored String

type Msg 
  = ClickedPhoto String
  | GotSelectedIndex Int
  | ClickedSize ThumbnailSize
  | ClickedSurpriseMe 

type ThumbnailSize 
  = Small
  | Medium
  | Large

initialModel: Model
initialModel =
  { status = Loading
    , chosenSize = Medium
  }


-- photoArray: Array Photo
-- photoArray = Array.fromList initialModel.photos

-- getPhotoUrl: Int -> String
-- getPhotoUrl index =
--   case Array.get index photoArray of
--     Just photo ->
--       photo.url
--     Nothing ->
--       ""

-- randomPhotoPicker: Random.Generator Int
-- randomPhotoPicker = Random.int 0 (Array.length photoArray - 1)

update: Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of 
    GotSelectedIndex index ->
      ({ model | selectedUrl = selectUrl (getPhotoUrl index) model.status}, Cmd.none )
    ClickedPhoto url ->
      ({ model | selectedUrl = selectUrl url model.status}, Cmd.none )
    ClickedSize size ->
      ({ model | chosenSize = size }, Cmd.none )
    ClickedSurpriseMe ->
      ( model, Random.generate GotSelectedIndex randomPhotoPicker )

selectUrl: String -> Status -> Status
selectUrl url status =
  case status of
    Loaded photos _ ->
      Loaded photos url
    Loading ->
      status
    Errored errorMessage ->
      status


urlPrefix: String
urlPrefix =
  "http://elm-in-action.com/"

view: Model -> Html Msg
view model =
  div [ class "content" ] <|
    case model.status of
      Loaded photos selectedUrl ->
        viewLoaded photos selectedUrl model.chosenSize
      Loading ->
        []
      Errored errorMessage ->
        [text ("Error: " ++ errorMessage)]
  
viewLoaded : List Photo -> String -> ThumbnailSize -> List (Html Msg)
viewLoaded photos selectedUrl chosenSize =
  [ h1 [] [ text "Photo Groove" ]
  , button [ onClick ClickedSurpriseMe ][ text "Surprise me" ]
  , h3 [][ text "Thumbnail Size:" ]
  , div [ id "choose-size" ]
      (List.map viewSizeChooser [ Small, Medium, Large ])
  , div [ id "thumbnails", class (sizeToString chosenSize)](
      List.map (viewThumbnail selectedUrl) photos
    ) -- -> Generates a list of 3 images
  , img [ class "large", src (urlPrefix ++ "large/" ++ selectedUrl)][]
  ]
  
viewThumbnail: String -> Photo -> Html Msg
viewThumbnail selectedUrl thumb =
    img 
      [ src (urlPrefix ++ thumb.url)
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

main: Program() Model Msg
main =
  Browser.element
    { init = \flags -> ( initialModel, Cmd.none )
    , view = view
    , update = update
    , subscriptions = \model -> Sub.none
    }