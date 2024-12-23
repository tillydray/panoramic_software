module Main exposing (main)

-- TODO consider using https://package.elm-lang.org/packages/turboMaCk/any-dict/latest/Dict-Any to improve type safety

import Browser
import Dict exposing (Dict)
import Html exposing (Html, button, div, h2, img, li, p, span, text, ul)
import Html.Attributes exposing (disabled, src, style)
import Html.Events exposing (onClick)
import Http
import Json.Decode as Decode exposing (Decoder)
import List exposing (sort)
import String


main : Program () Model Msg
main =
    Browser.element
        { init = \_ -> initModel
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }


type alias Model =
    { breedDict : BreedDict
    , viewState : ViewState
    }


type alias BreedDictResponse =
    Dict BreedName (List SubBreedName)


type alias BreedName =
    String


type alias SubBreedName =
    String


type alias BreedDict =
    Dict BreedName BreedInfo


type alias BreedInfo =
    { imageUrls : ImageUrlStatus, subBreeds : SubBreedDict }


type alias SubBreedDict =
    Dict SubBreedName ImageUrlStatus


type ImageUrl
    = ImageUrl String


type ViewState
    = BreedList
    | BreedDetails BreedName PageIndex
    | SubBreedDetails BreedName SubBreedName PageIndex


type ImageUrlStatus
    = Fetched (List ImageUrl)
    | NotFetchedYet


type PageIndex
    = PageIndex Int


initModel : ( Model, Cmd Msg )
initModel =
    ( { breedDict = Dict.empty
      , viewState = BreedList
      }
    , fetchBreeds
    )


type Msg
    = GotBreeds (Result Http.Error BreedDictResponse)
    | GotImages BreedName (Result Http.Error (List ImageUrl))
    | GotSubBreedImages BreedName SubBreedName (Result Http.Error (List ImageUrl))
    | SelectBreed BreedName
    | SelectSubBreed BreedName SubBreedName
    | BackToList
    | NextPage
    | PreviousPage


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotBreeds result ->
            case result of
                Ok breedDictResponse ->
                    ( { model
                        | breedDict = breedDictResponseToBreedDict breedDictResponse
                      }
                    , Cmd.none
                    )

                -- TODO handle errors
                -- BadUrl, BadBody, or BadStatus 4xx -> tell user to contact us
                -- Timeout, NetworkError, or BadStatus 5xx -> retry, possibly using exponential backoff or similar
                _ ->
                    ( model, Cmd.none )

        SelectBreed breedName ->
            let
                cmd =
                    case
                        Dict.get breedName model.breedDict
                            |> Maybe.withDefault { imageUrls = NotFetchedYet, subBreeds = Dict.empty }
                            |> .imageUrls
                    of
                        NotFetchedYet ->
                            fetchBreedImages breedName

                        Fetched _ ->
                            Cmd.none
            in
            ( { model
                | viewState = BreedDetails breedName (PageIndex 0)
              }
            , cmd
            )

        SelectSubBreed breedName subBreedName ->
            let
                subBreedDict =
                    Dict.get breedName model.breedDict
                        |> Maybe.withDefault (BreedInfo NotFetchedYet Dict.empty)
                        |> .subBreeds

                subBreedImageUrlStatus =
                    Maybe.withDefault NotFetchedYet (Dict.get subBreedName subBreedDict)

                cmd =
                    case subBreedImageUrlStatus of
                        Fetched _ ->
                            Cmd.none

                        NotFetchedYet ->
                            fetchSubBreedImages breedName subBreedName
            in
            ( { model
                | viewState = SubBreedDetails breedName subBreedName (PageIndex 0)
              }
            , cmd
            )

        GotImages breedName result ->
            case result of
                Ok imageUrls ->
                    let
                        oldBreedInfo =
                            Dict.get breedName model.breedDict
                                |> Maybe.withDefault (BreedInfo (Fetched []) Dict.empty)

                        newBreedInfo =
                            { oldBreedInfo | imageUrls = Fetched imageUrls }
                    in
                    ( { model
                        | breedDict = Dict.insert breedName newBreedInfo model.breedDict
                      }
                    , Cmd.none
                    )

                -- TODO handle errors
                -- BadUrl, BadBody, or BadStatus 4xx -> tell user to contact us
                -- Timeout, NetworkError, or BadStatus 5xx -> retry, possibly using exponential backoff or similar
                _ ->
                    ( model, Cmd.none )

        GotSubBreedImages breedName subBreedName result ->
            case result of
                Ok imageUrls ->
                    let
                        oldBreedInfo =
                            Dict.get breedName model.breedDict
                                |> Maybe.withDefault (BreedInfo NotFetchedYet Dict.empty)

                        newSubBreedDict =
                            Dict.insert subBreedName (Fetched imageUrls) oldBreedInfo.subBreeds

                        newBreedInfo =
                            { oldBreedInfo | subBreeds = newSubBreedDict }
                    in
                    ( { model
                        | breedDict = Dict.insert breedName newBreedInfo model.breedDict
                      }
                    , Cmd.none
                    )

                -- TODO handle errors
                -- BadUrl, BadBody, or BadStatus 4xx -> tell user to contact us
                -- Timeout, NetworkError, or BadStatus 5xx -> retry, possibly using exponential backoff or similar
                _ ->
                    ( model, Cmd.none )

        BackToList ->
            ( { model | viewState = BreedList }, Cmd.none )

        NextPage ->
            case model.viewState of
                BreedDetails breedName pageIndex ->
                    ( { model | viewState = BreedDetails breedName (plus pageIndex 1) }, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        PreviousPage ->
            case model.viewState of
                BreedDetails breedName pageIndex ->
                    ( { model | viewState = BreedDetails breedName (minus pageIndex 1) }, Cmd.none )

                _ ->
                    ( model, Cmd.none )


breedDictResponseToBreedDict : BreedDictResponse -> BreedDict
breedDictResponseToBreedDict res =
    Dict.map mkBreedInfo res


mkBreedInfo : BreedName -> List SubBreedName -> BreedInfo
mkBreedInfo _ subBreedNames =
    { imageUrls = NotFetchedYet
    , subBreeds = Dict.fromList (List.map mkSubBreedTuple subBreedNames)
    }


mkSubBreedTuple : SubBreedName -> ( SubBreedName, ImageUrlStatus )
mkSubBreedTuple subBreedName =
    ( subBreedName, NotFetchedYet )


{-| Improves type safety and ergonomics
-}
minus : PageIndex -> Int -> PageIndex
minus (PageIndex pageIndex) num =
    PageIndex (pageIndex - num)


plus : PageIndex -> Int -> PageIndex
plus (PageIndex pageIndex) num =
    PageIndex (pageIndex + num)


view : Model -> Html Msg
view model =
    case model.viewState of
        BreedList ->
            viewBreedList model

        BreedDetails breedName pageIndex ->
            let
                imageUrlStatus =
                    Dict.get breedName model.breedDict
                        |> Maybe.withDefault { imageUrls = NotFetchedYet, subBreeds = Dict.empty }
                        |> .imageUrls
            in
            viewBreedDetails imageUrlStatus breedName pageIndex

        SubBreedDetails breedName subBreedName pageIndex ->
            let
                subBreedDict =
                    Dict.get breedName model.breedDict
                        |> Maybe.withDefault (BreedInfo NotFetchedYet Dict.empty)
                        |> .subBreeds

                imageUrlStatus =
                    Dict.get subBreedName subBreedDict
                        |> Maybe.withDefault NotFetchedYet
            in
            viewBreedDetails imageUrlStatus subBreedName pageIndex


viewBreedList : Model -> Html Msg
viewBreedList model =
    -- we won't get stuck here forever so long as we handle HTTP Errors in `update`.
    if Dict.isEmpty model.breedDict then
        div [] [ text "Loading..." ]

    else
        let
            breedsList =
                Dict.keys model.breedDict
                    |> sort
        in
        if List.length breedsList > 0 then
            div []
                [ h2 [] [ text "Dog Breeds" ]
                , ul []
                    (List.map (viewBreedListItem model.breedDict) breedsList)
                ]

        else
            div [] [ text "No breeds loaded." ]


viewBreedListItem : BreedDict -> BreedName -> Html Msg
viewBreedListItem breedDict breedName =
    let
        subBreedNames =
            Dict.get breedName breedDict
                |> Maybe.withDefault (BreedInfo NotFetchedYet Dict.empty)
                |> .subBreeds
                |> Dict.keys
    in
    li []
        [ button [ onClick (SelectBreed breedName) ] [ text breedName ]
        , if not (List.isEmpty subBreedNames) then
            ul []
                (List.map
                    (\subBreedName ->
                        li []
                            [ button
                                [ onClick (SelectSubBreed breedName subBreedName) ]
                                [ text subBreedName ]
                            ]
                    )
                    subBreedNames
                )

          else
            text ""
        ]


viewBreedDetails : ImageUrlStatus -> BreedName -> PageIndex -> Html Msg
viewBreedDetails imageUrlStatus breedName (PageIndex pageIndex) =
    case imageUrlStatus of
        NotFetchedYet ->
            div [] [ text "Loading..." ]

        Fetched imageUrls ->
            let
                imagesPerPage =
                    20

                totalNumOfImageUrls =
                    List.length imageUrls

                numOfPages =
                    if totalNumOfImageUrls == 0 then
                        1

                    else
                        ((totalNumOfImageUrls - 1) // imagesPerPage) + 1

                imageUrls_ =
                    imageUrls
                        |> List.drop (pageIndex * imagesPerPage)
                        |> List.take imagesPerPage

                canGoToNextPage =
                    pageIndex < numOfPages - 1

                canGoToPrevPage =
                    pageIndex > 0
            in
            div []
                [ button [ onClick BackToList ] [ text "Back to List" ]
                , h2 [] [ text ("Breed: " ++ breedName) ]
                , p [] [ text ("Total Images: " ++ String.fromInt totalNumOfImageUrls) ]
                , div []
                    [ button [ onClick PreviousPage, disabled (not canGoToPrevPage) ] [ text "Previous" ]
                    , span
                        [ style "margin" "0 10px" ]
                        [ text ("Page " ++ String.fromInt (pageIndex + 1) ++ " of " ++ String.fromInt numOfPages) ]
                    , button [ onClick NextPage, disabled (not canGoToNextPage) ] [ text "Next" ]
                    ]
                , div [ style "display" "flex", style "flex-wrap" "wrap" ]
                    (List.map (\imageUrl -> viewImage imageUrl) imageUrls_)
                ]


viewImage : ImageUrl -> Html msg
viewImage (ImageUrl imageUrl) =
    img [ src imageUrl, style "width" "200px", style "margin" "5px" ] []


fetchBreeds : Cmd Msg
fetchBreeds =
    Http.get
        { url = "https://dog.ceo/api/breeds/list/all"
        , expect = Http.expectJson GotBreeds breedsDecoder
        }


fetchBreedImages : BreedName -> Cmd Msg
fetchBreedImages breedName =
    let
        url =
            "https://dog.ceo/api/breed/" ++ breedName ++ "/images"
    in
    Http.get
        { url = url
        , expect = Http.expectJson (GotImages breedName) imagesDecoder
        }


fetchSubBreedImages : BreedName -> SubBreedName -> Cmd Msg
fetchSubBreedImages breedName subBreedName =
    let
        url =
            "https://dog.ceo/api/breed/" ++ breedName ++ "/" ++ subBreedName ++ "/images"
    in
    Http.get
        { url = url
        , expect = Http.expectJson (GotSubBreedImages breedName subBreedName) imagesDecoder
        }


breedsDecoder : Decoder BreedDictResponse
breedsDecoder =
    Decode.field "message" (Decode.dict (Decode.list Decode.string))


imagesDecoder : Decoder (List ImageUrl)
imagesDecoder =
    Decode.field "message" (Decode.map (List.map ImageUrl) (Decode.list Decode.string))
