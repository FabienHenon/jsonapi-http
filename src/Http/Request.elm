module Http.Request exposing
    ( Request, RequestCustomData, RequestNoContent
    , request, requestCustomData, requestNoContent
    , RequestFiles, requestFiles
    )

{-| `Http.Request` allows you to create http requests with [`jsonapi`](https://package.elm-lang.org/packages/FabienHenon/jsonapi/latest/) objects.

The request functions will return a [`RemoteData`](https://package.elm-lang.org/packages/krisajenkins/remotedata/latest/) with a jsonapi `Document`.
The `content-type` used in the request headers is `application/vnd.api+json` according to the jsonapi specification.


# Definitions

@docs Request, RequestCustomData, RequestNoContent


# Requests

@docs request, requestCustomData, requestNoContent

-}

import File exposing (File)
import Http
import Http.Error exposing (RequestError(..))
import Http.Methods
import Http.Url
import Json.Decode as JD
import Json.Encode as JE
import JsonApi.Decode as Decode
import JsonApi.Document exposing (Document)
import RemoteData
import Task exposing (Task)


{-| Defines a `Request` with a url, some headers, a body, and a jsonapi document decoder.
`meta`and `data` are types used by the jsonapi document that represent the `meta` object you would like to decode, and the `data` object you would
like to decode
-}
type alias Request meta data =
    { url : Http.Url.Url
    , headers : List Http.Header
    , body : JE.Value
    , documentDecoder : JD.Decoder (Result (List Decode.Error) (Document meta data))
    }


{-| Defines a `Request` with a url, some headers, a body, and a custom decoder.
`data` is the type of the object you would like to decode.
-}
type alias RequestCustomData data =
    { url : Http.Url.Url
    , headers : List Http.Header
    , body : JE.Value
    , documentDecoder : JD.Decoder (Result (List Decode.Error) data)
    }


{-| Defines a `Request` with a url, some headers, a list of files, and a custom decoder.
`data` is the type of the object you would like to decode.
-}
type alias RequestFiles meta data msg =
    { url : Http.Url.Url
    , headers : List Http.Header
    , multipartNames : List String
    , files : List File
    , multipartEncoder : String -> File -> Http.Part
    , documentDecoder : JD.Decoder (Result (List Decode.Error) (Document meta data))
    , tracker : Maybe String
    , msg : RemoteData.RemoteData RequestError (Document meta data) -> msg
    }


{-| Defines a `Request` with a url, some headers, and a body. There is no content decoder.
Useful for `DELETE` requests for instance.
-}
type alias RequestNoContent =
    { url : Http.Url.Url
    , headers : List Http.Header
    , body : JE.Value
    }


{-| Create a request task that will decode a jsonapi object
-}
request : Request meta data -> Task Never (RemoteData.RemoteData RequestError (Document meta data))
request request_ =
    Http.task
        { method = request_.url |> .method |> Http.Methods.toString
        , url = request_.url |> .url
        , headers =
            Http.header "Accept" "application/vnd.api+json"
                :: request_.headers
        , body = Http.stringBody "application/vnd.api+json" (JE.encode 0 request_.body)
        , resolver = resourceResolver request_.documentDecoder
        , timeout = Nothing
        }


{-| Create a request task that will decode a custom object
-}
requestCustomData : RequestCustomData data -> Task Never (RemoteData.RemoteData RequestError data)
requestCustomData request_ =
    Http.task
        { method = request_.url |> .method |> Http.Methods.toString
        , url = request_.url |> .url
        , headers =
            Http.header "Accept" "application/vnd.api+json"
                :: request_.headers
        , body = Http.stringBody "application/vnd.api+json" (JE.encode 0 request_.body)
        , resolver = resourceResolver request_.documentDecoder
        , timeout = Nothing
        }


{-| Create a request files that will decode a jsonapi object
-}
requestFiles : RequestFiles meta data msg -> Cmd msg
requestFiles request_ =
    Http.request
        { method = request_.url |> .method |> Http.Methods.toString
        , headers =
            Http.header "Accept" "application/vnd.api+json"
                :: request_.headers
        , url = request_.url |> .url
        , body = List.map2 request_.multipartEncoder request_.multipartNames request_.files |> Http.multipartBody
        , expect = resourceExpecter request_.msg request_.documentDecoder
        , timeout = Nothing
        , tracker = request_.tracker
        }


{-| Create a request task that will expect nothing to decode
-}
requestNoContent : RequestNoContent -> Task Never (RemoteData.RemoteData RequestError ())
requestNoContent request_ =
    Http.task
        { method = request_.url |> .method |> Http.Methods.toString
        , url = request_.url |> .url
        , headers =
            Http.header "Accept" "application/vnd.api+json"
                :: request_.headers
        , body = Http.stringBody "application/vnd.api+json" (JE.encode 0 request_.body)
        , resolver = noContentResolver
        , timeout = Nothing
        }


resourceExpecter : (RemoteData.RemoteData RequestError (Document meta data) -> msg) -> JD.Decoder (Result (List Decode.Error) (Document meta data)) -> Http.Expect msg
resourceExpecter msg documentDecoder =
    Http.expectStringResponse (Result.withDefault (RemoteData.Failure (CustomError "Unknown error")) >> msg) <|
        \response ->
            case response of
                Http.BadUrl_ url ->
                    Ok (RemoteData.Failure (HttpError (Http.BadUrl url)))

                Http.Timeout_ ->
                    Ok (RemoteData.Failure (HttpError Http.Timeout))

                Http.NetworkError_ ->
                    Ok (RemoteData.Failure (HttpError Http.NetworkError))

                Http.BadStatus_ metadata body ->
                    if metadata.statusCode == 422 then
                        case JD.decodeString documentDecoder body of
                            Ok document ->
                                case document of
                                    Err errors ->
                                        Ok (RemoteData.Failure (JsonApiError errors))

                                    Ok doc ->
                                        Ok (RemoteData.Success doc)

                            Err err ->
                                Ok (RemoteData.Failure (HttpError (Http.BadBody (JD.errorToString err))))

                    else
                        Ok (RemoteData.Failure (HttpError (Http.BadStatus metadata.statusCode)))

                Http.GoodStatus_ metadata body ->
                    case JD.decodeString documentDecoder body of
                        Ok document ->
                            case document of
                                Err errors ->
                                    Ok (RemoteData.Failure (JsonApiError errors))

                                Ok doc ->
                                    Ok (RemoteData.Success doc)

                        Err err ->
                            Ok (RemoteData.Failure (HttpError (Http.BadBody (JD.errorToString err))))


resourceResolver : JD.Decoder (Result (List Decode.Error) a) -> Http.Resolver Never (RemoteData.RemoteData RequestError a)
resourceResolver documentDecoder =
    Http.stringResolver <|
        \response ->
            case response of
                Http.BadUrl_ url ->
                    Ok (RemoteData.Failure (HttpError (Http.BadUrl url)))

                Http.Timeout_ ->
                    Ok (RemoteData.Failure (HttpError Http.Timeout))

                Http.NetworkError_ ->
                    Ok (RemoteData.Failure (HttpError Http.NetworkError))

                Http.BadStatus_ metadata body ->
                    if metadata.statusCode == 422 then
                        case JD.decodeString documentDecoder body of
                            Ok document ->
                                case document of
                                    Err errors ->
                                        Ok (RemoteData.Failure (JsonApiError errors))

                                    Ok doc ->
                                        Ok (RemoteData.Success doc)

                            Err err ->
                                Ok (RemoteData.Failure (HttpError (Http.BadBody (JD.errorToString err))))

                    else
                        Ok (RemoteData.Failure (HttpError (Http.BadStatus metadata.statusCode)))

                Http.GoodStatus_ metadata body ->
                    case JD.decodeString documentDecoder body of
                        Ok document ->
                            case document of
                                Err errors ->
                                    Ok (RemoteData.Failure (JsonApiError errors))

                                Ok doc ->
                                    Ok (RemoteData.Success doc)

                        Err err ->
                            Ok (RemoteData.Failure (HttpError (Http.BadBody (JD.errorToString err))))


noContentResolver : Http.Resolver Never (RemoteData.RemoteData RequestError ())
noContentResolver =
    Http.stringResolver <|
        \response ->
            case response of
                Http.BadUrl_ url ->
                    Ok (RemoteData.Failure (HttpError (Http.BadUrl url)))

                Http.Timeout_ ->
                    Ok (RemoteData.Failure (HttpError Http.Timeout))

                Http.NetworkError_ ->
                    Ok (RemoteData.Failure (HttpError Http.NetworkError))

                Http.BadStatus_ metadata body ->
                    Ok (RemoteData.Failure (HttpError (Http.BadStatus metadata.statusCode)))

                Http.GoodStatus_ metadata body ->
                    case metadata.statusCode of
                        200 ->
                            Ok (RemoteData.Success ())

                        202 ->
                            Ok (RemoteData.Success ())

                        204 ->
                            Ok (RemoteData.Success ())

                        _ ->
                            Ok (RemoteData.Failure (HttpError (Http.BadStatus metadata.statusCode)))
