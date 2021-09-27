module Http.Request exposing
    ( Request, RequestCustomData, RequestNoContent, RequestFiles, RequestAdv, RequestEx, RequestCustomDataEx, RequestNoContentEx, RequestFilesEx, RequestAdvEx, AdvanceContent(..)
    , request, requestCustomData, requestNoContent, requestFiles, requestAdv, requestEx, requestCustomDataEx, requestNoContentEx, requestFilesEx, requestAdvEx
    )

{-| `Http.Request` allows you to create http requests with [`jsonapi`](https://package.elm-lang.org/packages/FabienHenon/jsonapi/latest/) objects.

The request functions will return a [`RemoteData`](https://package.elm-lang.org/packages/krisajenkins/remotedata/latest/) with a jsonapi `Document`.
The `content-type` used in the request headers is `application/vnd.api+json` according to the jsonapi specification.


# Definitions

@docs Request, RequestCustomData, RequestNoContent, RequestFiles, RequestAdv, RequestEx, RequestCustomDataEx, RequestNoContentEx, RequestFilesEx, RequestAdvEx, AdvanceContent


# Requests

@docs request, requestCustomData, requestNoContent, requestFiles, requestAdv, requestEx, requestCustomDataEx, requestNoContentEx, requestFilesEx, requestAdvEx

-}

import Dict
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


{-| Defines a `RequestEx` with a url, some headers, a body, and a jsonapi document decoder.
`meta`and `data` are types used by the jsonapi document that represent the `meta` object you would like to decode, and the `data` object you would
like to decode.
You can also extract some response headers to use them later
-}
type alias RequestEx meta data =
    { url : Http.Url.Url
    , headers : List Http.Header
    , body : JE.Value
    , documentDecoder : JD.Decoder (Result (List Decode.Error) (Document meta data))
    , extractHeaders : List String
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


{-| Defines a `Request` with a url, some headers, a body, and a custom decoder.
`data` is the type of the object you would like to decode.
You can also extract some response headers to use them later
-}
type alias RequestCustomDataEx data =
    { url : Http.Url.Url
    , headers : List Http.Header
    , body : JE.Value
    , documentDecoder : JD.Decoder (Result (List Decode.Error) data)
    , extractHeaders : List String
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


{-| Defines a `Request` with a url, some headers, a list of files, and a custom decoder.
`data` is the type of the object you would like to decode.
You can also extract some response headers to use them later
-}
type alias RequestFilesEx meta data msg =
    { url : Http.Url.Url
    , headers : List Http.Header
    , multipartNames : List String
    , files : List File
    , multipartEncoder : String -> File -> Http.Part
    , documentDecoder : JD.Decoder (Result (List Decode.Error) (Document meta data))
    , tracker : Maybe String
    , extractHeaders : List String
    , msg : RemoteData.RemoteData ( RequestError, List ( String, String ) ) ( Document meta data, List ( String, String ) ) -> msg
    }


{-| Defines a `Request` with a url, some headers, and a body. There is no content decoder.
Useful for `DELETE` requests for instance.
-}
type alias RequestNoContent =
    { url : Http.Url.Url
    , headers : List Http.Header
    , body : JE.Value
    }


{-| Defines a `Request` with a url, some headers, and a body. There is no content decoder.
Useful for `DELETE` requests for instance.
You can also extract some response headers to use them later
-}
type alias RequestNoContentEx =
    { url : Http.Url.Url
    , headers : List Http.Header
    , body : JE.Value
    , extractHeaders : List String
    }


{-| Type used to specify the content received from an advanced request like `requestAdv` or `requestAdvEx`
Possible values are:

  - We received a JsonAPI Document
  - We received no content (a 204 response from the API)

-}
type AdvanceContent meta data
    = DocumentContent (Document meta data)
    | NoContent


{-| Defines a `RequestAdv` with a url, some headers, a body, and a jsonapi document decoder.
`meta`and `data` are types used by the jsonapi document that represent the `meta` object you would like to decode, and the `data` object you would
like to decode
-}
type alias RequestAdv meta data =
    { url : Http.Url.Url
    , headers : List Http.Header
    , body : JE.Value
    , documentDecoder : JD.Decoder (Result (List Decode.Error) (Document meta data))
    }


{-| Defines a `RequestAdvEx` with a url, some headers, a body, and a jsonapi document decoder.
`meta`and `data` are types used by the jsonapi document that represent the `meta` object you would like to decode, and the `data` object you would
like to decode.
You can also extract some response headers to use them later
-}
type alias RequestAdvEx meta data =
    { url : Http.Url.Url
    , headers : List Http.Header
    , body : JE.Value
    , documentDecoder : JD.Decoder (Result (List Decode.Error) (Document meta data))
    , extractHeaders : List String
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
        , resolver = resourceResolver request_.documentDecoder [] removeHeadersFromTask
        , timeout = Nothing
        }


{-| Create a request task that will decode a jsonapi object. It will also return in the msg the list of extracted headers.
-}
requestEx : RequestEx meta data -> Task Never (RemoteData.RemoteData ( RequestError, List ( String, String ) ) ( Document meta data, List ( String, String ) ))
requestEx request_ =
    Http.task
        { method = request_.url |> .method |> Http.Methods.toString
        , url = request_.url |> .url
        , headers =
            Http.header "Accept" "application/vnd.api+json"
                :: request_.headers
        , body = Http.stringBody "application/vnd.api+json" (JE.encode 0 request_.body)
        , resolver = resourceResolver request_.documentDecoder request_.extractHeaders identity
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
        , resolver = resourceResolver request_.documentDecoder [] removeHeadersFromTask
        , timeout = Nothing
        }


{-| Create a request task that will decode a custom object. It will also return in the msg the list of extracted headers.
-}
requestCustomDataEx : RequestCustomDataEx data -> Task Never (RemoteData.RemoteData ( RequestError, List ( String, String ) ) ( data, List ( String, String ) ))
requestCustomDataEx request_ =
    Http.task
        { method = request_.url |> .method |> Http.Methods.toString
        , url = request_.url |> .url
        , headers =
            Http.header "Accept" "application/vnd.api+json"
                :: request_.headers
        , body = Http.stringBody "application/vnd.api+json" (JE.encode 0 request_.body)
        , resolver = resourceResolver request_.documentDecoder request_.extractHeaders identity
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
        , expect = resourceExpecter (removeHeadersFromTask >> request_.msg) request_.documentDecoder []
        , timeout = Nothing
        , tracker = request_.tracker
        }


{-| Create a request files that will decode a jsonapi object. It will also return in the msg the list of extracted headers.
-}
requestFilesEx : RequestFilesEx meta data msg -> Cmd msg
requestFilesEx request_ =
    Http.request
        { method = request_.url |> .method |> Http.Methods.toString
        , headers =
            Http.header "Accept" "application/vnd.api+json"
                :: request_.headers
        , url = request_.url |> .url
        , body = List.map2 request_.multipartEncoder request_.multipartNames request_.files |> Http.multipartBody
        , expect = resourceExpecter request_.msg request_.documentDecoder request_.extractHeaders
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
        , resolver = noContentResolver [] removeHeadersFromNoContentTask
        , timeout = Nothing
        }


{-| Create a request task that will expect nothing to decode. It will also return in the msg the list of extracted headers.
-}
requestNoContentEx : RequestNoContentEx -> Task Never (RemoteData.RemoteData ( RequestError, List ( String, String ) ) (List ( String, String )))
requestNoContentEx request_ =
    Http.task
        { method = request_.url |> .method |> Http.Methods.toString
        , url = request_.url |> .url
        , headers =
            Http.header "Accept" "application/vnd.api+json"
                :: request_.headers
        , body = Http.stringBody "application/vnd.api+json" (JE.encode 0 request_.body)
        , resolver = noContentResolver request_.extractHeaders identity
        , timeout = Nothing
        }


{-| Create an advanced request task that will decode an advance content: a jsonapi document or no content
-}
requestAdv : Request meta data -> Task Never (RemoteData.RemoteData RequestError (AdvanceContent meta data))
requestAdv request_ =
    Http.task
        { method = request_.url |> .method |> Http.Methods.toString
        , url = request_.url |> .url
        , headers =
            Http.header "Accept" "application/vnd.api+json"
                :: request_.headers
        , body = Http.stringBody "application/vnd.api+json" (JE.encode 0 request_.body)
        , resolver = resourceAdvResolver request_.documentDecoder [] removeHeadersFromTask
        , timeout = Nothing
        }


{-| Create a request task that will decode an advance content: a jsonapi document or no content. It will also return in the msg the list of extracted headers.
-}
requestAdvEx : RequestEx meta data -> Task Never (RemoteData.RemoteData ( RequestError, List ( String, String ) ) ( AdvanceContent meta data, List ( String, String ) ))
requestAdvEx request_ =
    Http.task
        { method = request_.url |> .method |> Http.Methods.toString
        , url = request_.url |> .url
        , headers =
            Http.header "Accept" "application/vnd.api+json"
                :: request_.headers
        , body = Http.stringBody "application/vnd.api+json" (JE.encode 0 request_.body)
        , resolver = resourceAdvResolver request_.documentDecoder request_.extractHeaders identity
        , timeout = Nothing
        }


resourceExpecter : (RemoteData.RemoteData ( RequestError, List ( String, String ) ) ( Document meta data, List ( String, String ) ) -> msg) -> JD.Decoder (Result (List Decode.Error) (Document meta data)) -> List String -> Http.Expect msg
resourceExpecter msg documentDecoder headersToExtract =
    Http.expectStringResponse (Result.withDefault (RemoteData.Failure ( CustomError "Unknown error", [] )) >> msg) <|
        \response ->
            case response of
                Http.BadUrl_ url ->
                    Ok (RemoteData.Failure ( HttpError (Http.BadUrl url), [] ))

                Http.Timeout_ ->
                    Ok (RemoteData.Failure ( HttpError Http.Timeout, [] ))

                Http.NetworkError_ ->
                    Ok (RemoteData.Failure ( HttpError Http.NetworkError, [] ))

                Http.BadStatus_ metadata body ->
                    if metadata.statusCode == 422 then
                        case JD.decodeString documentDecoder body of
                            Ok document ->
                                case document of
                                    Err errors ->
                                        Ok (RemoteData.Failure ( JsonApiError errors, extractHeaders headersToExtract metadata ))

                                    Ok doc ->
                                        Ok (RemoteData.Success ( doc, extractHeaders headersToExtract metadata ))

                            Err err ->
                                Ok (RemoteData.Failure ( HttpError (Http.BadBody (JD.errorToString err)), extractHeaders headersToExtract metadata ))

                    else
                        Ok (RemoteData.Failure ( HttpError (Http.BadStatus metadata.statusCode), extractHeaders headersToExtract metadata ))

                Http.GoodStatus_ metadata body ->
                    case JD.decodeString documentDecoder body of
                        Ok document ->
                            case document of
                                Err errors ->
                                    Ok (RemoteData.Failure ( JsonApiError errors, extractHeaders headersToExtract metadata ))

                                Ok doc ->
                                    Ok (RemoteData.Success ( doc, extractHeaders headersToExtract metadata ))

                        Err err ->
                            Ok (RemoteData.Failure ( HttpError (Http.BadBody (JD.errorToString err)), extractHeaders headersToExtract metadata ))


resourceResolver : JD.Decoder (Result (List Decode.Error) a) -> List String -> (RemoteData.RemoteData ( RequestError, List ( String, String ) ) ( a, List ( String, String ) ) -> RemoteData.RemoteData err succ) -> Http.Resolver Never (RemoteData.RemoteData err succ)
resourceResolver documentDecoder headersToExtract mapper =
    Http.stringResolver <|
        \response ->
            (case response of
                Http.BadUrl_ url ->
                    Ok (RemoteData.Failure ( HttpError (Http.BadUrl url), [] ))

                Http.Timeout_ ->
                    Ok (RemoteData.Failure ( HttpError Http.Timeout, [] ))

                Http.NetworkError_ ->
                    Ok (RemoteData.Failure ( HttpError Http.NetworkError, [] ))

                Http.BadStatus_ metadata body ->
                    if metadata.statusCode == 422 then
                        case JD.decodeString documentDecoder body of
                            Ok document ->
                                case document of
                                    Err errors ->
                                        Ok (RemoteData.Failure ( JsonApiError errors, extractHeaders headersToExtract metadata ))

                                    Ok doc ->
                                        Ok (RemoteData.Success ( doc, extractHeaders headersToExtract metadata ))

                            Err err ->
                                Ok (RemoteData.Failure ( HttpError (Http.BadBody (JD.errorToString err)), extractHeaders headersToExtract metadata ))

                    else
                        Ok (RemoteData.Failure ( HttpError (Http.BadStatus metadata.statusCode), extractHeaders headersToExtract metadata ))

                Http.GoodStatus_ metadata body ->
                    case JD.decodeString documentDecoder body of
                        Ok document ->
                            case document of
                                Err errors ->
                                    Ok (RemoteData.Failure ( JsonApiError errors, extractHeaders headersToExtract metadata ))

                                Ok doc ->
                                    Ok (RemoteData.Success ( doc, extractHeaders headersToExtract metadata ))

                        Err err ->
                            Ok (RemoteData.Failure ( HttpError (Http.BadBody (JD.errorToString err)), extractHeaders headersToExtract metadata ))
            )
                |> Result.map mapper


noContentResolver : List String -> (RemoteData.RemoteData ( RequestError, List ( String, String ) ) (List ( String, String )) -> RemoteData.RemoteData err succ) -> Http.Resolver Never (RemoteData.RemoteData err succ)
noContentResolver headersToExtract mapper =
    Http.stringResolver <|
        \response ->
            (case response of
                Http.BadUrl_ url ->
                    Ok (RemoteData.Failure ( HttpError (Http.BadUrl url), [] ))

                Http.Timeout_ ->
                    Ok (RemoteData.Failure ( HttpError Http.Timeout, [] ))

                Http.NetworkError_ ->
                    Ok (RemoteData.Failure ( HttpError Http.NetworkError, [] ))

                Http.BadStatus_ metadata body ->
                    Ok (RemoteData.Failure ( HttpError (Http.BadStatus metadata.statusCode), extractHeaders headersToExtract metadata ))

                Http.GoodStatus_ metadata body ->
                    case metadata.statusCode of
                        200 ->
                            Ok (RemoteData.Success (extractHeaders headersToExtract metadata))

                        202 ->
                            Ok (RemoteData.Success (extractHeaders headersToExtract metadata))

                        204 ->
                            Ok (RemoteData.Success (extractHeaders headersToExtract metadata))

                        _ ->
                            Ok (RemoteData.Failure ( HttpError (Http.BadStatus metadata.statusCode), extractHeaders headersToExtract metadata ))
            )
                |> Result.map mapper


resourceAdvResolver : JD.Decoder (Result (List Decode.Error) (Document meta data)) -> List String -> (RemoteData.RemoteData ( RequestError, List ( String, String ) ) ( AdvanceContent meta data, List ( String, String ) ) -> RemoteData.RemoteData err succ) -> Http.Resolver Never (RemoteData.RemoteData err succ)
resourceAdvResolver documentDecoder headersToExtract mapper =
    Http.stringResolver <|
        \response ->
            (case response of
                Http.BadUrl_ url ->
                    Ok (RemoteData.Failure ( HttpError (Http.BadUrl url), [] ))

                Http.Timeout_ ->
                    Ok (RemoteData.Failure ( HttpError Http.Timeout, [] ))

                Http.NetworkError_ ->
                    Ok (RemoteData.Failure ( HttpError Http.NetworkError, [] ))

                Http.BadStatus_ metadata body ->
                    if metadata.statusCode == 422 then
                        case JD.decodeString documentDecoder body of
                            Ok document ->
                                case document of
                                    Err errors ->
                                        Ok (RemoteData.Failure ( JsonApiError errors, extractHeaders headersToExtract metadata ))

                                    Ok doc ->
                                        Ok (RemoteData.Success ( DocumentContent doc, extractHeaders headersToExtract metadata ))

                            Err err ->
                                Ok (RemoteData.Failure ( HttpError (Http.BadBody (JD.errorToString err)), extractHeaders headersToExtract metadata ))

                    else
                        Ok (RemoteData.Failure ( HttpError (Http.BadStatus metadata.statusCode), extractHeaders headersToExtract metadata ))

                Http.GoodStatus_ metadata body ->
                    case metadata.statusCode of
                        204 ->
                            Ok (RemoteData.Success ( NoContent, extractHeaders headersToExtract metadata ))

                        _ ->
                            case JD.decodeString documentDecoder body of
                                Ok document ->
                                    case document of
                                        Err errors ->
                                            Ok (RemoteData.Failure ( JsonApiError errors, extractHeaders headersToExtract metadata ))

                                        Ok doc ->
                                            Ok (RemoteData.Success ( DocumentContent doc, extractHeaders headersToExtract metadata ))

                                Err err ->
                                    Ok (RemoteData.Failure ( HttpError (Http.BadBody (JD.errorToString err)), extractHeaders headersToExtract metadata ))
            )
                |> Result.map mapper


extractHeaders : List String -> Http.Metadata -> List ( String, String )
extractHeaders headersToExtract =
    let
        headersToExtractLower =
            headersToExtract
                |> List.map String.toLower
    in
    .headers
        >> Dict.toList
        >> List.filter (Tuple.first >> String.toLower >> isMember headersToExtractLower)


isMember : List String -> String -> Bool
isMember items item =
    List.member item items


removeHeadersFromTask : RemoteData.RemoteData ( err, List ( String, String ) ) ( succ, List ( String, String ) ) -> RemoteData.RemoteData err succ
removeHeadersFromTask =
    RemoteData.mapBoth Tuple.first Tuple.first


removeHeadersFromNoContentTask : RemoteData.RemoteData ( err, List ( String, String ) ) (List ( String, String )) -> RemoteData.RemoteData err ()
removeHeadersFromNoContentTask =
    RemoteData.mapBoth (always ()) Tuple.first
