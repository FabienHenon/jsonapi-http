module Http.Error exposing
    ( RequestError(..), ErrorConfig
    , displayError, getErrorForField, getJsonApiErrors
    )

{-| The `Http.Error` module defines a new kind of http error related to json api


# Definitions

@docs RequestError, ErrorConfig


# Functions

@docs displayError, getErrorForField, getJsonApiErrors

-}

import Dict
import Html
import Html.Attributes as Attr
import Http
import Json.Decode as JD
import JsonApi.Decode as Decode


{-| You can have:

  - `HttpError`: a normal http error
  - `JsonApiError`: an error related to the json api specification
  - `CustomError`: a custom error

-}
type RequestError
    = HttpError Http.Error
    | JsonApiError (List Decode.Error)
    | CustomError String


{-| Configuration used to display a `RequestError`

  - `httpErrortoString` is the function used to display a string from an http error
  - `errorClass` is an additionnal class you can add to the main container of the error
  - `detailClass` is an additionnal class you can add to children of the main container of the error

-}
type alias ErrorConfig =
    { httpErrorToString : Http.Error -> String
    , errorClass : Maybe String
    , detailClass : Maybe String
    }


{-| Displays a `RequestError`. You must provide a configuration to be able to display
correctly any kind of error.

Errors are displayed like this:

    <span class="error">
        <span class "error-detail error-detail-username1">Error on username</span>
        <span class "error-detail error-detail-password2">Error on password</span>
    </span>

-}
displayError : ErrorConfig -> RequestError -> Html.Html msg
displayError config error =
    case error of
        HttpError err ->
            Html.span [ Attr.class ("error " ++ (config.errorClass |> Maybe.withDefault "")) ]
                [ Html.span [ Attr.class ("error-detail " ++ (config.detailClass |> Maybe.withDefault "")) ]
                    [ Html.text (config.httpErrorToString err) ]
                ]

        JsonApiError err ->
            Html.span [ Attr.class ("error " ++ (config.errorClass |> Maybe.withDefault "")) ]
                (err |> List.map (viewJsonApiError config.detailClass))

        CustomError err ->
            Html.span [ Attr.class ("error " ++ (config.errorClass |> Maybe.withDefault "")) ]
                [ Html.span [ Attr.class ("error-detail " ++ (config.detailClass |> Maybe.withDefault "")) ]
                    [ Html.text err ]
                ]


{-| Returns the jsonapi error message for the given field name

Let's say you have the following json api error:

    {
       "errors":[
          {
             "detail":"Password should be at least 8 character(s)",
             "source":{
                "pointer":"/data/attributes/password"
             },
             "title":"should be at least 8 character(s)"
          }
       ],
       "jsonapi":{
          "version":"1.0"
       }
    }

You can get the detail error message _"Password should be at least 8 character(s)"_ like this:

    getErrorForField "password" error

-}
getErrorForField : String -> List Decode.Error -> Maybe String
getErrorForField field =
    List.filter (isCorrectField field)
        >> List.head
        >> Maybe.map getErrorMsg


{-| Retuens a list of the json api errors or an empty list if there is no error.

Let's say you have the following json api errors:

    {
       "errors":[
          {
             "detail":"Password should be at least 8 character(s)",
             "source":{
                "pointer":"/data/attributes/password"
             },
             "title":"should be at least 8 character(s)"
          },
          {
             "detail":"Username must no be empty",
             "source":{
                "pointer":"/data/attributes/username"
             },
             "title":"should not be empty"
          }
       ],
       "jsonapi":{
          "version":"1.0"
       }
    }

Calling `getJsonApiErrors errors` will return:

    [ ( "password", "Password should be at least 8 character(s)" )
    , ( "username", "Username must no be empty" )
    ]

-}
getJsonApiErrors : RequestError -> List ( String, String )
getJsonApiErrors error =
    case error of
        JsonApiError errors ->
            errors
                |> List.filterMap getError

        HttpError err ->
            []

        CustomError err ->
            []


getError : Decode.Error -> Maybe ( String, String )
getError error =
    error.source
        |> Maybe.andThen getSourceValue
        |> Maybe.map (\value -> ( value, getErrorMsg error ))


isCorrectField : String -> Decode.Error -> Bool
isCorrectField field { source } =
    source
        |> Maybe.andThen getSourceValue
        |> Maybe.map (\value -> value == field)
        |> Maybe.withDefault False


viewJsonApiError : Maybe String -> Decode.Error -> Html.Html msg
viewJsonApiError detailClass error =
    Html.span
        [ Attr.class ("error-detail " ++ (error.source |> Maybe.andThen addFieldClass |> Maybe.withDefault "") ++ " " ++ (detailClass |> Maybe.withDefault "")) ]
        [ Html.text (error |> getErrorMsg)
        ]


getSourceValue : JD.Value -> Maybe String
getSourceValue =
    JD.decodeValue (JD.dict JD.string)
        >> Result.toMaybe
        >> Maybe.andThen (Dict.get "pointer")
        >> Maybe.map (String.replace "/data/attributes/" "")


addFieldClass : JD.Value -> Maybe String
addFieldClass =
    getSourceValue
        >> Maybe.map ((++) "error-detail-")


getErrorMsg : Decode.Error -> String
getErrorMsg { detail, title, code, id } =
    detail
        |> Maybe.withDefault
            (title
                |> Maybe.withDefault
                    (code
                        |> Maybe.withDefault
                            (id |> Maybe.withDefault "")
                    )
            )
