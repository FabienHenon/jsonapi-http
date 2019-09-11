module Http.Methods exposing
    ( Method(..)
    , toString
    )

{-| Http methods, to use with requests and urls


# Definitions

@docs Method


# Functions

@docs toString

-}


{-| Defines http verbs
-}
type Method
    = POST
    | PUT
    | DELETE
    | GET
    | PATCH


{-| Transforms an http verb to a string
-}
toString : Method -> String
toString method =
    case method of
        POST ->
            "POST"

        PUT ->
            "PUT"

        DELETE ->
            "DELETE"

        GET ->
            "GET"

        PATCH ->
            "PATCH"
