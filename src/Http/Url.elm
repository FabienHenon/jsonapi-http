module Http.Url exposing (Url)

{-| Defines the `Url`type to be used with http requests


# Definitions

@docs Url

-}

import Http.Methods exposing (Method(..))


{-| An url is composed of a url string and a method
-}
type alias Url =
    { url : String
    , method : Method
    }
