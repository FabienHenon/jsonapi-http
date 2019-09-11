module Http.Error exposing (RequestError(..))

{-| The `Http.Error` module defines a new kind of http error related to json api


# Definitions

@docs RequestError

-}

import Http
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
