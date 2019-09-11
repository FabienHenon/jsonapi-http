# jsonapi-http

Make HTTP requests with jsonapi decoding/encoding in Elm.

This package makes use of the packages [jsonapi](https://package.elm-lang.org/packages/FabienHenon/jsonapi/latest/) and 
[remotedata](https://package.elm-lang.org/packages/krisajenkins/remotedata/latest/).

You will be able to send HTTP requests that expect a jsonapi object as a response.

## Getting started

Here is an example of the creation of a post, that expects a post as a return value.
The posts we send and receive will follow the jsonapi specification.

```elm
type alias User =
   { id : String
   , name : String
   }


type alias Post =
    { id : String
    , title : String
    , body : String
    , creator : User
    }


type alias PostPayload =
    { title : String
    , body : String
    }


createPost : (RemoteData.RemoteData Http.Error.RequestError Post) -> PostPayload -> Cmd msg
createPost msg body =
    Http.Request.request
        { headers = []
        , url = { url = "/api/post", method = Http.Methods.POST }
        , body = encodeBody body
        , documentDecoder = JsonApi.Decode.resource "posts" postDecoder
        }
        |> Task.map (RemoteData.map JsonApi.Document.resource)
        |> Task.perform msg


encodeBody : PostPayload -> Json.Encode.Value
encodeBody body =
    JsonApi.Encode.Document.build
        |> JsonApi.Encode.Document.withResource
            (JsonApi.Resource.build "posts"
                |> JsonApi.Resource.withAttributes
                    [ ( "body", Json.Encode.string body.body )
                    , ( "title", Json.Encode.string body.title )
                    ]
            )
        |> JsonApi.Encode.document


postDecoder : JsonApi.Resource.Resource -> Json.Decode.Decoder Post
postDecoder res =
    Json.Decode.map4 Post
        (Json.Decode.succeed (JsonApi.Resource.id res))
        (Json.Decode.field "title" Json.Decode.string)
        (Json.Decode.field "body" Json.Decode.string)
        (JsonApi.Decode.relationship "creator" res User.userDecoder)


userDecoder : JsonApi.Resource.Resource -> Json.Decode.Decoder User
userDecoder res = 
    Json.Decode.map2 User
        (Json.Decode.succeed (JsonApi.Resource.id res))
        (Json.Decode.field "name" Json.Decode.string)
```

Here is how to use the functions defined above to create a new post and retrieve this
newly created post from the server.

```elm
type Msg =
    OnPostCreated (RemoteData.RemoteData Http.Error.RequestError Post)


type alias Model =
    { post : RemoteData.RemoteData Http.Error.RequestError Post }


newPost : PostPayload
newPost =
    { title = "My new post"
    , body = "This is a new post"
    }


init : ( Model, Cmd Msg )
init =
    ( { post = RemoteData.Loading }
    , createPost OnPostCreated newPost
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        OnPostCreated post ->
            ( { model | post = post }
            , Cmd.none
            )


view : Model -> Html Msg
view model =
    case model.post of
        RemoteData.NotAsked ->
            text "The post will be created soon"

        RemoteData.Loading ->
            text "Still a few seconds to wait!"

        RemoteData.Failure error ->
            viewError error

        RemoteData.Success post ->
            div []
                [ div [ class "post-title" ] [ text post.title ]
                , div [ class "post-body" ] [ text post.body ]
                , div [ class "post-creator" ] [ text post.creator.name ]
                ]


viewError : Http.Error.RequestError -> Html Msg
viewError error =
    case error of
        Http.Error.HttpError httpError ->
            text "This is a HTTP error as we already know them..."

        Http.Error.CustomError msg ->
            text ("Custom error " ++ msg)

        Http.Error.JsonApiError errors ->
            text "This is an error coming from the jsonapi paylaod"


subscriptions : Model -> Sub Msg
subscriptions = 
    Sub.none
```