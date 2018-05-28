import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Json.Decode as Decode
import Json.Encode as Encode

main : Program Never Model Msg
main =
  Html.program
  -- record som har 4 fields
    { init = start -- parameter = function
    , view = view
    , update = update
    , subscriptions = subscriptions
    }

url : String -> String
url action = "http://localhost:9000/member/" ++ action

-- MODEL

type alias Model =
  { count : Int
  , message : String
  , member : Member
  }

-- declare a type and call it member(indhold i {} er typen) er også en funktion
type alias Member =
 {
    id : Int
    , name : String
    , email : String
 }

start : (Model, Cmd Msg)
start =
  ( Model 0 "No message" (Member 2 "navn" "email")
  , Cmd.none
  )

-- UPDATE
-- ting som skal kunne køre i update SKAL være skrevet her
type Msg
  = GetMemberCount
  | MemberCountReceived (Result Http.Error Int)
  | GetMember
  | MemberReceived (Result Http.Error Member)
  | IdChanged String
  | NameChanged String
  | EmailChanged String
  | PostMember
  | MemberPosted (Result Http.Error Member)

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    GetMemberCount ->
      (model, getMemberCount)

    MemberCountReceived (Ok newCount) ->
      ( { model | count = newCount }, Cmd.none)

    MemberCountReceived (Err error) ->
      ( { model | message = toString error }, Cmd.none)

    GetMember ->
        (model, getMember) -- når denne kaldes kører metoden nede fra subscriptions

    MemberReceived (Ok member)->
        ({model | member = member}, Cmd.none)

    MemberReceived (Err error) ->
        ( { model | message = toString error }, Cmd.none)

    IdChanged idChanged ->
     case String.toInt idChanged of
        Ok id ->
         ({model | member = (Member id model.member.name model.member.email)},Cmd.none)
        Err _ ->
         ({model| member = (Member 0 model.member.name model.member.email)}, Cmd.none)

    NameChanged nameChanged ->
     let
        member = model.member
     in
        ({model | member = {member | name = nameChanged}}, Cmd.none)-- member of the model. take the model and change that member

    EmailChanged emailChanged ->
     let
        member = model.member
     in
        ({model | member = {member | email = emailChanged}}, Cmd.none) -- member of the model. take the model and change that member

    PostMember ->
     (model, postMember model.member)

    MemberPosted (Ok member)->
     ({model | member = member}, getMemberCount)

    MemberPosted (Err error)->
     ({model | message = toString error}, getMemberCount)



-- VIEW

view : Model -> Html Msg
view model =
  div []
    [ h2 [] [text ("Member Count = " ++ toString model.count) ]
    , button [ onClick GetMemberCount ] [ text "Update Member Count" ]
    , button [ onClick GetMember ] [ text "Get Member" ]
    , button [ onClick PostMember ] [ text "Post Member" ]
    , hr [] []
    , input [type_ "text", value (toString model.member.id), onInput IdChanged][] -- runtime kalder på denne funktion når der sker noget i input
    , input [type_ "text", value model.member.name, onInput NameChanged][] -- onInput constructs an
    , input [type_ "text", value model.member.email, onInput EmailChanged][]
    , text model.member.name
    , text model.message
    ]

-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model = Sub.none

-- HTTP
--Function som konverterer JSON om til Elm
memberDecoder : Decode.Decoder Member -- ny decoder som leder efter felt og gør det som vi vil have det
memberDecoder =
    Decode.map3 Member --constructor to member
    -- decoder til a, b , c for at kunne vælge mellem nøgler(venstre) og værdier(højre)
    (Decode.field "id" Decode.int)
    (Decode.field "name" Decode.string)
    (Decode.field "email" Decode.string)


-- Function som konverterer Elm til JSON
-- et træ som er bygget op af keys og values
memberEncoder : Member -> Encode.Value
memberEncoder member =
    Encode.object
    [("id", Encode.int member.id) -- bliver lavet om til en value
    , ("name", Encode.string member.name)
    , ("email", Encode.string member.email)
    ]

memberJsonBody : Member -> Http.Body
memberJsonBody member =
    Http.jsonBody <| memberEncoder member

getMember : Cmd Msg
getMember =
    Http.send MemberReceived (Http.get (url "1") memberDecoder)  -- kalder funktionen MemberReceived i update


getMemberCount : Cmd Msg
getMemberCount =
    Http.send MemberCountReceived (Http.get (url "count") Decode.int)

postMember : Member -> Cmd Msg
postMember member =
    Http.send MemberPosted (Http.post (url "") (memberJsonBody member) memberDecoder)
