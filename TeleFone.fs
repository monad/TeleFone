namespace TeleFone

module Telegram =
  open FSharp.Data
  open FSharp.Data.JsonExtensions

  let JSONEXN = "An issue was encountered parsing the JSON object."
  let HTTPEXN = "An issue was encountered contacting the server."

  //  (?=) :: JsonValue -> a -> option JsonValue 
  let (?=) (value: JsonValue) prop =
    value.TryGetProperty prop

  //  succ :: int -> int
  let succ x = x + 1

  //  >>= :: (a -> option b) -> option a -> option b
  let (>>=) a f =
    Option.bind f a

  //  <@> :: (a -> b) -> option a -> option b
  let (<@>) f a =
    Option.map f a

  //  getEndpoint :: string -> string
  let getEndpoint token path = 
    "https://api.telegram.org/bot" + token + "/" + path

  //  getUpdates :: string -> option JsonValue 
  let getUpdates token =
    try 
      getEndpoint token "getUpdates" |> 
        Http.RequestString |> JsonValue.Parse |> Some
    with
      | :? System.Net.WebException -> printfn "%s (%s)" HTTPEXN __LINE__ ; None
      | _                          -> printfn "%s (%s)" JSONEXN __LINE__ ; None

  //  getUpdatesO :: string -> int -> option JsonValue
  let getUpdatesO token offset =
    let url = getEndpoint token "getUpdates"
    try
      Http.RequestString (url, query=["offset", offset]) |> JsonValue.Parse |> Some
    with
      | :? System.Net.WebException -> printfn "%s (%s)" HTTPEXN __LINE__ ; None 
      | _                          -> printfn "%s (%s)" JSONEXN __LINE__ ; None

  //  verifyPoll :: JsonValue -> bool
  let verifyPoll (poll: JsonValue) = 
    match (poll ?= "ok") with
      | Some jval -> jval.AsBoolean ()
      | None      -> false

  //  result :: JsonValue -> option JsonValue
  let result (poll: JsonValue) =
    match poll ?= "result" with
      | Some res -> Some <| res.AsArray ()
      | None     -> None

  //  message_id :: JsonValue -> int
  let message_id (element: JsonValue) =
    match (element ?= "message") with
      | Some msg ->
        match (msg ?= "message_id") with
          | Some n -> n.AsInteger ()
          | None   -> 0
      | None    -> 0

  //  sendMessage :: int -> string -> unit
  let sendMessage token cid body =
    let url = getEndpoint token "sendMessage"
    try
      Http.RequestString (url, query=["chat_id", cid; "text", body]) |> ignore 
    with
      | :? System.Net.WebException -> printfn "%s (%s)" HTTPEXN __LINE__ |> ignore
      | _                          -> printfn "%s (%s)" HTTPEXN __LINE__ |> ignore

  //  getNewId :: JsonValue [] -> int
  let getNewId (msgs: JsonValue []) =
    msgs |> Seq.map message_id |> Seq.last |> succ
