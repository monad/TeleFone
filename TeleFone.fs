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
  let getUpdatesO (token: string) (offset: int) =
    let url     = getEndpoint token "getUpdates"
    let soffset = sprintf "%d" offset
    try
      Http.RequestString (url, query=["offset", soffset]) |> JsonValue.Parse |> Some
    with
      | :? System.Net.WebException -> printfn "%s (%s)" HTTPEXN __LINE__ ; None 
      | _                          -> printfn "%s (%s)" JSONEXN __LINE__ ; None

  //  verifyPoll :: JsonValue -> bool
  let verifyPoll (poll: JsonValue) = 
    match (poll ?= "ok") with
      | Some jval -> jval.AsBoolean ()
      | None      -> false

  //  result :: JsonValue -> option JsonValue []
  let result (poll: JsonValue) =
    match poll ?= "result" with
      | Some res -> Some <| res.AsArray ()
      | None     -> None

  //  update_id :: JsonValue -> int
  let update_id (element: JsonValue) =
    match (element ?= "update_id") with
      | Some id -> id.AsInteger ()
      | None    -> 0

  //  message_body :: JsonValue -> string
  let message_text (element: JsonValue) =
    match (element ?= "message") with
      | Some message -> message.["text"].AsString ()
      | None -> ""

  //  message :: JsonValue -> option JsonValue
  let message (results: JsonValue) =
    results ?= "message"

  //  from_id :: JsonValue -> int
  let from_id (element: JsonValue) =
    match message element with
      | Some msg ->
        match (msg ?= "from") with
          | Some from -> from.["id"].AsInteger ()
          | None -> 0
      | None         -> 0

  //  chat_id :: JsonValue -> int
  let chat_id (element: JsonValue) =
    match message element with
      | Some msg -> 
        match (msg ?= "chat") with
          | Some chat -> chat.["id"].AsInteger ()
          | None -> 0
      | None     -> 0
    

  //  message_id :: JsonValue -> int
  let message_id (element: JsonValue) =
    match message element  with
      | Some msg ->
        match (msg ?= "message_id") with
          | Some n -> n.AsInteger ()
          | None   -> 0
      | None     -> 0

  //  sendMessage :: string -> int -> string -> unit
  let sendMessage (token: string) (cid: int) (body: string) =
    let url  = getEndpoint token "sendMessage"
    let scid = sprintf "%d" cid
    try
      Http.RequestString (url, query=["chat_id", scid; "text", body]) |> ignore 
    with
      | :? System.Net.WebException -> printfn "%s (%s)" HTTPEXN __LINE__ |> ignore
      | _                          -> printfn "%s (%s)" HTTPEXN __LINE__ |> ignore

  //  getNewId :: JsonValue [] -> option int
  let getNewId (msgs: JsonValue []) =
    match Seq.isEmpty msgs with
      | true -> None
      | false -> msgs |> Seq.map update_id |> Seq.last |> succ |> Some
