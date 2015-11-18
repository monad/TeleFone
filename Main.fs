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

  //  getEndpoint :: string -> string
  let getEndpoint token path = 
    "https://api.telegram.org/bot" + token + "/" + path

  //  getUpdates :: string -> option JsonValue 
  let getUpdates token =
    try 
      getEndpoint token "getUpdates" |> 
        Http.RequestString |> JsonValue.Parse |> Some
    with
      | :? System.Net.WebException -> printfn "%s" HTTPEXN ; None
      | _                          -> printfn "%s" JSONEXN ; None

  //  getUpdatesO :: string -> int -> option JsonValue
  let getUpdatesO token offset =
    try
      let soffset = sprintf "%d" offset
      getEndpoint token "getUpdates" |>
        (fun url -> Http.RequestString(url, query=["offset", soffset]))
    with
      | :? System.Net.WebException -> printfn "%s" HTTPEXN ; "" 
      | _                          -> printfn "something happened." ; "" 

  //  verifyPoll :: JsonValue -> bool
  let verifyPoll (poll: JsonValue) = 
    match (poll ?= "ok") with
      | Some jval -> jval.AsBoolean ()
      | None      -> false

  //  result :: JsonValue -> option JsonValue
  let result (poll: JsonValue) =
    poll ?= "result"

  //  message_id :: JsonValue -> option int
  let message_id msg =
    msg ?= "message_id" |> Option.map (fun (x: JsonValue) -> x.AsInteger ())
    

  //  newUpdateId :: JsonValue -> int
  //  let newUpdateId (msgs: JsonValue) =

module Main =
  [<EntryPoint>]
  let main args =
    let token = ""
    let update = Telegram.getUpdatesO token 10
    printfn "%d" <| newUpdateId update
    0
