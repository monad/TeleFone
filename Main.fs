namespace TeleFone

module Telegram =
  open FSharp.Data
  open FSharp.Data.JsonExtensions

  let JSONEXN = "An issue was encountered parsing the JSON object."
  let HTTPEXN = "An issue was encountered contacting the server."

  //  (?=) :: JsonValue -> a -> Some JsonValue | None
  let (?=) (value: JsonValue) prop =
    value.TryGetProperty prop

  //  getEndpoint :: string -> string
  let getEndpoint token path = 
    "https://api.telegram.org/bot" + token + "/" + path

  //  getUpdates :: string -> Some JsonValue | None
  let getUpdates token =
    try 
      getEndpoint token "getUpdates" |> 
        Http.RequestString |> JsonValue.Parse |> Some
    with
      | :? System.Net.WebException -> printfn "%s" HTTPEXN ; None
      | _                          -> printfn "%s" JSONEXN ; None

  //  verifyPoll :: JsonValue -> bool
  let verifyPoll (poll: JsonValue) = 
    match (poll ?= "ok") with
      | Some jval -> jval.AsBoolean ()
      | None      -> false

  //  result :: JsonValue -> [JsonValue]
  let result (poll: JsonValue) =
    poll ?= "result"

module Main =
  [<EntryPoint>]
  let main args =
    let update = Telegram.getUpdates ""
    match Option.map Telegram.result update with
      | Some res -> printfn "%O" res
      | None     -> printfn "No results, or malformed update."
    0
