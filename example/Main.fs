open TeleFone

(*
 * Bot example. Sends a message when someone speaks.
 *)

module Main =
  open FSharp.Data

  let TOKEN  = ""
  let PREFIX = "/"

  let (<@>) = Telegram.(<@>)
  let (>>=) = Telegram.(>>=)
  let sleep (x: int) = System.Threading.Thread.Sleep x

  //  handler :: JsonValue -> unit
  let handler (message: JsonValue) =
    let cid = Telegram.chat_id message
    let body = Telegram.message_text message

    match body with
      | msg when msg = PREFIX + "hi" -> 
          Telegram.sendMessage TOKEN cid "hi"
      | msg when msg = PREFIX + "me" ->
          Telegram.sendMessage TOKEN cid (sprintf "hi %d" <| Telegram.from_id message)
      | _ -> ignore ()

  // --

  let rec mainLoop (offset: int) =
    let newUpdate = Telegram.getUpdatesO TOKEN offset 
    let newOffset = (newUpdate >>= Telegram.result >>= Telegram.getNewId)

    sleep 100 

    match (newUpdate >>= Telegram.result) with
      | Some results -> Seq.iter (fun res -> handler res) results
      | None         -> ignore ()

    match newOffset with
      | Some nO -> mainLoop nO
      | None    -> mainLoop 0 

  [<EntryPoint>]
  let main args =
    let initial = Telegram.getUpdates TOKEN 

    match (initial >>= Telegram.result >>= Telegram.getNewId) with
      | Some x -> mainLoop x 
      | None   -> mainLoop 0 
    0
