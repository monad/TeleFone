open TeleFone

(*
 * Bot example. Sends a message when someone speaks.
 *)

module Main =
  let TOKEN = ""
  let GROUP = ""

  let (<@>) = Telegram.(<@>)
  let (>>=) = Telegram.(>>=)
  let sleep x = System.Threading.Thread.Sleep (x * 1000)

  let rec mainLoop (offset: int) =
    let newUpdate = Telegram.getUpdatesO TOKEN offset 
    let newOffset = (newUpdate >>= Telegram.result >>= Telegram.getNewId)

    sleep 1

    match newOffset with
      | Some nO -> printfn "Got message." ; Telegram.sendMessage TOKEN GROUP "hi" ; mainLoop nO
      | None    -> printfn "No updates." ; mainLoop 0

  [<EntryPoint>]
  let main args =
    let initial = Telegram.getUpdates TOKEN 

    match (initial >>= Telegram.result >>= Telegram.getNewId) with
      | Some x -> mainLoop x 
      | None   -> ignore
    0
