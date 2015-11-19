open TeleFone

module Main =
  [<EntryPoint>]
  let main args =
    let token = ""
    let update = Telegram.getUpdates token
    let (<@>) = Telegram.(<@>)
    let (>>=) = Telegram.(>>=)
    match Telegram.getNewId <@> (update >>= Telegram.result) with
      | Some x -> printfn "update %O\nid: %d" update x
      | None   -> printfn ":["
    0
