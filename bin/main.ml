(*
let data = open_in "test.txt" |> In_channel.input_all |> Ut.Fasta.parse

let () =
  List.map snd data
  |> Suffix.lcs
  |> print_endline
   *)

let () =
  Suffix.lcs ["carbonate"; "operate"; "reasonate"; "cooperate"]
  |> print_endline
