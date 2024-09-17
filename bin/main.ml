let data = open_in "input.txt" |> In_channel.input_all |> Ut.Fasta.parse

let () =
  List.map snd data
  |> Suffix.lcs
  |> Option.iter print_endline
