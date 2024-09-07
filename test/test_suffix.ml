open Suffix

let suffixes str =
  let rec aux seq =
    if Seq.is_empty seq then
      Seq.return seq
    else
      Seq.cons seq (aux @@ Seq.drop 1 seq)
  in
  aux @@ String.to_seq str
  |> Seq.map String.of_seq

let test str =
  let tree = suffix @@ Astr.of_string str in
  let sufs = suffixes str in
  Seq.for_all (is_suffix tree) sufs

let () = Printexc.record_backtrace true

let () =
  [
    test "cacao";
    test "absbdbfnfr";
    test "AACCGTATA";
  ]
  |> List.iter (fun b ->
      if not b then failwith "test")
