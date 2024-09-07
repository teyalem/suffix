module S = Set.Make(Char)

type word = int * int

let inf = Int.max_int

type node = {
  name : string;
  mutable g : (word * node) list;
  mutable f : node option;
}

let create_node name =
  { name;
    g = [];
    f = None; }

let set_suffix n s =
  n.f <- Some s

let update_node s (k, p) r =
  let rec aux mem = function
    | [] ->
      if mem then []
      else [(k, p), r]
    | ((ok, _), _) as x :: xs ->
      if ok = k then
        ((k, p), r) :: aux true xs
      else
        x :: aux mem xs
  in
  s.g <- aux false s.g

let any_transition s r =
  s.g <- [(-1, -1), r]

let find_transition str n t =
  List.find (fun ((k, _), _) ->
      k < 0 || str.[k] = t)
    n.g

let mem_transition str n t =
  List.exists (fun ((k, _), _) ->
      k < 0 || str.[k] = t)
    n.g

let suffix_of s =
  try
    Option.get s.f
  with e ->
    Printf.eprintf "suffix not exists on: %s\n"
      s.name;
    List.iter (fun ((k, p), ns) ->
        Printf.eprintf "[%d, %d] %s\n" k p ns.name)
      s.g;
    raise e

let _export_dot str tree =
  let open Printf in
  let cnt = ref 0 in
  let rec aux name node =
    List.iter (fun ((k, p), s) ->
        let p =
          if p = inf then String.length str - 1
          else p
        in
        let sub_name = sprintf "N%d" !cnt in
        incr cnt;
        let substr =
          try String.sub str k (p-k+1)
          with _ -> sprintf "%d, %d" k p
        in
        printf "%s -> %s [label=%s]\n" name sub_name substr;
        aux sub_name s)
      node.g
  in
  printf "digraph G {\n";
  aux tree.name tree;
  printf "}"

let suffix str =
  let fal = create_node "fal" in
  let root = create_node "Root" in
  any_transition fal root;
  set_suffix root fal;

  let canonize s (k, p) =
    let rec aux s k =
      if p < k then s, k
      else
        let (nk, np), ns = find_transition str s str.[k] in
        if np - nk <= p - k then
          aux ns (k + np - nk + 1)
        else
          s, k
    in
    aux s k
  in

  let test_and_split s (k, p) c =
    if k <= p then
      let (nk, np), ns = find_transition str s str.[k] in
      if c = str.[nk+p-k+1] then true, s
      else begin
        let r = create_node "" in
        update_node s (nk, nk+p-k) r;
        update_node r (nk+p-k+1, np) ns;
        false, r
      end
    else
      mem_transition str s c, s
  in

  let update s (k, i) =
    let oldr = ref root in
    let rec aux (s, k) (end_point, r) =
      if end_point then s, k
      else begin
        update_node r (i, inf) (create_node "");
        if !oldr != root then set_suffix !oldr r;
        oldr := r;
        let s, k = canonize (suffix_of s) (k, i-1) in
        aux (s, k) (test_and_split s (k, i-1) str.[i])
      end
    in
    let s, k = aux (s, k) (test_and_split s (k, i-1) str.[i]) in
    if !oldr != root then set_suffix !oldr s;
    s, k
  in

  let rec aux s k i =
    if i < String.length str then
      let s, k = update s (k, i) in
      let s, k = canonize s (k, i) in
      aux s k (i+1)
  in
  aux root 0 0;
  root

let is_suffix (str, tree) suf =
  let len = String.length suf in
  let rec aux s k =
    if k = len then true
    else if mem_transition str s suf.[k] then
      let (nk, np), ns = find_transition str s suf.[k] in
      if List.is_empty ns.g then
        true
      else
        aux ns (k+np-nk+1)
    else
      false
  in
  aux tree 0

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
  let tree = suffix str in
  let sufs = suffixes str in
  Seq.for_all (is_suffix (str, tree)) sufs

let () = Printexc.record_backtrace true

let () =
  [
    test "cacao";
    test "absbdbfnfr";
    test "AACCGTATA";
  ]
  |> List.iter (fun b ->
      if not b then failwith "test")
