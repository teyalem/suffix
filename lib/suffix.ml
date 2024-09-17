type word = int * int

module Token = struct
  type t =
    | Char of char
    | End of int
end

module Astr = struct
  type t = Token.t array

  let length = Array.length

  let sub_word s (k, p) =
    Array.sub s k (p-k+1)
    |> Array.to_seq
    |> Seq.map (function
        | Token.Char c -> c
        | End _ -> '$')
    |> String.of_seq

  let of_string str =
    let str =
      String.to_seq str
      |> Seq.map (fun c -> Token.Char c)
    in
    Seq.(append str (return @@ Token.End 1))
    |> Array.of_seq

  let of_strings ss =
    let rec aux i = function
      | [] -> []
      | x :: xs ->
        x @ (Token.End i :: aux (i+1) xs)
    in
    List.map (fun s ->
        String.to_seq s
        |> Seq.map (fun c -> Token.Char c)
        |> List.of_seq)
      ss
    |>  aux 0
    |> Array.of_list

  (*
  let of_seq = Array.of_seq
     *)
  let to_seq = Array.to_seq

end

type node = {
  name : string;
  at : int;
  mutable g : (word * node) list;
  mutable f : node option;
}

type t = {
  str : Astr.t;
  root : node;
}

(** helper functions *)

let create_node name at =
  { name;
    at;
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
      k < 0 || str.(k) = t)
    n.g

let mem_transition str n t =
  List.exists (fun ((k, _), _) ->
      k < 0 || str.(k) = t)
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

let export_dot { str; root } =
  let open Printf in
  let buf = Buffer.create 100 in
  let cnt = ref 0 in

  let rec aux name node =
    if List.is_empty node.g then
      bprintf buf "%s[label=\"%d\"]\n" name node.at
    else
      List.iter (fun ((k, p), s) ->
          let sub_name = sprintf "N%d" !cnt in
          incr cnt;
          let substr =
            try Astr.sub_word str (k, p)
            with _ -> sprintf "%d, %d" k p
          in
          bprintf buf "%s -> %s [label=\"%s\"]\n" name sub_name substr;
          aux sub_name s)
        node.g
  in

  bprintf buf "digraph G {\n";
  aux root.name root;
  bprintf buf "}";
  Buffer.contents buf

let suffix str =
  let str_end = Astr.length str - 1 in

  let fal = create_node "fal" 0 in
  let root = create_node "Root" 0 in
  any_transition fal root;
  set_suffix root fal;

  let canonize s (k, p) =
    let rec aux s k =
      if p < k then s, k
      else
        let (nk, np), ns = find_transition str s str.(k) in
        if np - nk <= p - k then
          aux ns (k + np - nk + 1)
        else
          s, k
    in
    aux s k
  in

  let test_and_split s (k, p) c =
    if k <= p then
      let (nk, np), ns = find_transition str s str.(k) in
      if c = str.(nk+p-k+1) then true, s
      else begin
        let r = create_node "" ~-1 in
        update_node s (nk, nk+p-k) r;
        update_node r (nk+p-k+1, np) ns;
        false, r
      end
    else
      mem_transition str s c, s
  in

  (* TODO: refactor update *)
  let update s (k, i) =
    let oldr = ref root in
    let rec aux (s, k) (end_point, r) =
      if end_point then s, k
      else begin
        update_node r (i, str_end) (create_node "" k);
        if !oldr != root then set_suffix !oldr r;
        oldr := r;
        let s, k = canonize (suffix_of s) (k, i-1) in
        aux (s, k) (test_and_split s (k, i-1) str.(i))
      end
    in
    let s, k = aux (s, k) (test_and_split s (k, i-1) str.(i)) in
    if !oldr != root then set_suffix !oldr s;
    s, k
  in

  let rec build s k i =
    if i < Astr.length str then
      let s, k = update s (k, i) in
      let s, k = canonize s (k, i) in
      build s k (i+1)
  in
  build root 0 0;
  { str; root }

let is_suffix { str; root } suf =
  let suf = Astr.of_string suf in
  let rec aux s k =
    if mem_transition str s suf.(k) then
      let (nk, np), ns = find_transition str s suf.(k) in
      if List.is_empty ns.g then
        true
      else
        aux ns (k+np-nk+1)
    else
      false
  in
  aux root 0

let lcs ss =
  let l = List.length ss in
  let str = Astr.of_strings ss in

  let ends =
    Astr.to_seq str
    |> Seq.mapi (fun i x -> i, x)
    |> Seq.filter_map (fun (i, x) ->
        match x with
        | Token.End _ -> Some i
        | _ -> None)
    |> List.of_seq
  in
  let ends = -1 :: ends in

  let prune_invalid { root; _ } =
    let clamp (k, p) =
      let rec aux = function
        | [] | [_] -> failwith "clamp"
        | a :: b :: ns ->
          if a <= k && k <= b then
            min p b
          else
            aux (b :: ns)
      in
      aux ends
    in

    let rec aux node =
      node.g <-
        List.filter_map (fun ((k, p), s) ->
            let np = clamp (k, p) in
            if np = p then begin
              aux s;
              Some ((k, p), s)
            end
            else if List.is_empty s.g then
              Some ((k, np), s)
            else
              None)
          node.g
    in
    aux root;
    { str; root }
  in

  let { str; root } =
    str
    |> suffix 
    |> prune_invalid
  in

  let module S = Set.Make(Int) in
  let every_str =
    Seq.init l Fun.id
    |> S.of_seq
  in

  let rec aux node word =
    let partials, commons =
    node.g
    |> List.partition_map (fun ((k, p), s) ->
        match str.(p) with
        | Token.End i ->
          Either.Left (S.singleton i)
        | _ ->
          aux s @@
          word ^ Astr.sub_word str (k, p))
    in

    if List.is_empty commons then
      let set =
        List.fold_left S.union S.empty partials
      in
      if S.equal set every_str then
        Either.Right [word]
      else
        Left set
    else
      Right (List.concat commons)
  in

  aux root ""
  |> Either.find_right
  |> Option.map (List.fold_left (fun a s ->
      if String.(length a >= length s) then
        a
      else
        s)
      "")
