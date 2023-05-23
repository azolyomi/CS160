open Utils

(** Reference implementation of regex semantics. *)
module Make (A : Alphabet.S) :
  Sig.Semantics with type symbol = A.t and type program = Syntax.Make(A).program =
struct
  (* include regex syntax *)
  module Syntax = Syntax.Make (A)
  include Syntax

  type input = word [@@deriving show]
  type output = bool [@@deriving show]

  (** Return true iff regex r matches input w. *)
    let rec split (left: input) (right : input) : (input * input) list =
      (* shift letters from right to left until right is empty *)
      (* return cat of all of those, e.g. *)
      (* [], abcd -> [([], abcd), (a, bcd), (ab, cd), (abc, d), (abcd, [])] *)
      (* return ALL such tuples *)
      match right with
      | [] -> [ (left, []) ]
      | s :: right' ->
          let left' = left @ [ s ] in
          (left', right') :: split left' right'
  
    (** Return true iff regex r matches input w. *)
    let rec interpret (w : input) (r : program) : output =
      match r with
      | Void -> false
      | Lit s -> (
          match w with
          | [] -> A.is_epsilon s
          | [ s' ] -> A.equal s s'
          | _ -> false)
      | Or (r1, r2) -> interpret w r1 || interpret w r2
      | Cat (r1, r2) -> (
          List.exists (
            fun (w1, w2) -> interpret w1 r1 && interpret w2 r2
          ) (([], w) :: split [] w))
      (*  matches regex of star *)
      | Star r' -> (
          match w with
          | [] -> true
          | _ ->
              List.exists
                (fun (w1, w2) ->
                  interpret w1 r' && interpret w2 (Star r'))
                (split [] w)
      )
end
