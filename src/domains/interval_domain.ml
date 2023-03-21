(*open Abstract_syntax_tree*)
open Value_domain

module Intervals = (struct

  type borne =
  | Cst of Z.t
  | POS_INF
  | NEG_INF

  type t =
  | INTERVAL of borne * borne
  | BOT

  let bottom = BOT
  
  let top = INTERVAL(NEG_INF, POS_INF)
  
  let const c = INTERVAL(Cst c, Cst c)

  let add_borne a b = match a, b with
  | Cst x, Cst y -> Cst (Z.add x y)
  | POS_INF, _ | _, POS_INF -> POS_INF
  (*| NEG_INF, _ | _, NEG_INF -> NEG_INF*)
  | _, _ -> NEG_INF

  let sub_borne a b = match a, b with
  | Cst x, Cst y -> Cst (Z.sub x y)
  | NEG_INF, _ | _, POS_INF -> NEG_INF
  (*| _, POS_INF | NEG_INF, _ -> POS_INF*)
  | _, _ -> POS_INF

  let min_borne a b = match a, b with
  | NEG_INF, _ | _, NEG_INF -> NEG_INF
  | POS_INF, x | x, POS_INF -> x
  | x, y when x > y -> y
  | x, _ -> x
  
  let max_borne a b = match a, b with
  | POS_INF, _ | _, POS_INF -> POS_INF
  | NEG_INF, x | x, NEG_INF -> x
  | x, y when x > y -> x
  | _, x -> x

  let add a b = match a, b with
  | BOT, _ | _, BOT -> BOT
  | INTERVAL(x1, x2), INTERVAL(y1, y2) -> INTERVAL(add_borne x1 y1, add_borne x2 y2)

  let sub a b = match a, b with
  | BOT, _ | _, BOT -> BOT
  | INTERVAL(x1, x2), INTERVAL(y1, y2) -> INTERVAL(sub_borne x1 y2, sub_borne x2 y1)
  
  let join a b = match a, b with
  | BOT, x | x, BOT -> x
  | INTERVAL(x1, x2), INTERVAL(y1, y2) -> INTERVAL(min_borne x1 y1, max_borne x2 y2)

  let meet a b = match a, b with
  | INTERVAL(x1, x2), INTERVAL(y1, y2) when x2 >= y1 || x1 <= y2 -> INTERVAL(max_borne x1 y1, min_borne x2 y2)
  (*| BOT, _ | _, BOT -> BOT
  | INTERVAL(x1, x2), INTERVAL(y1, y2) when x2 < y1 || x1 > y2 -> BOT*)
  | _, _ -> BOT

  let print_borne a = match a with
  | NEG_INF -> "-∞"
  | POS_INF -> "+∞"
  | Cst x -> Z.to_string x
  
  let print fmt x = match x with
  | BOT -> Format.fprintf fmt "⊥"
  | INTERVAL(x, y) -> Format.fprintf fmt "[%s, %s]" (print_borne x) (print_borne y)

  let geq (a:t) (b:t) : t*t = match a, b with
  | INTERVAL(x1, x2), INTERVAL(y1, y2) when x1 <= y2 -> INTERVAL(x1, min_borne x2 y2), INTERVAL(max_borne x1 y1, y2)
  (*| INTERVAL(x1, x2), INTERVAL(y1, y2) -> BOT, BOT
  | BOT, _ | _, BOT -> BOT, BOT*)
  | _, _ -> BOT, BOT

  (*debug : à vérifier*)
  let gt (a:t) (b:t) : t*t = match a, b with
  (* | INTERVAL(x1, x2), INTERVAL(y1, y2) -> geq INTERVAL(add_borne x1 (Cst Z.one), add_borne x2 (Cst Z.one)) INTERVAL(y1, y2) *)
  | INTERVAL(x1, x2), INTERVAL(y1, y2) when x1 < y2 -> INTERVAL(x1, min_borne x2 (sub_borne y2 (Cst Z.one))),
                                                        INTERVAL(max_borne (add_borne x1 (Cst Z.one)) y1, y2)
  | _, _ -> BOT, BOT


  (*debug start*)
  let bwd_binary = assert false
  let bwd_unary = assert false
  let compare = assert false
  let widen = assert false
  let binary = assert false
  let unary = assert false
  let is_bottom = assert false (*ok*)
  let subset = assert false (*subset a b checks if a is included in b*)
  let rand = assert false
  (*debug end*)

  
end : VALUE_DOMAIN)