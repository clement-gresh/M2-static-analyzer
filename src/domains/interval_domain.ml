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

  let add a b = match a, b with
  | BOT, _ | _, BOT -> BOT
  | INTERVAL(x1, x2), INTERVAL(y1, y2) -> INTERVAL(add_borne x1 y1, add_borne x2 y2)

  let sub a b = match a, b with
  | BOT, _ | _, BOT -> BOT
  | INTERVAL(x1, x2), INTERVAL(y1, y2) -> INTERVAL(sub_borne x1 y2, sub_borne x2 y1)

  (*debug start*)
  let bwd_binary = assert false
  let bwd_unary = assert false
  let compare = assert false
  let widen = assert false
  let binary = assert false
  let unary = assert false
  let print = assert false
  let is_bottom = assert false
  let subset = assert false
  let meet = assert false
  let join = assert false
  let rand = assert false
  (*debug end*)

  
end : VALUE_DOMAIN)