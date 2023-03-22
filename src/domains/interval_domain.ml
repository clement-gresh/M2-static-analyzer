open Abstract_syntax_tree
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

  let print_borne a = match a with
  | NEG_INF -> "-∞"
  | POS_INF -> "+∞"
  | Cst x -> Z.to_string x

  let neg_borne a = match a with
  | POS_INF -> NEG_INF
  | NEG_INF -> POS_INF
  | Cst x -> Cst (Z.neg x)

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
  | Cst x, Cst y when x > y -> Cst y
  | x, _ -> x
  
  let max_borne a b = match a, b with
  | POS_INF, _ | _, POS_INF -> POS_INF
  | NEG_INF, x | x, NEG_INF -> x
  | Cst x, Cst y when x > y -> Cst x
  | _, x -> x

  let geq_borne a b = match a, b with
  | POS_INF, _ | _, NEG_INF -> true
  | NEG_INF, _ | _, POS_INF -> false
  | Cst x, Cst y when x >= y -> true
  | _, _ -> false

  let eq_borne (b1:borne) (b2:borne) : bool =
    match b1, b2 with
    | Cst x, Cst y -> Z.equal x y
    | POS_INF, POS_INF -> true
    | NEG_INF, NEG_INF -> true
    | _, _ -> false 

  let div_born a b = match a,b  with
    | Cst x, Cst y -> Cst (Z.div x y)
    | x,NEG_INF | x,POS_INF -> Cst Z.zero
    | POS_INF, Cst x -> if x > Z.zero then
          POS_INF
            else
          NEG_INF				
      | NEG_INF, Cst x -> if x > Z.zero then
          NEG_INF
            else
          POS_INF	
  
          let mul_born a b = match a,b  with
          | Cst x, Cst y -> Cst (Z.mul x y)
          | NEG_INF,NEG_INF -> POS_INF 
          | POS_INF,POS_INF -> POS_INF 
          | NEG_INF, POS_INF | POS_INF,NEG_INF -> NEG_INF
          | NEG_INF, Cst x | Cst x,NEG_INF ->
              if x = Z.zero then
                  Cst Z.zero
              else 
                  if x < Z.zero then
                      POS_INF
                  else
                      NEG_INF
          | POS_INF, Cst x | Cst x,POS_INF ->
              if x = Z.zero then
                  Cst Z.zero
              else 
                  if x < Z.zero then
                      NEG_INF
                  else
                      POS_INF
  
  let gt_borne a b = match a, b with
  | x, Cst y -> geq_borne x (add_borne (Cst y) (Cst Z.one))
  | x, y -> geq_borne x y   (*debug : is POS_INF strictly greater than POS_INF ?*)
  (* | NEG_INF, _ | _, POS_INF -> true
  | POS_INF, _ | _, NEG_INF -> false
  | Cst x, Cst y when x < y -> true
  | _, _ -> false *)
    
  let eq (i1:t) (i2:t) : bool =
    match i1, i2 with
    | INTERVAL (b1, b2), INTERVAL (b3, b4) -> eq_borne b1 b3 && eq_borne b2 b4
    | BOT, BOT -> true
    | _, _ -> false 

  let neq (d1:t) (d2:t) : bool =
    not (eq d1 d2)

  let gt_zero a = match a with
	| Cst c -> c > Z.zero
	| _ -> false

  let mul x y = match x,y with
  | BOT,_ | _, BOT -> BOT
  | INTERVAL(a, b), INTERVAL(c, d) ->
    let ac = mul_born a c in
    let ad = mul_born a d in
    let bc = mul_born b c in
    let bd = mul_born b d in
    let borne_inf = min (min ac ad) (min bc bd) in
    let borne_max = max (max ac ad) (max bc bd) in
    INTERVAL(borne_inf, borne_max)

let div x y = match x,y with
  | BOT,_ | _,BOT -> BOT
  | INTERVAL (_, _), INTERVAL(Cst x1, _) when x1 = Z.zero -> BOT
  | INTERVAL (_, _), INTERVAL(_, Cst x1) when x1 = Z.zero -> BOT
  | INTERVAL(a,b), INTERVAL(c,d) -> 
        let ac = div_born a c in
        let ad = div_born a d in
        let bc = div_born b c in
        let bd = div_born b d in
        let borne_inf = if gt_zero c then
                min ac ad else min bc bd in
        let borne_sup = if gt_zero c then
                min bc bd else max ac ad in
        INTERVAL(borne_inf,borne_sup)

  let print fmt x = match x with
  | BOT -> Format.fprintf fmt "⊥"
  | INTERVAL(x, y) -> Format.fprintf fmt "[%s;%s]" (print_borne x) (print_borne y)

  let subset a b = match a,b with
  | BOT, _ | _, INTERVAL(NEG_INF, POS_INF) -> true
  | INTERVAL(x1, x2), INTERVAL(y1, y2) when geq_borne x1 y1 && geq_borne y2 x2 -> true
  | _, _ -> false

  let neg a = match a with
  | INTERVAL(x, y) -> INTERVAL(neg_borne y, neg_borne x)
  | _ -> BOT

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

  let leq a b = match a, b with
  | INTERVAL(x1, x2), INTERVAL(y1, y2) when geq_borne y2 x1 -> INTERVAL(x1, min_borne x2 y2), INTERVAL(max_borne x1 y1, y2)
  | _, _ -> BOT, BOT

  (*debug : à vérifier*)
  let lt a b = match a, b with
  | INTERVAL(x1, x2), INTERVAL(y1, y2) when gt_borne y2 x1 -> INTERVAL(x1, min_borne x2 (sub_borne y2 (Cst Z.one))),
                                                              INTERVAL(max_borne (add_borne x1 (Cst Z.one)) y1, y2)
  | _, _ -> BOT, BOT

  let geq (a:t) (b:t) : t*t = match a, b with
  | INTERVAL(x1, x2), INTERVAL(y1, y2) when geq_borne x2 y1 -> INTERVAL(max_borne x1 y1, x2), INTERVAL(y1, min_borne x2 y2)
  (*| INTERVAL(x1, x2), INTERVAL(y1, y2) -> BOT, BOT
  | BOT, _ | _, BOT -> BOT, BOT*)
  | _, _ -> BOT, BOT

  (*debug : à vérifier*)
  let gt (a:t) (b:t) : t*t = match a, b with
  (* | INTERVAL(x1, x2), INTERVAL(y1, y2) -> geq INTERVAL(add_borne x1 (Cst Z.one), add_borne x2 (Cst Z.one)) INTERVAL(y1, y2) *)
  | INTERVAL(x1, x2), INTERVAL(y1, y2) when gt_borne x2 y1 -> INTERVAL(max_borne x1 (add_borne y2 (Cst Z.one)), x2),
                                                        INTERVAL(y1, max_borne (sub_borne x2 (Cst Z.one)) y2)
  | _, _ -> BOT, BOT
  
  (* operator dispatch *)
  let widen = join
  let rand x y =
    if x=y then const x (* [x,x] *)
    else if x<y then INTERVAL(Cst x, Cst y) (* [x ,y]*)
    else BOT
  let is_bottom a =
    a=BOT
  let unary x op = match op with
  | AST_UNARY_PLUS  -> x
  | AST_UNARY_MINUS -> neg x

  let binary x y op = match op with
  | AST_PLUS     -> add x y
  | AST_MINUS    -> sub x y
  | AST_MULTIPLY -> mul x y
  | AST_DIVIDE   -> div x y

  let compare x y op = match op with
  | AST_EQUAL         -> eq x y
  | AST_NOT_EQUAL     -> neq x y
  | AST_GREATER_EQUAL -> geq x y
  | AST_GREATER       -> gt x y
  | AST_LESS_EQUAL    -> let y',x' = geq y x in x',y'
  | AST_LESS          -> let y',x' = gt y x in x',y'

  let bwd_unary x op r = match op with
  | AST_UNARY_PLUS  -> meet x r
  | AST_UNARY_MINUS -> meet x (neg r)

  let bwd_binary x y op r = match op with

  | AST_PLUS ->
      (* r=x+y => x=r-y and y=r-x *)
      meet x (sub r y), meet y (sub r x)

  | AST_MINUS ->
      (* r=x-y => x=y+r and y=x-r *)
      meet x (add y r), meet y (sub x r)

  | AST_MULTIPLY ->
      (* r=x*y => (x=r/y or y=r=0) and (y=r/x or x=r=0)  *)
      let contains_zero o = subset (const Z.zero) o in
      (if contains_zero y && contains_zero r then x else meet x (div r y)),
      (if contains_zero x && contains_zero r then y else meet y (div r x))

  | AST_DIVIDE ->
      (* this is sound, but not precise *)
      x, y
  
end : VALUE_DOMAIN)