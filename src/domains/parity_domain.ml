open Abstract_syntax_tree

module Parity = (struct
  (* create methods is_even, etc.
     need to add the method in value_domain  *)

  type t =
  | BOT
  | EVEN
  | ODD
  | TOP

  let bottom = BOT

  let top = TOP
  
  let const c =
    let rest = Z.rem c (Z.add Z.one Z.one) in   (*faire des tests avec Z.mod (a Z.mod b), Z.rem et Z.rest*)
    if rest=Z.zero then EVEN
    else ODD
  
  let print fmt x = match x with
  | BOT -> Format.fprintf fmt "⊥"
  | TOP -> Format.fprintf fmt "⊤"
  | EVEN -> Format.fprintf fmt "even"
  | ODD -> Format.fprintf fmt "odd"

  let rand x y =
    if x=y then const x
    else if x < y then TOP
    else BOT

  let neg x = x

  let add x y = match x, y with
  | _,BOT | BOT,_ -> BOT
  | _,TOP | TOP,_ -> TOP
  | EVEN,EVEN | ODD,ODD -> EVEN
  | EVEN,ODD | ODD,EVEN -> ODD

  let sub x y = add x y

  let mul x y = match x, y with
  | _,BOT | BOT,_ -> BOT
  | _,TOP | TOP,_ -> TOP
  | EVEN,_ | _,EVEN -> EVEN
  | ODD,ODD -> ODD

  let div x y = match x, y with
  | _,BOT | BOT,_ -> BOT
  | _,_ -> TOP

  let join x y = match x, y with
  | a,BOT | BOT,a -> a
  | EVEN,EVEN -> EVEN
  | ODD,ODD -> ODD
  (* | _,TOP | TOP,_ -> TOP
  | EVEN,ODD | ODD,EVEN -> TOP *)
  | _,_ -> TOP

  let meet x y = match x, y with
  | _,BOT | BOT,_ -> BOT
  | a,TOP | TOP,a -> a
  | EVEN,EVEN -> EVEN
  | ODD,ODD -> ODD
  | EVEN,ODD | ODD,EVEN -> BOT
  
  let eq x y = meet x y, meet x y

  let neq x y = match x, y with
  | BOT,_ | _,BOT -> BOT,BOT
  | a, b -> a, b

  let geq x y = match x, y with
  | BOT,_ | _,BOT -> BOT,BOT
  | a, b -> a, b

  let gt x y = match x, y with
  | BOT,_ | _,BOT  -> BOT,BOT
  | a, b -> a, b

  let subset x y = match x, y with
  | BOT,_ | _,TOP -> true
  | EVEN,EVEN | ODD,ODD -> true
  (* | TOP,_ | _,BOT -> false
  | EVEN,ODD | ODD,EVEN -> false *)
  | _,_ -> false

  let is_bottom x = x=BOT

  (* no need for a widening as the domain has finite height; we use the join *)
  let widen = join

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
    let contains_zero o = match o with (* debug : is this right ? *)
    | TOP -> true
    | _ -> false
    in
    (if contains_zero y && contains_zero r then x else meet x (div r y)),
    (if contains_zero x && contains_zero r then y else meet y (div r x))

  | AST_DIVIDE ->
    (* this is sound, but not precise *)
    x, y

end)
 