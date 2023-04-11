open Value_domain
open Value_reduction

module ReducedProduct(R : VALUE_REDUCTION) = (struct
  module A = R.A
  module B = R.B

  type t = R.t (* A.t * B.t *)

  let top = A.top, B.top
  
  let bottom = A.bottom, B.bottom

  (* operator dispatch *)

  let unary (a,b) op =
    let a' = A.unary a op in
    let b' = B.unary b op in
    R.reduce (a', b')

  let binary (x1,y1) (x2,y2) op =
    let x' = A.binary x1 x2 op in
    let y' = B.binary y1 y2 op in
    R.reduce (x', y')

  let compare (x1,y1) (x2,y2) op =
    let x1', x2' = A.compare x1 x2 op in
    let y1', y2' = B.compare y1 y2 op in
    R.reduce (x1', y1'), R.reduce (x2', y2')

  let bwd_unary (a,b) op (r1,r2) =
    let a' = A.bwd_unary a op r1 in
    let b' = B.bwd_unary b op r2 in
    R.reduce (a', b')

  let bwd_binary (x1,y1) (x2,y2) op (xr,yr) =
    let x1', x2' = A.bwd_binary x1 x2 op xr in
    let y1', y2' = B.bwd_binary y1 y2 op yr in
    R.reduce (x1', y1'), R.reduce (x2', y2')

  (* constant *)
  let const x =
    let x1 = A.const x in
    let x2 = B.const x in
    R.reduce(x1, x2)

  (* interval *)
  let rand x1 x2 =
    let x = A.rand x1 x2 in
    let x' = B.rand x1 x2 in
    R.reduce (x, x')

  (* arithmetic operations *)
(* 
  let neg = assert false

  let add = assert false

  let sub = assert false

  let mul = assert false *)

  (* set-theoretic operations *)

  let join (x1,y1) (x2,y2) = 
    let x = A.join x1 x2 in
    let y = B.join y1 y2 in
    R.reduce (x, y)

  let meet (x1,y1) (x2,y2) = 
    let x = A.meet x1 x2 in
    let y = B.meet y1 y2 in
    R.reduce (x, y)

  let widen (x1,y1) (x2,y2) = 
    let x = A.widen x1 x2 in
    let y = B.widen y1 y2 in
    R.reduce (x, y)


  (* comparison operations (filters) *)
(* 
  let eq = assert false

  let geq = assert false

  let gt = assert false *)


  (* subset inclusion of concretizations *)
  let subset (x1,y1) (x2,y2) = A.subset x1 x2 && B.subset y1 y2

  (* check the emptiness of the concretization *)
  let is_bottom (x,y) = A.is_bottom x && B.is_bottom y

  (* print abstract element *)
  let print fmt (x1,x2) = 
    (A.print fmt x1);
    Format.fprintf fmt " ∧ ";
    (B.print fmt x2);

end : VALUE_DOMAIN)