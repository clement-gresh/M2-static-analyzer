open Value_reduction
open Parity_domain
open Interval_domain

module ParityIntervalsReduction = (struct
  module A = Parity
  module B = Intervals

  type t = A.t * B.t

  let reduce = assert false

  (*let rest a = Z.rem a (Z.add Z.one Z.one)
  
  let reduce ((a,b):t) : t = assert false match (a,b) with
  | A.bottom, _ | _, B.bottom -> A.bottom, B.bottom
  
  (* interval of type [c, c] *)
  | A.top, B.INTERVAL(Cst c, Cst c) when rest c = Z.zero -> (A.EVEN, B.INTERVAL(Cst c, Cst c))
  | A.top, B.INTERVAL(Cst c, Cst c) when rest c = Z.one -> (A.ODD, B.INTERVAL(Cst c, Cst c))
  | A.ODD, B.INTERVAL(Cst c, Cst c) when rest c = Z.zero -> (BOT, BOT)
  | A.ODD, B.INTERVAL(Cst c, Cst c) when rest c = Z.one -> (A.ODD, B.INTERVAL(Cst c, Cst c))
  | A.EVEN, B.INTERVAL(Cst c, Cst c) when rest c = Z.zero -> (A.EVEN, B.INTERVAL(Cst c, Cst c))
  | A.EVEN, B.INTERVAL(Cst c, Cst c) when rest c = Z.one -> (BOT, BOT)

  (* interval of type [even, even] or [odd, odd] *)
  | A.ODD, B.INTERVAL(Cst c, Cst d) when rest c = Z.zero && rest d = Z.zero
    -> (A.ODD, B.INTERVAL(Z.add (Cst c) Z.one, Z.sub (Cst d) Z.one))
  | A.EVEN, B.INTERVAL(Cst c, Cst d) when rest c = Z.one && rest d = Z.one
    -> (A.EVEN, B.INTERVAL(Z.add (Cst c) Z.one, Z.sub (Cst d) Z.one))
  
  (* interval of type [even, odd] or [odd, even] *)
  | A.ODD, B.INTERVAL(Cst c, Cst d) when rest c = Z.zero && rest d = Z.one
    -> (A.ODD, B.INTERVAL(Z.add (Cst c) Z.one, Cst d))
  | A.ODD, B.INTERVAL(Cst c, Cst d) when rest c = Z.one && rest d = Z.zero
    -> (A.ODD, B.INTERVAL(Cst c, Z.sub (Cst d) Z.one))
  | A.EVEN, B.INTERVAL(Cst c, Cst d) when rest c = Z.zero && rest d = Z.one
    -> (A.EVEN, B.INTERVAL(Cst c, Z.sub (Cst d) Z.one))
  | A.EVEN, B.INTERVAL(Cst c, Cst d) when rest c = Z.one && rest d = Z.zero
    -> (A.EVEN, B.INTERVAL(Z.add (Cst c) Z.one, Cst d))

  | x, y -> (x, y) *)

end : VALUE_REDUCTION)