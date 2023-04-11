open Value_reduction
open Parity_domain
open Interval_domain

module ParityIntervalsReduction = (struct
  module A = Parity
  module B = Intervals

  type t = A.t * B.t

  (* let reduce = assert false *)

  let rest a = Z.rem a (Z.add Z.one Z.one)
  
  let reduce ((a,b):t) : t = match (a,b) with
  | A.BOT, _ | _, B.BOT -> A.bottom, B.bottom
  
  (* interval of type [c, c] *)
  | A.TOP, B.INTERVAL(Cst c, Cst c') when c=c' && rest c = Z.zero -> (A.EVEN, B.INTERVAL(Cst c, Cst c))
  | A.TOP, B.INTERVAL(Cst c, Cst c') when c=c' && rest c = Z.one -> (A.ODD, B.INTERVAL(Cst c, Cst c))
  | A.ODD, B.INTERVAL(Cst c, Cst c') when c=c' && rest c = Z.zero -> (BOT, BOT)
  | A.ODD, B.INTERVAL(Cst c, Cst c') when c=c' && rest c = Z.one -> (A.ODD, B.INTERVAL(Cst c, Cst c))
  | A.EVEN, B.INTERVAL(Cst c, Cst c') when c=c' && rest c = Z.zero -> (A.EVEN, B.INTERVAL(Cst c, Cst c))
  | A.EVEN, B.INTERVAL(Cst c, Cst c') when c=c' && rest c = Z.one -> (BOT, BOT)

  (* interval of type [even, even] or [odd, odd] *)
  | A.ODD, B.INTERVAL(Cst c, Cst d) when rest c = Z.zero && rest d = Z.zero
    -> (A.ODD, B.INTERVAL(Cst (Z.add c Z.one),Cst (Z.sub d Z.one)))
  | A.EVEN, B.INTERVAL(Cst c, Cst d) when rest c = Z.one && rest d = Z.one
    -> (A.EVEN, B.INTERVAL(Cst (Z.add c Z.one), Cst (Z.sub d Z.one)))
  
  (* interval of type [even, odd] or [odd, even] *)
  | A.ODD, B.INTERVAL(Cst c, d) when rest c = Z.zero -> (A.ODD, B.INTERVAL(Cst (Z.add c Z.one), d))
  | A.ODD, B.INTERVAL(c, Cst d) when rest d = Z.zero -> (A.ODD, B.INTERVAL(c, Cst (Z.sub d Z.one)))
  | A.EVEN, B.INTERVAL(c, Cst d) when rest d = Z.one -> (A.EVEN, B.INTERVAL(c, Cst (Z.sub d Z.one)))
  | A.EVEN, B.INTERVAL(Cst c, d) when rest c = Z.one -> (A.EVEN, B.INTERVAL(Cst (Z.add c Z.one), d))

  | x, y -> (x, y)

end : VALUE_REDUCTION)