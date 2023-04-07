open Abstract_syntax_tree
open Domain

module Disjunctions(D:DOMAIN) = (struct         (* domain est non-relationnal*)


  type t =
  | SIMPLE of D.t
  | DISJ of t * t

  let init (_:unit) = SIMPLE(D.init ())

    (* empty set *)
    let bottom = assert false

    (* add a variable *)
    let rec add_var (a:t) (b:var) : t = match a with
    | SIMPLE x -> SIMPLE(D.add_var x b)
    | DISJ(x, y) -> DISJ(add_var x b, add_var y b)

    (* remove a variable *)
    let rec del_var (a:t) (b:var) : t = match a with
    | SIMPLE x -> SIMPLE(D.del_var x b)
    | DISJ(x, y) -> DISJ(del_var x b, del_var y b)

    (* assign an integer expression to a variable *)
    let rec assign (a:t) (v:var) (expr:int_expr) : t = match a with
      | SIMPLE x -> SIMPLE(D.assign x v expr)
      | DISJ(x, y) -> DISJ(assign x v expr, assign y v expr)

    (* filter environments to keep only those satisfying the comparison *)
    let compare = assert false (* rec calls to compare ? Checks what has been done in other domains, like intervals *)

    (* abstract join *)
    let join = assert false (* join arbre1 arbre 2 = DISJ(arbre1, arbre2) *)

    (* abstract intersection *)
    let meet = assert false
    (* on fait le meet de chacun des feuilles de gacuhe avec chacune des feuilles de droite
       si on obtient bottom, on rejette le résultat
       sinon on l'ajoute.
       On se retrouve dans le pire cas avec un nombre quadratique de feuilles *)

    (* widening *)
    let widen = assert false

    (* whether an abstract element is included in another one *)
    let subset = assert false  (* cours 11, slide 25 : comment faire le subset ainsi que ses limites *)

    (* whether the abstract element represents the empty set *)
    let is_bottom = assert false

    (* prints *)
    let print = assert false
    let print_all = assert false

  
end : DOMAIN)