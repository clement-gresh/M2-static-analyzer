open Abstract_syntax_tree
open Domain

module Disjunctions(D:DOMAIN) = (struct         (* le domaine D est non-relationnal*)

  type t =
  | SIMPLE of D.t
  | DISJ of t * t

  let init (_:unit) = SIMPLE(D.init ())

    (* empty set *)
    let bottom (_:unit) = SIMPLE(D.bottom ()) (* debug : a verifier *)

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
    let join (a:t) (b:t) : t = DISJ(a, b) (* join arbre1 arbre 2 = DISJ(arbre1, arbre2) *)

    (* whether the abstract element represents the empty set *)
    let rec is_bottom (a:t) : bool = match a with
    | SIMPLE x -> D.is_bottom x
    | DISJ(x, y) -> is_bottom x && is_bottom y

    (* abstract intersection *)
    let rec meet (a:t) (b:t) : t = match a, b with
      | SIMPLE x, SIMPLE y -> SIMPLE(D.meet x y)
      | SIMPLE x, DISJ(y1, y2) ->                       (* si on on a bottom, on rejette le résultat *)
        if is_bottom (meet (SIMPLE x) y1) then
          if is_bottom (meet (SIMPLE x) y2) then
            bottom ()
          else 
            meet (SIMPLE x) y2
        else
          if is_bottom (meet (SIMPLE x) y2) then
            meet (SIMPLE x) y1
          else DISJ(meet (SIMPLE x) y1, meet (SIMPLE x) y2)
      | DISJ(_, _), SIMPLE _ -> meet b a    (* debug : est-ce que (meet a b) est bien équivalent à (meet b a) ? Si ce n'est pas le cas, il faut faire comme dans le cas précédent (i.e. 4 cas différents) *)
      | DISJ(x1, x2), DISJ(_, _) ->                     (* si on on a bottom, on rejette le résultat *)
        if is_bottom (meet x1 b) then
          if is_bottom (meet x2 b) then
            bottom ()
          else meet x2 b
        else
          if is_bottom (meet x2 b) then
            meet x1 b
          else DISJ(meet x1 b, meet x2 b)
        (* DISJ(DISJ(meet x1 y1, meet x1 y2), DISJ(meet x2 y1, meet x2 y2)) *)
    (* on fait le meet de chacun des feuilles de gacuhe avec chacune des feuilles de droite.
       Si on obtient bottom, on rejette le résultat. Sinon on l'ajoute.
       On se retrouve dans le pire cas avec un nombre quadratique de feuilles *)

    (* widening *)
    let widen = assert false (* cours 11, slide 27 *)
    (* A priori, il faut :
       - jusqu'à n, faire l'union entre An et ce qu'on obtient à la sortie du tour de boucle suivant (avec An en entrée et qu'on appelle Bn+1)
       - après n, faire l'élargissement entre l'union de An et l'union de Bn+1
       (je ne comprend pas ce que veut dire union de An)*)

    (* whether an abstract element is included in another one *)
    let rec subset (a:t) (b:t) : bool = match a, b with  (* cours 11, slide 25 : comment faire le subset ainsi que la description des limites du subset *)
    | SIMPLE x, SIMPLE y -> D.subset x y
    | SIMPLE x, DISJ(y1, y2) -> subset (SIMPLE x) y1 || subset (SIMPLE x) y2
    | DISJ(x1, x2), SIMPLE y -> subset x1 (SIMPLE y) && subset x2 (SIMPLE y)
    | DISJ(x1, x2), DISJ(y1, y2) -> (subset x1 y1 || subset x1 y2) && (subset x2 y1 || subset x2 y2)

    (* prints *)
    let rec print (fmt:Format.formatter) (a:t) (l:var list) : unit = match a with (* if a var is in varlist, then we print it*)
      | SIMPLE x ->
        (Format.fprintf fmt " [ ";
        D.print fmt x l;
        Format.fprintf fmt " ] ";)
      | DISJ(x, y) ->
        (print fmt x l;
        Format.fprintf fmt " - ";
        print fmt y l; )

    let rec print_all (fmt:Format.formatter) (a:t) : unit = match a with
    | SIMPLE x ->
      (Format.fprintf fmt " [ ";
      D.print_all fmt x;
      Format.fprintf fmt " ] ";)
    | DISJ(x, y) ->
      ( print_all fmt x;
      Format.fprintf fmt " - ";
      print_all fmt y; )

  
end : DOMAIN)