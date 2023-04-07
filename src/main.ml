(*
  Cours "Typage et Analyse Statique" - Master STL
  Sorbonne Université
  Antoine Miné 2015-2022
*)


module ConcreteAnalysis =
  Interpreter.Interprete(Concrete_domain.Concrete)

module ConstantAnalysis =
  Interpreter.Interprete
    (Non_relational_domain.NonRelational
       (Constant_domain.Constants))

module IntervalAnalysis =
    Interpreter.Interprete
      (Non_relational_domain.NonRelational
        (Interval_domain.Intervals))                (* nom_fichier.nom_module *)

(* module ParityAnalysis =              (* debug : it feels like we dont need this module, only the next 2 *)
  Interpreter.Interprete
    (Non_relational_domain.NonRelational
      (Parity_domain.Parity))

module ParityIntervalReduction =
  Interpreter.Interprete
    (Non_relational_domain.NonRelational
      (Value_reduced_product.ReducedProduct(Parity_interval_reduction.ParityIntervalsReduction))) *)

module DisjunctionAnalysis =
  Interpreter.Interprete
    (Disjunction_domain.Disjunctions 
      (Non_relational_domain.NonRelational
          (Parity_domain.Parity)))

(* parse and print filename *)
let doit filename =
  let prog = File_parser.parse_file filename in
  Abstract_syntax_printer.print_prog Format.std_formatter prog


(* default action: print back the source *)
let eval_prog prog =
  Abstract_syntax_printer.print_prog Format.std_formatter prog

(* entry point *)
let main () =
  let action = ref eval_prog in
  let files = ref [] in
  (* parse arguments *)
  Arg.parse
    (* handle options *)
    ["-trace",
     Arg.Set Interpreter.trace,
     "Show the analyzer state after each statement";

     "-nonreldebug",
     Arg.Set Non_relational_domain.non_relational_debug,
     "Turns on debugging information for the non relational lifter";

     "-concrete",
     Arg.Unit (fun () -> action := ConcreteAnalysis.eval_prog),
     "Use the concrete domain";

     "-constant",
     Arg.Unit (fun () -> action := ConstantAnalysis.eval_prog),
     "Use the constant abstract domain";

     "-interval",
     Arg.Unit (fun () -> action := IntervalAnalysis.eval_prog),
     "Use the interval abstract domain";
    
     "-delay",
     Arg.Int (Interpreter.set_delay_nb),
     "Replace the first applications of widening by union";

     "-unroll",
     Arg.Int (Interpreter.set_unroll_nb),
     "Allows to unroll the first loop turns before the application of widening";
     
    (* debug: added an argument to ReducedProduct, dont know if that's right *)
     (* "-parity-interval",
     Arg.Unit (fun () -> action := ParityIntervalReduction.eval_prog),(*ReducedProduct(ParityIntervalReduction).eval_prog),*)
     "operate a reduction of the parity and interval domains"; *)

    ]
    (* handle filenames *)
    (fun filename -> files := (!files)@[filename])
    "";
  List.iter
    (fun filename ->
      let prog = File_parser.parse_file filename in
      !action prog
    )
    !files

let _ = main ()
