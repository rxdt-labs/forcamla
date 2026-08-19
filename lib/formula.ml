(* Needed for trying to assign a complex formula a value. *)
exception NotATermException of string

(* Stores the arithmetic expression of a formula. *)
type 'a expr =
  Val of 'a ref
| UnaryOp of 'a expr * ('a -> 'a)
| BinOp of 'a expr * 'a expr * ('a -> 'a -> 'a)

(* Basically 'a expr, but with some additional fields for updating. *)
and 'a formula =
{
  mutable parents: 'a formula list;
  mutable pred_parents: 'a system list;
  mutable on_change: ('a -> 'a -> unit) list;
  mutable value: 'a;
  expression: 'a expr;
  mutable needs_update: bool;
}
(* Similar to 'a expr but for equations instead of formula. *)
and 'b equation_expr =
| LogicOp of 'b expr * ('b -> bool)
| Comparison of 'b expr * 'b expr * ('b -> 'b -> bool)

(* Similar to 'a expr and equation_expr but for systems of equations. *)
and 'c system_expr =
| Single of 'c equation_expr
| And of  'c system_expr * 'c system_expr
| Or of 'c system_expr * 'c system_expr
(* A wrapper around system_expr to allow for updating. *)
and 'e system =
{
  mutable parents: 'e system list;
  mutable when_satisfied: (unit -> unit) list;
  mutable on_change: (bool -> bool -> unit) list;
  mutable value: bool;
  expression: 'e system_expr;
  mutable needs_update: bool;
}

type 'f source =
{
  mutable exec_while : ('f system * (unit -> unit)) list;
  mutable exec_always : ('f system * (bool -> unit)) list;
}

(* Evaluate what an float expr currently should be. *)
let rec eval_expr (e: 'g expr): 'a =
  match e with
    | UnaryOp (a, op) -> op (eval_expr a)
    | BinOp (a, b, op) -> op (eval_expr a) (eval_expr b)
    | Val x      -> !x


let rec eval_expr_equation (e: 'h equation_expr): bool =
  match e with
    | LogicOp (a, op) -> op (eval_expr a)
    | Comparison (a, b, op) -> op (eval_expr a) (eval_expr b)

let rec eval_system_expr (s: 'i system_expr): bool = match s with
| Single a -> eval_expr_equation a
| And (a, b) -> (eval_system_expr a) && (eval_system_expr b)
| Or (a, b) -> (eval_system_expr a) || (eval_system_expr b)


(* Evaluate a type *but* avoid unneeded updates with caching *)
let eval (f: 'j formula): 'j = 
  if f.needs_update then eval_expr f.expression else f.value
let eval_system (eq: 'k system): bool = 
  if eq.needs_update then eval_system_expr eq.expression else eq.value

let rec propegate (f: 'l formula) (old_val: 'l): unit =
  List.iter (fun g -> g old_val f.value) f.on_change;
  List.iter (fun p -> p.needs_update <- true) f.parents;
  List.iter (fun (p: 'l system) -> p.needs_update <- true) f.pred_parents;
  List.iter update_a_formula f.parents;
  List.iter update_system f.pred_parents;
  f.needs_update <- false
and propegate_system (eq: 'l system) (old_value: bool): unit =
  List.iter (fun g -> g old_value eq.value) eq.on_change;
  if eq.value = true then 
    (List.iter (fun g -> g ()) eq.when_satisfied;
    List.iter (fun (p: 'l system) -> p.needs_update <- true) eq.parents) else ()
and update_a_term (f: 'l formula) (new_val: 'a) =
  let old_val = f.value in
  match f.expression with
  | Val t ->
      if !t <> new_val then
     (t := new_val;
      f.value <- new_val;
      propegate f old_val) else ()
  | _ -> raise (NotATermException "Formula is not a term and cannot be reassigned.")
and update_a_formula (f: 'l formula) =
  let new_val = eval f in
  let old_val = f.value in
  if f.value <> new_val then
    (f.value <- new_val;
     propegate f old_val)
and update_system (s: 'l system): unit =
  match s.expression with
  | Single a -> let new_val = eval_expr_equation a in
                let old_val = s.value in
    if new_val <> s.value then
      (s.value <- new_val; propegate_system s old_val)
  | And (a, b) -> let new_val = (eval_system_expr a) && (eval_system_expr b) in
                  let old_val = s.value in
    if new_val <> s.value then
      (s.value <- new_val; propegate_system s old_val)
  | Or (a, b) -> let new_val = (eval_system_expr a) || (eval_system_expr b) in
                 let old_val = s.value in
    if new_val <> s.value then
      (s.value <- new_val; propegate_system s old_val)

(* Create a formula helper. *)
let formula_create (e: 'm expr) (value: 'm) =
{ 
  parents = []; 
  value = value;
  on_change = [];
  needs_update = false; 
  expression = e;
  pred_parents = [];
}


(* Construct a formula of a single term. *)
let t (value: 'n): 'n formula = formula_create (Val (ref value)) value

(* Shorthand for update methods. *)
let (=:) = update_a_term

(* Extract values. Basically the same as (!) for reference types. *)
let (!) (f: 'o formula) = f.value
let (!!) (s: 'o system) = s.value

(* Arithmetic functions. *)

(* Create a binary operation that merges two formula into a more complex one. *)
let bin_form_a (op: 'p -> 'p -> 'p) (mk_expr: 'p expr -> 'p expr -> 'p expr) (f1: 'a formula) (f2: 'a formula): 'a formula =
  let f = formula_create (mk_expr f1.expression f2.expression) (op f1.value f2.value) in
  f1.parents <- f :: f1.parents;
  f2.parents <- f :: f2.parents;
  f

(* Addition of int typed formula. *)
let add_form_int = bin_form_a (+) (fun a b -> BinOp (a, b, (+)))
let (+) = add_form_int

(* Subtraction of new types. *)
let sub_form_int = bin_form_a (-) (fun a b -> BinOp (a, b, (-)))
let (-) = sub_form_int

(* Multiplication of new types. *)
let mul_form_int = bin_form_a ( * ) (fun a b -> BinOp (a, b, ( * )))
let ( * ) = mul_form_int

(* Division of new types. *)
let div_form_int = bin_form_a (/) (fun a b -> BinOp (a, b, (/)))
let (/) = div_form_int

(* Addition of float typed formula. *)
let add_form_float = bin_form_a (+.) (fun a b -> BinOp (a, b, (+.)))
let (+.) = add_form_float

(* Subtraction of new types. *)
let sub_form_float = bin_form_a (-.) (fun a b -> BinOp (a, b, (-.)))
let (-.) = sub_form_float

(* Multiplication of new types. *)
let mul_form_float = bin_form_a ( *. ) (fun a b -> BinOp (a, b, ( *. )))
let ( *. ) = mul_form_float

(* Division of new types. *)
let div_form_float = bin_form_a (/.) (fun a b -> BinOp (a, b, (/.)))
let (/.) = div_form_float

(* Comparison operators. (Equation creation) *)

let system_create (e: 'q system_expr) (value: bool): 'q system =
{ 
  parents = [];
  value = value;
  on_change = [];
  needs_update = false; 
  expression = e; 
  when_satisfied = [];
}

let equation_create (e: 'r equation_expr) (value: bool): 'r system = system_create (Single e) value

(* Forming new equations from formula and comparison operators. *)
let comp_form_a (comp: 's -> 's -> bool) 
                (mk_cmp : 's formula -> 's formula -> 's equation_expr)
                (f1: 's formula) (f2: 's formula): 's system =
  let eq = equation_create (mk_cmp f1 f2) (comp f1.value f2.value) in
  f1.pred_parents <- eq :: f1.pred_parents;
  f2.pred_parents <- eq :: f2.pred_parents;
  eq

(* Equality of two int formulas. *)
let eq_form_int = comp_form_a (=) (fun a b -> Comparison (a.expression, b.expression, (=)))
let (=?) = eq_form_int

(* Not equals of two int formulas. *)
let ne_form_int = comp_form_a (<>) (fun a b -> Comparison (a.expression, b.expression, (<>)))
let (<>?) = ne_form_int

(* Equality of two float formulas. *)
let eq_form_float = comp_form_a (=) (fun a b -> Comparison (a.expression, b.expression, (=)))
let (=.) = eq_form_float

(* Not equals of two float formulas. *)
let ne_form_float = comp_form_a (<>) (fun a b -> Comparison (a.expression, b.expression, (<>)))
let (<>.) = ne_form_float

(* Greater than of two int formulas. *)
let gt_form_int = comp_form_a (>) (fun a b -> Comparison (a.expression, b.expression, (>)))
let (>?) = gt_form_int

(* Greater than or equals of two int formulas. *)
let gte_form_int = comp_form_a (>=) (fun a b -> Comparison (a.expression, b.expression, (>=)))
let (>=?) = gte_form_int

(* Greater than of two float formulas. *)
let gt_form_float = comp_form_a (>) (fun a b -> Comparison (a.expression, b.expression, (>)))
let (>.) = gt_form_float

(* Greater than or equals of two float formulas. *)
let gte_form_float = comp_form_a (>=) (fun a b -> Comparison (a.expression, b.expression, (>=)))
let (>=.) = gte_form_float

(* Less than of two int formulas. *)
let lt_form_int = comp_form_a (>) (fun a b -> Comparison (a.expression, b.expression, (<)))
let (<?) = lt_form_int

(* Less than or equals of two int formulas. *)
let lte_form_int = comp_form_a (>=) (fun a b -> Comparison (a.expression, b.expression, (<=)))
let (<=?) = lte_form_int

(* Less than of two float formulas. *)
let lt_form_float = comp_form_a (>) (fun a b -> Comparison (a.expression, b.expression, (<)))
let (<.) = lt_form_float

(* Less than or equals of two float formulas. *)
let lte_form_float = comp_form_a (>=) (fun a b -> Comparison (a.expression, b.expression, (<=)))
let (<=.) = lte_form_float

(* System creation *)

let sys_make (op: bool -> bool -> bool)
             (mk_sys: 't system -> 't system -> 't system_expr)
             (s1: 't system) (s2: 't system): 't system =
  let sys = system_create (mk_sys s1 s2) (op s1.value s2.value) in
  s1.parents <- sys :: s1.parents;
  s2.parents <- sys :: s2.parents;
  sys

let and_eqs (s1: 'u system) (s2: 'u system) = sys_make (&&) (fun a b -> And (a.expression, b.expression)) 
  s1 s2
let (&&) = and_eqs

let or_eqs (s1: 'v system) (s2: 'v system) = sys_make (||) (fun a b -> Or (a.expression, b.expression)) 
  s1 s2
let (||) = or_eqs

(* For everything else that needs to be user defined. *)
let formula_reg_bin (op: 'w -> 'w -> 'w): ('w formula -> 'w formula -> 'w formula) = 
  bin_form_a op (fun a b -> BinOp (a, b, op))

(* Source functions. *)

let make_source (): 'x source = 
{
  exec_while = []; 
  exec_always = [];
}

let listen (s: 'y source): unit =
  (* Execute the exec_while functions if the condition is true. *)
  List.iter (fun pair -> if eval_system (fst pair) then (snd pair) () else ()) s.exec_while;
  (* Next execute the exec_always function regardless and supply the value of the system. *)
  List.iter (fun pair -> (snd pair) (eval_system (fst pair))) s.exec_always

(* Listeners *)
let on_change (f: 'z formula) (g: 'z -> 'z -> unit) = f.on_change <- g :: f.on_change
let system_change (f: 'aa system) (g: bool -> bool -> unit) = f.on_change <- g :: f.on_change
let when_satisfied (f: 'ab system) (g: unit -> unit) = f.when_satisfied <- g :: f.when_satisfied
let exec_always (src: 'ac source) (s: 'ac system) (g: bool -> unit) = src.exec_always <- (s, g) :: src.exec_always
let exec_while (src: 'ad source) (s: 'ad system) (g: unit -> unit) = src.exec_while <- (s, g) :: src.exec_while

