(* Needed for trying to assign a complex formula a value. *)
exception NotATermException of string

(* Stores the arithmetic expression of a formula. *)
type 'a expr =
  Val of 'a ref
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
and 'a equation_expr =
| Comp of 'a expr * 'a expr * ('a -> 'a -> bool)

(* Similar to 'a expr and equation_expr but for systems of equations. *)
and 'a system_expr =
| Single of 'a equation_expr
| And of 'a system_expr * 'a system_expr
| Or of 'a system_expr * 'a system_expr
(* A wrapper around system_expr to allow for updating. *)
and 'a system =
{
  mutable parents: 'a system list;
  mutable when_satisfied: (unit -> unit) list;
  mutable on_change: (bool -> bool -> unit) list;
  mutable value: bool;
  expression: 'a system_expr;
  mutable needs_update: bool;
}

type 'a source =
{
  mutable sys_func : ('a system * (unit -> unit)) list;
}

(* Evaluate what an float expr currently should be. *)
let rec eval_expr (e: 'a expr): 'a =
  match e with
    | BinOp (a, b, op) -> op (eval_expr a) (eval_expr b)
    | Val x      -> !x


let rec eval_expr_equation (e: 'a equation_expr): bool =
  match e with
    | Comp (a, b, op) -> op (eval_expr a) (eval_expr b)

let rec eval_system_expr (s: 'a system_expr): bool = match s with
| Single a -> eval_expr_equation a
| And (a, b) -> (eval_system_expr a) && (eval_system_expr b)
| Or (a, b) -> (eval_system_expr a) || (eval_system_expr b)


(* Evaluate a type *but* avoid unneeded updates with caching *)
let eval (f: 'a formula): 'a = 
  if f.needs_update then eval_expr f.expression else f.value
let eval_system (eq: 'a system): bool = 
  if eq.needs_update then eval_system_expr eq.expression else eq.value

let rec propegate (f: 'a formula) (old_val: 'a): unit =
  List.iter (fun g -> g old_val f.value) f.on_change;
  List.iter (fun p -> p.needs_update <- true) f.parents;
  List.iter (fun (p: 'a system) -> p.needs_update <- true) f.pred_parents;
  List.iter update_a_formula f.parents;
  List.iter update_system f.pred_parents;
  f.needs_update <- false
and propegate_system (eq: 'a system) (old_value: bool): unit =
  List.iter (fun g -> g old_value eq.value) eq.on_change;
  if eq.value = true then 
    (List.iter (fun g -> g ()) eq.when_satisfied;
    List.iter (fun (p: 'a system) -> p.needs_update <- true) eq.parents) else ()
and update_a_term (f: 'a formula) (new_val: 'a) =
  let old_val = f.value in
  match f.expression with
  | Val t ->
      if !t <> new_val then
     (t := new_val;
      f.value <- new_val;
      propegate f old_val) else ()
  | _ -> raise (NotATermException "Formula is not a term and cannot be reassigned.")
and update_a_formula (f: 'a formula) =
  let new_val = eval f in
  let old_val = f.value in
  if f.value <> new_val then
    (f.value <- new_val;
     propegate f old_val)
and update_system (s: 'a system): unit =
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
let formula_create (e: 'a expr) (value: 'a) =
{ 
  parents = []; 
  value = value;
  on_change = [];
  needs_update = false; 
  expression = e;
  pred_parents = [];
}


(* Construct a formula of a single term. *)
let t (value: 'a): 'a formula = formula_create (Val (ref value)) value

(* Shorthand for update methods. *)
let (=:) = update_a_term

(* Extract values. Basically the same as (!) for reference types. *)
let (!) (f: 'a formula) = f.value
let (!!) (s: 'a system) = s.value

(* Arithmetic functions. *)

(* Create a binary operation that merges two formula into a more complex one. *)
let bin_form_a (op: 'a -> 'a -> 'a) (mk_expr: 'a expr -> 'a expr -> 'a expr) (f1: 'a formula) (f2: 'a formula): 'a formula =
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

let system_create (e: 'a system_expr) (value: bool): 'a system =
{ 
  parents = [];
  value = value;
  on_change = [];
  needs_update = false; 
  expression = e; 
  when_satisfied = [];
}

let equation_create (e: 'a equation_expr) (value: bool): 'a system = system_create (Single e) value

(* Forming new equations from formula and comparison operators. *)
let comp_form_a (comp: 'a -> 'a -> bool) 
                (mk_cmp : 'a formula -> 'a formula -> 'a equation_expr)
                (f1: 'a formula) (f2: 'a formula): 'a system =
  let eq = equation_create (mk_cmp f1 f2) (comp f1.value f2.value) in
  f1.pred_parents <- eq :: f1.pred_parents;
  f2.pred_parents <- eq :: f2.pred_parents;
  eq

(* Equality of two int formulas. *)
let eq_form_int = comp_form_a (=) (fun a b -> Comp (a.expression, b.expression, (=)))
let (=?) = eq_form_int

(* Not equals of two int formulas. *)
let ne_form_int = comp_form_a (<>) (fun a b -> Comp (a.expression, b.expression, (<>)))
let (<>?) = ne_form_int

(* Equality of two float formulas. *)
let eq_form_float = comp_form_a (=) (fun a b -> Comp (a.expression, b.expression, (=)))
let (=.) = eq_form_float

(* Not equals of two float formulas. *)
let ne_form_float = comp_form_a (<>) (fun a b -> Comp (a.expression, b.expression, (<>)))
let (<>.) = ne_form_float

(* Greater than of two int formulas. *)
let gt_form_int = comp_form_a (>) (fun a b -> Comp (a.expression, b.expression, (>)))
let (>?) = gt_form_int

(* Greater than or equals of two int formulas. *)
let gte_form_int = comp_form_a (>=) (fun a b -> Comp (a.expression, b.expression, (>=)))
let (>=?) = gte_form_int

(* Greater than of two float formulas. *)
let gt_form_float = comp_form_a (>) (fun a b -> Comp (a.expression, b.expression, (>)))
let (>.) = gt_form_float

(* Greater than or equals of two float formulas. *)
let gte_form_float = comp_form_a (>=) (fun a b -> Comp (a.expression, b.expression, (>=)))
let (>=.) = gte_form_float

(* Less than of two int formulas. *)
let lt_form_int = comp_form_a (>) (fun a b -> Comp (a.expression, b.expression, (<)))
let (<?) = lt_form_int

(* Less than or equals of two int formulas. *)
let lte_form_int = comp_form_a (>=) (fun a b -> Comp (a.expression, b.expression, (<=)))
let (<=?) = lte_form_int

(* Less than of two float formulas. *)
let lt_form_float = comp_form_a (>) (fun a b -> Comp (a.expression, b.expression, (<)))
let (<.) = lt_form_float

(* Less than or equals of two float formulas. *)
let lte_form_float = comp_form_a (>=) (fun a b -> Comp (a.expression, b.expression, (<=)))
let (<=.) = lte_form_float

(* System creation *)

let sys_make (op: bool -> bool -> bool)
             (mk_sys: 'a system -> 'a system -> 'a system_expr)
             (s1: 'a system) (s2: 'a system): 'a system =
  let sys = system_create (mk_sys s1 s2) (op s1.value s2.value) in
  s1.parents <- sys :: s1.parents;
  s2.parents <- sys :: s2.parents;
  sys

let and_eqs (s1: 'a system) (s2: 'a system) = sys_make (&&) (fun a b -> And (a.expression, b.expression)) 
  s1 s2
let (&&) = and_eqs

let or_eqs (s1: 'a system) (s2: 'a system) = sys_make (||) (fun a b -> Or (a.expression, b.expression)) 
  s1 s2
let (||) = or_eqs

(* For everything else that needs to be user defined. *)
let formula_reg_bin (op: 'a -> 'a -> 'a): ('a formula -> 'a formula -> 'a formula) = 
  bin_form_a op (fun a b -> BinOp (a, b, op))

(* Source functions. *)

let make_source (): 'a source = { sys_func = []; }

let listen (s: 'a source): unit =
  List.iter (fun pair -> if eval_system (fst pair) then (snd pair) () else ()) s.sys_func

(* Listeners *)
let on_change (f: 'a formula) (g: 'a -> 'a -> unit) = f.on_change <- g :: f.on_change
let system_change (f: 'a system) (g: bool -> bool -> unit) = f.on_change <- g :: f.on_change
let when_satisfied (f: 'a system) (g: unit -> unit) = f.when_satisfied <- g :: f.when_satisfied
let exec_while (src: 'a source) (s: 'a system) (g: unit -> unit) = src.sys_func <- (s, g) :: src.sys_func
