(** Create mathematical formula, equations, and systems of equations as well as define event listeners that check for various changes in these mathematical objects. *)

(** {1 The Fundamental Types} *)

(** A combination of terms (similar to variables) and operations that represent a mathematical formula *)
type 'a formula

(** A collecction of equations (possibly a collection of one equation). {b Note:} For type safety reasons, [system] type is distinct from the [formula] type. *)
type 'b system

(** {1 Term Creation} *)

(** Terms are of type [formula]. In particular, a {i term} refers to a [formula] with no binary operations.
    There is no term type, but the distinction is made because {i only} terms can be assigned values 
    (otherwise a [NotATermException] error is thrown!).
*)

(** Lift basic types to term types. *)
val t : 'c -> 'c formula

(** {1 Update Term Methods} *)

(** Update a term to a new value. (If the supplied formula is {i not} a term, an error is thrown.) *)
val (=:) : 'd formula -> 'd -> unit

(** {1 Extraction: Get the (current) value for a [formula] or [system]} *)

(** Get current value of a formula. Similar to [(!)] for ref types. *)
val (!) : 'e formula -> 'e

(** Get current value of a system. Similar to [(!)] for ref types. *)
val (!!) : 'f system -> bool

(** {1 Formula creation methods} *)

(** {2 Arithmetic for [int formula]} *)

(** Shorthand versions are presented in the next section! *)

(** Create a formula that is the sum of two int formula. *)
val add_form_int : int formula -> int formula -> int formula

(** Create a formula that is the difference of two int formula. *)
val sub_form_int : int formula -> int formula -> int formula

(** Create a formula that is the product of two int formula. *)
val mul_form_int : int formula -> int formula -> int formula

(** Create a formula that is the quotient of two int formula *)
val div_form_int : int formula -> int formula -> int formula

(** {2 Shorthand arithmetic for [int formula]} *)

(** Shorthand for addition in an int formula. *)
val (+) : int formula -> int formula -> int formula

(** Shorthand for subtraction in an int formula. *)
val (-) : int formula -> int formula -> int formula

(** Shorthand for multiplication in an int formula. *)
val ( * ) : int formula -> int formula -> int formula

(** Shorthand for division in an int formula. *)
val (/) : int formula -> int formula -> int formula

(** {2 Arithmetic for [float formula]} *)

(** Shorthand versions are presented in the next section! *)

(** Create a formula that is the sum of two float formula. *)
val add_form_float : float formula -> float formula -> float formula

(** Create a formula that is the differece of two float formula. *)
val sub_form_float : float formula -> float formula -> float formula

(** Create a formula that is the product of two float formula. *)
val mul_form_float : float formula -> float formula -> float formula

(** Create a formula that is the quotient of two float formula. *)
val div_form_float : float formula -> float formula -> float formula

(** {2 Shorthand [float formula] arithmetic} *)

(** Shorthand for addition in a float formula. *)
val (+.) : float formula -> float formula -> float formula

(** Shorthand for subtraction in a float formula. *)
val (-.) : float formula -> float formula -> float formula

(** Shorthand for multiplication in a float formula. *)
val ( *. ) : float formula -> float formula -> float formula

(** Shorthand for division in a float formula. *)
val (/.) : float formula -> float formula -> float formula

(** {2 Concat [string formula]} *)

(** Create a formula that is the concatenation of two string formula. *)
val concat_strings : string formula -> string formula -> string formula

(** Shorthand for concatenation of two formula. *)
val (^) : string formula -> string formula -> string formula

(** {2 [bool formula] operations} (Shorthand below!) *)

(** Logical and of two [bool formula] *)
val and_ : bool formula -> bool formula -> bool formula

(** Logical or of two [bool formula] *)
val or_ : bool formula -> bool formula -> bool formula

(** {1 [system] Constructors} *)

(** {2 Fundamental [system] Constructors} *)

(** Fundamental [system] types have only one equation.
    Given two formula [f1] and [f2], fundamental system are of the form [f1 comp f2] where [comp] is a 
    method of {i comparing} the two formula.
    Shorthand versions are mentioned below.
*)

(** Create an equation that determines if two int formula are equal. *)
val eq_form_int : int formula -> int formula -> int system

(** Create an equation that determines if two float formula are equal. *)
val eq_form_float : float formula -> float formula -> float system

(** Create an equation that determines if two int formula are not equal. *)
val ne_form_int : int formula -> int formula -> int system

(** Create an equation that determines if two float formula are not equal. *)
val ne_form_float : float formula -> float formula -> float system

(** Create an equation that determines if two int formula are equal. *)
val gt_form_int : int formula -> int formula -> int system

(** Create an equation that determines if two float formula are equal. *)
val gt_form_float : float formula -> float formula -> float system

(** Create an equation that determines if two int formula are not equal. *)
val gte_form_int : int formula -> int formula -> int system

(** Create an equation that determines if two float formula are not equal. *)
val gte_form_float : float formula -> float formula -> float system

(** Create an equation that determines if two int formula are equal. *)
val lt_form_int : int formula -> int formula -> int system

(** Create an equation that determines if two float formula are equal. *)
val lt_form_float : float formula -> float formula -> float system

(** Create an equation that determines if two int formula are not equal. *)
val lte_form_int : int formula -> int formula -> int system

(** Create an equation that determines if two float formula are not equal. *)
val lte_form_float : float formula -> float formula -> float system

(** {2 Shorthand Fundamental [system] Constructors} *)

(** Shorthand for creating a equation that determines if two int formulas are equal. *)
val (=?) : int formula -> int formula -> int system

(** Shorthand for creating a bool formula that determines if two float formulas are equal. *)
val (=.) : float formula -> float formula -> float system

(** Shorthand for creating a equation that determines if two int formulas are not equal. *)
val (<>?) : int formula -> int formula -> int system

(** Shorthand for creating a bool formula that determines if two float formulas are not equal. *)
val (<>.) : float formula -> float formula -> float system

(** Shorthand for creating a equation that determines if for two int formulas LHS > RHS. *)
val (>?) : int formula -> int formula -> int system

(** Shorthand for creating a bool formula that determines if two float formulas LHS > RHS. *)
val (>.) : float formula -> float formula -> float system

(** Shorthand for creating a equation that determines if for two int formulas LHS >= RHS. *)
val (>=?) : int formula -> int formula -> int system

(** Shorthand for creating a bool formula that determines if for two float formulas LHS >= RHS. *)
val (>=.) : float formula -> float formula -> float system

(** Shorthand for creating a equation that determines if for two int formulas LHS < RHS. *)
val (<?) : int formula -> int formula -> int system

(** Shorthand for creating a bool formula that determines if two float formulas LHS < RHS. *)
val (<.) : float formula -> float formula -> float system

(** Shorthand for creating a equation that determines if for two int formulas LHS <= RHS. *)
val (<=?) : int formula -> int formula -> int system

(** Shorthand for creating a bool formula that determines if for two float formulas LHS <= RHS. *)
val (<=.) : float formula -> float formula -> float system

(** {2 Combine [system] types} *)

(* Connect equations via and or or. Shorthand mentioned later. *)

(** And two equations together *)
val and_eqs : 'g system -> 'g system -> 'g system

(** Or two equations together *)
val or_eqs : 'h system -> 'h system -> 'h system

(** {2 Shorthand Combine [system] types} *)

(** Shorthand for anding two equations together. *)
val (&&) : 'i system -> 'i system -> 'i system

(** Shorthand for oring two equations together. *)
val (||) : 'j system -> 'j system -> 'j system

(** Create a binary operation from an operation that takes 'a type inputs. In
 essence, lift an existing binary operator work on formulas. *)
val formula_reg_bin : ('k -> 'k -> 'k) -> ('k formula -> 'k formula -> 'k formula)


(** {1 [source] Operations} *)

type 'l source

(** Make a source to listen with. *)
val make_source : unit -> 'm source

(** Listen with a specified source *)
val listen : 'n source -> unit

(** Note: 
  Refining event listeners via the [source] type with [exec_while] is mentioned in the next section! 
*)

(** {1 Event listener constructors} *)

(** Listen and execute a function when a [formula] changes value. *)
val on_change : 'o formula -> ('o -> 'o -> unit) -> unit

(** Listen and execute a function when a [system] changes value. *)
val system_change : 'p system -> (bool -> bool -> unit) -> unit

(** Listen and execute when a [system] becomes true. *)
val when_satisfied : 'q system -> (unit -> unit) -> unit

(** Source event listener. Suppose [s] has registered a system [eq].
    Execute function if [listen s] is called and supply the current value of [eq].
*)
val exec_always : 'r source -> 'r system -> (bool -> unit) -> unit

(** Source event listener. Suppose [s] has registered a system [eq].
    Execute function if the [eq] is currently [true] and [listen s] is called.
*)
val exec_while : 's source -> 's system -> (unit -> unit) -> unit

