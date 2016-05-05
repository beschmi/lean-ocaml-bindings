(* * Lean (high-level) interface. *)
(** This module exports a high-level interface to the Lean API. *)

open LeanUtil

(* ** Types *)
(** {1 Type abbreviations } *)

type name                = LeanInternal.name
type list_name           = LeanInternal.list_name
type options             = LeanInternal.options
type univ                = LeanInternal.univ
type list_univ           = LeanInternal.list_univ
type ios                 = LeanInternal.ios
type env                 = LeanInternal.env
type decl                = LeanInternal.decl
type cert_decl           = LeanInternal.cert_decl
type expr                = LeanInternal.expr
type list_expr           = LeanInternal.list_expr
type macro_def           = LeanInternal.macro_def
type inductive_type      = LeanInternal.ind_type
type list_inductive_type = LeanInternal.list_ind_type
type inductive_decl      = LeanInternal.ind_decl
type type_checker        = LeanInternal.type_checker
type cnstr_seq           = LeanInternal.cnstr_seq
type binder_kind         = LeanInternal.binder_kind
type lean_exc            = LeanInternal.lean_exc
                             
(** {1 Submodules } *)

(* ** Name *)

(** Manipulating Lean names (lists of strings/numbers). *)
module Name : sig
  type view =
    | Anon                 (** [Anon]: The anonymous name. *)
    | Str of name * string (** [Str(n,s)]: the name [n.s].   *)
    | Idx of name * uint   (** [Idx(n,i)]: the name [n.i].   *)

  (** Convert [name] into [view]. *)
  val view : name -> view

  (** Create anonymous [name]. *)
  val mk_anon : unit -> name

  (** Append [string] to [name]. *)
  val append_str : name -> str:string -> name

  (** Append [uint] index to [name]. *)
  val append_idx : name -> idx:uint -> name

  (** Create name for given string. Appends string to anonymous name. *)
  val mk_str : string -> name

  (** Create name for given string and index. Appends index to anonymous name. *)
  val mk_idx : string -> uint -> name

  (** Test if given [name]s are equal. *)
  val eq : name -> name -> bool

  (** Test if first [name] is smaller than second [name]. *)
  val lt : name -> name -> bool

  (** Test (quickly) if first [name] is smaller than second [name].
      (FIXME: document difference to [lt]) *)
  val quick_lt : name -> name -> bool
  
  (** Convert [name] to [string]. *)
  val to_string : name -> string

  (** Pretty printer for names. *)
  val pp : F.formatter -> name -> unit

  (** Unsafe pointer conversion for calling into OCaml from C. *)
  include (LeanInternal.UnsafeVoidp with type t := name)

  (** Lists of names. *)
  module List : (module type of LeanInternal.ListName)
end

(* ** Universes *)

(** Manipulating Lean universe expressions. *)
module Univ : sig
  (* It's nicer to have all types directly included for ocamldoc.
  include (module type of LeanInternal.Univ) *)
  type t = univ

  (** Create the zero universe. *)
  val mk_zero : unit -> t

  (** Create the successor of the given universe. *)
  val mk_succ : t -> t

  (** Create universe denoting the maximum of the given universes. *)
  val mk_max : t -> t -> t

  (** Create universe denoting the [Imax] of the given universes. *)
  val mk_imax : t -> t -> t

  (** Create the universe parameter with the given name. *)
  val mk_param : name -> t

  (** Create a global universe with the given name. *)
  val mk_global : name -> t

  (** Create a universe meta-variable with the given name. *)
  val mk_meta : name -> t
  
  (** Return the normal-form of the given universe. *)
  val normalize : t -> t

  (** Test equality of given universes. *)
  val eq : t -> t -> bool

  (** Test if first [universe] is smaller than second [universe]. *)
  val lt : t -> t -> bool
 
  (** Test (quickly) if first [name] is smaller than second [name].
      (FIXME: document difference to [lt]) *) 
  val quick_lt : t -> t -> bool

  (** Test if first [universe] is greater-or equal than second [universe]. *)
  val geq : t -> t -> bool

  (** Convert [universe] to [string]. *)
  val to_string : t -> string

  (** Convert [universe] to [string] with given [options]. *)
  val to_string_using : t -> options -> string

  (** Instantiate the parameters with the given universes. *)
  val instantiate : t -> list_name -> list_univ -> t

  (** View for universe. *)
  type view =
    | Zero                  (** [Zero]: The zero universe. *)
    | Succ   of univ        (** [Succ(u)]: Successor of the universe [u]. *)
    | Max    of univ * univ (** [Max(u1,u2)]: Maximum of [u1] and [u2]. *)
    | Imax   of univ * univ (** [IMax(u1,u2)]: Denotes [u2] if [u2=Zero], otherwise [Max u1 u2] *)
    | Param  of name        (** [Param]: Universe parameter with the given name. *)
    | Global of name        (** [Global]: Reference to a global universe. *)
    | Meta   of name        (** [Meta]: Meta variable with the given name. *)

  (** Convert [univ] into [view]. *)
  val view : univ -> view

  (** Pretty printer for universes. *)
  val pp : F.formatter -> univ -> unit

  (** Unsafe pointer conversion for calling into OCaml from C. *)
  include (LeanInternal.UnsafeVoidp with type t := univ)

  (** Lists of universes. *)
  module List : (module type of LeanInternal.ListUniv)
end

(* ** Local const *)

(** Expressions denoting local constants. We use this to refine the
    types in our interfaces. *)
module LocalConst : sig
  type t

  (** Get expression representing the given local constant. *)
  val to_expr : t -> expr

  (** Create local constant with given name and type. *)
  val mk_local_const : name -> expr -> t

  (** Create local constant with additional parameters. *)
  val mk_local_const_ext : binder_kind -> name -> name -> expr -> t

  (** Get the binder-kind of the given local constant. *)
  val binder_kind : t -> binder_kind

  (** Get the name of the given local constant. *)
  val name : t -> name

  (** Get the pretty-printing name of the given local constant. *)
  val pp_name : t -> name

  (** Get the type of the given local constant. *)
  val ty : t -> expr
end

(* ** Expression *)

(** Manipulating Lean expressions. *)
module Expr : sig

  (** Create a bound variable with the given de-Bruijn index. *)
  val mk_var : uint -> expr

  (** Create type for the given universe *)
  val mk_sort : univ -> expr

  (** Create a constant with given name and universe parameters *)
  val mk_const : name -> list_univ -> expr

  (** Create a function application for the given expressions. *)
  val mk_app : expr -> expr -> expr

  (** [mk_lamba bk n ~ty:t e] Creates a a lambda abstraction with binder-kind
      [bk], variable with name [n] of type [t], and body [e].
      The lambda-bound variable  *)
  val mk_lambda : binder_kind -> name -> ty:expr -> expr -> expr

  (** [mk_pi bk n ~ty:t e] Creates a pi abstraction with binder-kind
      [bk], variable with name [n] of type [t], and body [e].
      The lambda-bound variable  *)
  val mk_pi : binder_kind  -> name -> ty:expr -> expr -> expr

  (** [mk_pi bk n ~ty:t e] Creates a macro application to the given
      list of expressions. *)
  val mk_macro : macro_def -> list_expr -> expr

  (** Creates an expression from the local constant with the given name and type. *)
  val mk_local : name -> expr -> expr

  (** Creates an expression from the local constant with the given additional parameters. *)
  val mk_local_ext : name -> name -> expr -> binder_kind -> expr

  (** Creates a meta-variable with the given name and type. *)
  val mk_metavar : name -> expr -> expr

  (** Test equality of given expressions. *)
  val eq : expr -> expr -> bool

  (** Test equality of given macro definitions. *)
  val macro_def_eq : macro_def -> macro_def -> bool

  (** Test if first [expr] is smaller than second [expr]. *)
  val lt : expr -> expr -> bool

  (** Test (quickly) if first [expr] is smaller than second [expr].
      (FIXME: document difference to [lt]) *) 
  val quick_lt : expr -> expr -> bool

  (** Convert [expr] to [string]. *)
  val to_string : expr -> string

  (** Convert [expr] to [string] with given [env] and [ios]. *)
  val to_pp_string : env -> ios -> expr -> string

  (** Pretty printer for expressions corresponding to [view] *)
  val pp_debug : F.formatter -> expr -> unit

  (** Pretty printer for expressions. *)
  val pp : F.formatter -> expr -> unit

  (** Convert [macro_def] to [string]. *)
  val macro_def_to_string : macro_def -> string

  (** View for expression. *)
  type view =
    | Var    of uint
    | Sort   of univ
    | Const  of name * list_univ
    | Local  of LocalConst.t
    | Meta   of name * expr
    | App    of expr * expr
    | Lambda of binder_kind * name * expr * expr
    | Pi     of binder_kind * name * expr * expr
    | Macro  of macro_def * list_expr
    | Let    of unit (* FIXME *)

  (** Convert [expr] into [view]. *)
  val view : expr -> view

  (** Unsafe pointer conversion for calling into OCaml from C. *)
  include (LeanInternal.UnsafeVoidp with type t := expr)

  (** Lists of expressions. *)
  module List : (module type of LeanInternal.ListExpr)
end  

(* ** Declaration *)

(** Manipulating Lean declarations *)
module Decl : sig
  (* Regarding the args order : all optional args must go before last unnamed arg*)
  (** Create an axiom *)
  val mk_axiom : ?univ_params:list_name -> name -> ty:expr -> decl
                                                
  (** Create a constant *)
  val mk_constant : ?univ_params:list_name -> name -> ty:expr -> decl
                                                
  (** Create a definition with an explicit definitional height*)
  val mk_definition : ?univ_params:list_name -> 
                      name ->
                      ty:expr ->
                      value:expr ->
                      height:uint ->
                      conv_opt:bool ->
                      decl
                        
  (** Create a definition where the definitional height 
is computed using information from the environment *)
  val mk_definition_with : env ->
                           ?univ_params:list_name ->
                           name ->
                           ty:expr ->
                           value:expr ->
                           conv_opt:bool ->
                           decl
                             
  (** Create a theorem with an explicit definitional height*)
  val mk_theorem : ?univ_params:list_name ->
                   name ->
                   ty:expr ->
                   proof:expr ->
                   height:uint ->
                   decl

  (** Create a theorem where the definitional height 
is computed using information from the environment *)
  val mk_theorem_with : env ->
                        ?univ_params:list_name ->
                        name ->
                        ty:expr ->
                        proof:expr ->
                        decl

  (** Get the name of a declaration. *)
  val name : decl -> name
                       
  (** Get the list of universe params for a declaration. *)
  val univ_params : decl -> list_name

  (** Get the type of a declaration. *)
  val ty : decl -> expr
                                
  type view =
    (** A constant *)
    | Const 
    (** An axiom *)
    | Axiom 
    (** A definition with the associated value, definitional height, and
     whether to lazy unfold it *)
    | Def   of expr * uint * bool
    (** A theorem with the associated value and definitional height *)
    | Thm   of expr * uint
                        
  (** Convert [decl] into [view]. *)
  val view : decl -> view

  (** Create a certified declaration (may throw a LeanKernelException) *)
  val certify : env -> decl -> cert_decl
                                 
  (** Create a certified declaration (with exception catching) 
   to pattern-match its output.
   Reminder: LeanUtil> type ('a, 'b) trycatch = Success of 'a | Fail of 'b *)
  val try_certify : env -> decl -> (cert_decl, string) trycatch
                                 
  (** Unsafe pointer conversion for calling into OCaml from C. *)
  include (LeanInternal.UnsafeVoidp with type t := decl)
            
  (** Convert [decl] to [string] (with pretty_printing if given the opt pp arg) *)
  val to_string : ?pp: env * ios -> decl -> string

  (** Pretty printer for declarations corresponding to [view] *)
  val pp_debug : F.formatter -> decl -> unit

  (** Pretty printer for declarations. *)
  val pp : F.formatter -> decl -> unit
end
                      
(* ** Environment *)

(** Manipulating Lean environments *)
module Env : sig
  type trust_level = uint

  (** Trust level for all macros implemented in [Lean] *)
  val trust_high : trust_level

  (** Create an empty standard environment with the given trust level. 
   The returned environment is not a descendant of any other environment. *)
  val standard_env : trust_level -> env

                                      
  (** Create an empty hott environment with the given trust level. 
   The returned environment is not a descendant of any other environment. *)
  val hott_env : trust_level -> env

  (** Return the trust level of the given environment *)
  val trust_level : env -> trust_level

  (** Returns [true] if all proofs of a proposition in [Prop] are equivalent *)
  val is_proof_irrel : env -> bool
                                
  (** Returns whether [Prop] is impredicative in the environment *)
  val is_impredicative : env -> bool

  (** Returns [true] iff the environment contains a global universe with the given [name] *)
  val contains_univ : env -> name -> bool

  (** Add to the environment a new global universe with the given [name]
  (throws a [Lean_exception] if the environment already contains such universe) 
   The returned environment is a descendant of the input environemnt. *)
  val add_univ : env -> name -> env
                                  
  (** Returns an environment containing a global universe with the given [name]
  (even when it was already there)  
   The returned environment is a descendant of the input environemnt. *)
  val add_univ_ignore : env -> name -> env

  (** Add to the environment the given certified declaration.
   - Throws a [Lean_exception] if:
     * the [env] is not a descendant of the one used to [certify] the [decl],
     * the [env] already contains a declaration with the same [name] as the [cert_decl]
   - The returned environment is a descendant of the input environment. *)
  val add_cert_decl : env -> cert_decl -> env

  (** Replace the axiom that has the name of the given [cert_decl] with that [cert_decl] 
   - Throws a [Lean_exception] if:
     * the [env] is not a descendant of the one used to [certify] the [decl],
     * the [env] does not contain an axiom with the [name] of the [cert_decl]
     * the [cert_decl] is not a [theorem]
   - The returned environment is a descendant of the input environment. *)
  val replace_axiom : env -> cert_decl -> env

  (** Returns [true] iff the environment contains a declaration with the given [name] *)
  val contains_decl : env -> name -> bool

  (** Optional 'lookup' of a [decl] with the given [name] from the given [env] *)
  val get_opt_decl : env -> name -> decl option

  (** Returns [true] iff the first [env] is a descendant of the second [env] *)
  val is_descendant : env -> env -> bool
                                      
  (** Syntaxic sugar: [env' <| env] returns [true] iff [env'] is a descendant of [env] *)
  val (<|) : env -> env -> bool
                             
  (** Returns a new [env] with an empty "history"; it is a descendant of itself only *)
  val forget : env -> env
end
               
(* ** Option *)

(* ** IO state *)

(*
module Ios : sig(*
  include BaseView*)
  val mk : ?options:options -> unit -> ios
end

(* ** Environment *)

module Env : sig(*
  include BaseView*)
  val mk : ?filenames:list_name -> ios -> env
end

(* ** Inductive type *)
(* ** Inductive declaration *)
(* ** Module *)
(* ** Parser *)
(* ** Type checker *)
(* ** Declaration *)

type decl_view =
  | DeclAxiom of Name.t * Expr.ty
  | DeclConst of Name.t * Expr.ty
  | DeclDef   of Name.t * Expr.ty * Expr.t
  | DeclThm   of Name.t * Expr.ty * Expr.t

module Decl : sig(*
  include BaseView with
            type t = decl and
            type view = decl_view*)
  val to_string : ?pp: env * ios -> decl -> string
end

(* ** EnvParser *)

module type LeanFiles = sig
  val _olean : string list
  val _lean : string list
end

module GetExprParser (LF : LeanFiles) : sig
  type t = expr
  val to_string : t -> string
  val to_pp_string : t -> string
  val get_type : t -> t
  val get     : string -> t
  val get_with_univ_params : string -> t * list_name
  val as_1ary : t -> t -> t
  val as_2ary : t -> t -> t -> t
  val as_nary : t -> t list -> t
  val (<@) : t -> t -> t (* Syntactic sugar for "Lean argument feeding" *)

  (* Nats and Integers *)
  val lint_of_int    : int -> t
  val lnat_of_posint : int -> t

  (* Proof obligation generation *)
  val add_proof_obligation :
    ?prefix:string -> ?name:string -> ?univ_params:list_name -> t -> unit
  val proof_obligations_to_string : unit -> string
  val export_proof_obligations : ?univ_params:list_name -> string -> unit
end
*)
