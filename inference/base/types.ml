type expr = EVar of string
          | EApp of expr * expr
          | EAbs of string * expr
          | ELet of string * expr * expr

type typee = TVar of string
          | TFun of typee * typee

type scheme = Scheme of string list * typee

module FVSet = Set.Make(String)
type fv_set = FVSet.t

module Subst = Map.Make(String)
type subst = typee Subst.t

module Context = Map.Make(String)
type context = scheme Context.t

module VarFactory = struct
  type var_fac = unit -> typee
  let make_vf () : var_fac =
    let counter = ref 0 in
    fun () ->
      let v = "t" ^ string_of_int !counter in
      incr counter;
      TVar v
end