open Sexplib.Std
open Sexplib.Conv

type rule =
  {
	data: string;
	address: string * int
  } [@@deriving sexp]

type t = rule list [@@deriving sexp]


let load filename =
  Sexplib.Sexp.load_sexp filename |> t_of_sexp
		 
