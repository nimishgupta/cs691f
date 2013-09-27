open SystemF_syntax

module Id = Identifier
module M = Map.Make (Id)
module S = Set.Make (Id)

type t = {
  ids : typ M.t;
  tids : S.t
}
 
 let empty = { ids = M.empty; tids = S.empty }
 
 let extend x t env = { env with ids = M.add x t env.ids }

 let lookup x env = 
   try Some (M.find x env.ids)
   with Not_found -> None

let bind_tid x env = { env with tids = S.add x env.tids }

let bound_tid x env = S.mem x env.tids