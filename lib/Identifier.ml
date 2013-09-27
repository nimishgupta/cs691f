let n = ref 0

type t = {
	uid : int option;
	str : string
}

let fresh (x : string) : t =
	incr n;
	{ uid = Some !n; str = x }

let to_string (id : t) : string = id.str


let fresh_from id = fresh id.str

let compare (x : t) (y : t) : int = match (x.uid, y.uid) with
  | None, None -> Pervasives.compare x.str y.str
  | Some m, Some n -> Pervasives.compare m n
  | None, Some _ -> -1
  | Some _, None -> 1

let from_string (x : string) : t = { uid = None; str = x }