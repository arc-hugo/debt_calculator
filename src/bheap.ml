type 'a tree = ('a tree option * 'a * 'a tree option * int)
type 'a bheap = ('a tree option * ('a -> 'a -> bool))

exception Heap_exception of string

let empty_bheap (f: ('a -> 'a -> bool)) = (None * f)

let rank = function None -> 0 | Some (_,_,_,r) -> r
let leaf v = Some(None, v, None, 1)

let rec merge (t1: 'a tree option) (t2: 'a tree option) (cmp: ('a -> 'a -> bool) =
   match t1,t2 with
      | None, t | t, None -> t
      | Some (l, v1, r, _), Some(_, v2, _, _) ->
            if not (cmp v1 v2) then merge t2 t1
            else
               let merged = merge r t2 in
               let rank_l = rank l and rank_r = rank merged in
               if rank_l >= rank_r then Some(l, v1, merged, rank_r+1)
               else Some(merged, v1, l, rank_l+1)

let add (bt: 'a bheap) (v: 'a) = let (t * cmp) = bt in
   ((merge t (leaf v) cmp) * cmp)

let extract_root (bt: 'a bheap) = match bt with
   | None, _ -> raise Heap_exception "empty heap"
   | Some(l, v, r, _), cmp -> (v * (merge l r cmp))
