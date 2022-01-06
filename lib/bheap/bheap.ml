type 'a tree = Node of ('a tree * 'a * 'a tree * int) | Leaf
type 'a bheap = ('a tree * ('a -> 'a -> bool))

exception Heap_exception of string

let empty_bheap (f: ('a -> 'a -> bool)) = (Leaf, f)

let rank = function 
   | Leaf -> 0 
   | Node (_,_,_,r) -> r
let leaf v = Node(Leaf, v, Leaf, 1)

let rec merge (t1: 'a tree) (t2: 'a tree) (cmp: ('a -> 'a -> bool)) =
   match t1,t2 with
      | Leaf, t | t, Leaf -> t
      | Node (l, v1, r, _), Node(_, v2, _, _) ->
            if not (cmp v1 v2) then merge t2 t1 cmp
            else
               let merged = merge r t2 cmp in
               let rank_l = rank l and rank_r = rank merged in
               if rank_l >= rank_r then Node(l, v1, merged, rank_r+1)
               else Node(merged, v1, l, rank_l+1)

let add (bt: 'a bheap) (v: 'a) = 
   let (t, cmp) = bt in
   ((merge t (leaf v) cmp), cmp)

let extract_root (bt: 'a bheap) = match bt with
   | Leaf, _ -> raise (Heap_exception "empty heap")
   | Node(l, v, r, _), cmp -> (v, ((merge l r cmp), cmp))
