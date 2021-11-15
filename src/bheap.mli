
type 'a bheap

val empty_bheap: ('a -> 'a -> bool) -> 'a bheap
val add : 'a bheap -> 'a -> 'a bheap
val extract_root : 'a bheap -> ('a * 'a bheap)
