(* Functionnal version of the binary heap data structure *)
type 'a bheap

exception Heap_exception of string

(**************  CONSTRUCTORS  **************)
(* Empty binary heap.
 * Need a compare function to establish a priority between its elements. *)
val empty_bheap: ('a -> 'a -> bool) -> 'a bheap

(* Add a new element in a heap. *)
val add : 'a bheap -> 'a -> 'a bheap

(**************  GETTER  **************)
(* Extract the root element of a heap.
 * @raise Heap_exception if the heap is empty. *)
val extract_root : 'a bheap -> ('a * 'a bheap)
