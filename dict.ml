(* Interfaces and implementations of dictionaries.  A dictionary
 * is used to associate a value with a key.  In our case, we will
 * be using a dictionary to build an index for the web, associating
 * a set of URLs with each word that we find as we crawl the web.
 *)
exception TODO

module type DICT = 
sig
  type key   
  type value 
  type dict

  (* An empty dictionary *)
  val empty : dict 

  (* Reduce the dictionary using the provided function f and base case u. 
   * Our reducing function f must have the type:
   *      key -> value -> 'a -> 'a
   * and our base case u has type 'a.
   * 
   * If our dictionary is the (key,value) pairs (in any order)
   *      (k1,v1), (k2,v2), (k3,v3), ... (kn,vn)
   * then fold should return:
   *      f k1 v1 (f k2 v2 (f k3 v3 (f ... (f kn vn u))))
   *)
  val fold : (key -> value -> 'a -> 'a) -> 'a -> dict -> 'a

  (* Returns as an option the value associated with the provided key. If
   * the key is not in the dictionary, return None. *)
  val lookup : dict -> key -> value option

  (* Returns true if and only if the key is in the dictionary. *)
  val member : dict -> key -> bool

  (* Inserts a (key,value) pair into our dictionary. If the key is already
   * in our dictionary, update the key to have the new value. *)
  val insert : dict -> key -> value -> dict

  (* Removes the given key from the dictionary. If the key is not present,
   * return the original dictionary. *)
  val remove : dict -> key -> dict

  (* Return an arbitrary key, value pair along with a new dict with that
   * pair removed. Return None if the input dict is empty *)
  val choose : dict -> (key * value * dict) option

  (* functions to convert our types to strings for debugging and logging *)
  val string_of_key: key -> string
  val string_of_value : value -> string
  val string_of_dict : dict -> string

  (* Runs all the tests. see "Testing" in the assignment web page *)
  val run_tests : unit -> unit
end



(* Argument module signature to our DICT functors *)
module type DICT_ARG =
sig
  type key
  type value
  val compare : key -> key -> Order.order
  val string_of_key : key -> string
  val string_of_value : value -> string

  (* Use these functions for testing--see "Testing" in the assignment web page *)

  (* Generate a key. The same key is always returned *)
  val gen_key : unit -> key

  (* Generate a random key. *)
  val gen_key_random : unit -> key

  (* Generates a key greater than the argument. *)
  val gen_key_gt : key -> unit -> key

  (* Generates a key less than the argument. *)
  val gen_key_lt : key -> unit -> key

  (* Generates a key between the two arguments. Return None if no such
   * key exists. *)
  val gen_key_between : key -> key -> unit -> key option

  (* Generates a random value. *)
  val gen_value : unit -> value

  (* Generates a random (key,value) pair *)
  val gen_pair : unit -> key * value
end



(* An example implementation of our DICT_ARG signature. Use this struct
 * for testing. *)
module IntStringDictArg : DICT_ARG =
struct
  open Order
  type key = int
  type value = string
  let compare x y = if x < y then Less else if x > y then Greater else Eq
  let string_of_key = string_of_int
  let string_of_value v = v
  let gen_key () = 0
  let gen_key_gt x () = x + 1
  let gen_key_lt x () = x - 1
  let gen_key_between x y () = 
    let (lower, higher) = (min x y, max x y) in
    if higher - lower < 2 then None else Some (higher - 1)
  let gen_key_random =
    let _ = Random.self_init () in
    (fun () -> Random.int 10000)

  (* returns the nth string in lst, or "cow" n > length of list *)
  let rec lst_n (lst: string list) (n: int) : string =
    match lst with
      | [] -> "cow"
      | hd::tl -> if n = 0 then hd else lst_n tl (n-1)

  (* list of possible values to generate *)
  let possible_values = ["a";"c";"d";"e";"f";"g";"h";"i";"j";"k";"m";"n";
                         "o";"p";"q";"r";"s";"t";"u";"v";"w";"x";"y";"z";
                         "zzzzzz";"cheese";"foo";"bar";"baz";"quux";"42"]
  let num_values = List.length possible_values
  (* gen_value will return the string at this current index *)
  let current_index = ref 0
  let gen_value () =
    let index = !current_index in
    if index >= num_values then
      (current_index := 0; lst_n possible_values index)
    else
      (current_index := index + 1; lst_n possible_values index)
  let gen_pair () = (gen_key_random(), gen_value())
end



(* An association list implementation of our DICT signature. *)
module AssocListDict(D:DICT_ARG) : (DICT with type key = D.key
  with type value = D.value) = 
struct
  open Order;;
  type key = D.key;;
  type value = D.value;;
  type dict = (key * value) list;;

  (* INVARIANT: sorted by key, no duplicates *)

  let empty = [] ;;

  let fold f u = List.fold_left (fun a (k,v) -> f k v a) u

  let rec lookup d k = 
    match d with 
      | [] -> None
      | (k1,v1)::d1 -> 
        (match D.compare k k1 with
          | Eq -> Some v1
          | Greater -> lookup d1 k 
          | _ -> None)

  let member d k = 
    match lookup d k with 
      | None -> false 
      | Some _ -> true

  let rec insert d k v = 
    match d with 
      | [] -> [(k,v)]
      | (k1,v1)::d1 -> 
        (match D.compare k k1 with 
          | Less -> (k,v)::d
          | Eq -> (k,v)::d1
          | Greater -> (k1,v1)::(insert d1 k v))

  let rec remove d k = 
    match d with 
      | [] -> []
      | (k1,v1)::d1 ->
	(match D.compare k k1 with 
          | Eq -> d1
          | Greater -> (k1,v1)::(remove d1 k)
          | _ -> d)
	  
  let choose d = 
    match d with 
      | [] -> None
      | (k,v)::rest -> Some(k,v,rest)

  let string_of_key = D.string_of_key
  let string_of_value = D.string_of_value
  let string_of_dict (d: dict) : string = 
    let f = (fun y (k,v) -> y ^ "\n key: " ^ D.string_of_key k ^ 
      "; value: (" ^ D.string_of_value v ^ ")") in
    List.fold_left f "" d

  (****************************************************************)
  (* Tests for our AssocListDict functor                          *)
  (* These are just examples of tests, your tests should be a lot *)
  (* more thorough than these.                                    *)
  (****************************************************************)

  (* adds a list of (key,value) pairs in left-to-right order *)
  let insert_list (d: dict) (lst: (key * value) list) : dict = 
    List.fold_left (fun r (k,v) -> insert r k v) d lst

  (* adds a list of (key,value) pairs in right-to-left order *)
  let insert_list_reversed (d: dict) (lst: (key * value) list) : dict =
    List.fold_right (fun (k,v) r -> insert r k v) lst d

  (* generates a (key,value) list with n distinct keys in increasing order *)
  let generate_pair_list (size: int) : (key * value) list =
    let rec helper (size: int) (current: key) : (key * value) list =
      if size <= 0 then []
      else 
        let new_current = D.gen_key_gt current () in
        (new_current, D.gen_value()) :: (helper (size - 1) new_current)
    in
    helper size (D.gen_key ())

  (* generates a (key,value) list with keys in random order *)
  let rec generate_random_list (size: int) : (key * value) list =
    if size <= 0 then []
    else 
      (D.gen_key_random(), D.gen_value()) :: (generate_random_list (size - 1))

  let test_insert () =
    let pairs1 = generate_pair_list 26 in
    let d1 = insert_list empty pairs1 in
    List.iter (fun (k,v) -> assert(lookup d1 k = Some v)) pairs1 ;
    ()

  let test_remove () =
    let pairs1 = generate_pair_list 26 in
    let d1 = insert_list empty pairs1 in
    List.iter 
      (fun (k,v) -> 
        let r = remove d1 k in
        List.iter 
          (fun (k2,v2) ->
            if k = k2 then assert(lookup r k2 = None)
            else assert(lookup r k2 = Some v2)
          ) pairs1
      ) pairs1 ;
    ()

  let test_lookup () =
    ()

  let test_choose () =
    ()

  let test_member () =
    ()

  let test_fold () =
    ()

  let run_tests () = 
    test_insert() ;
    test_remove() ;
    test_lookup() ;
    test_choose() ;
    test_member() ;
    test_fold() ;
    ()

end    



(******************************************************************)
(* BTDict: a functor that implements our DICT signature           *)
(* using a balanced tree (2-3 trees)                              *)
(******************************************************************)

module BTDict(D:DICT_ARG) : (DICT with type key = D.key
with type value = D.value) =
struct
  open Order

  exception TODO

  type key = D.key
  type value = D.value

  (* A dictionary entry is a (key,value) pair. We compare two (key,value)
   * pairs with the provided key-comparison function D.compare. For example,
   * we may choose to keep a dictionary mapping links to their ranks. In this
   * case, our (key,value) pairs will be (link,rank) pairs, and we compare
   * links using string comparison. *)
  type pair = key * value

  (* Type definition for dictionary, which we choose to represent as a 2-3 Tree.
   * This is almost the same as the binary search tree definition from pset4 and
   * lecture, except we add one more case: a Three-node. 
   *
   * A Three-node contains two pairs and three subtrees: left, middle, and 
   * right, represented by the 3 dicts in the definition below. *)
  type dict = 
    | Leaf
    | Two of dict * pair * dict
    | Three of dict * pair * dict * pair * dict

  (* INVARIANTS: 
   * 2-node: Two(left,(k1,v1),right) 
   * (1) Every key k appearing in subtree left must be k < k1.
   * (2) Every key k appearing in subtree right must be k > k1. 
   * (3) The length of the path from the 2-node to
   *     every leaf in its two subtrees must be the same.  
   * 
   * 3-node: Three(left,(k1,v1),middle,(k2,v2),right) 
   * (1) k1 < k2.
   * (2) Every key k appearing in subtree left must be k < k1. 
   * (3) Every key k appearing in subtree right must be k > k2. 
   * (4) Every key k appearing in subtree middle must be k1 < k < k2.
   * (5) The length of the path from the 3-node to every leaf in its three 
   *     subtrees must be the same. 
   *)


  (* How do we represent an empty dictionary with 2-3 trees? *)
  let empty : dict = Leaf

  (* TODO:
   * Implement fold. Read the specification in the DICT signature above. *)
  let rec fold (f: key -> value -> 'a -> 'a) (u: 'a) (d: dict) : 'a =
    match d with 
    | Leaf->u
    | Two (d1, p1, d2)->
      let (k,v ) = p1 in
      let u' = f k v u in
      (* left *)
      let u'' = fold f u' d1 in 
      (* right *)
      fold f u'' d2 
    | Three (d1, p1, d2, p2, d3)->
      let (k1, v1), (k2, v2) = p1, p2 in 
      (* left *)
      let ul = fold f u d1 in
      (* center *)
      let uc = fold f ul d2 in
      (* right *)
      let ur = fold f uc d3 in
      let u' = f k1 v1 ur in
      f k2 v2 u'



  (* TODO:
   * Implement these to-string functions
   * of_key and of_value are given as anonymous functions to avoid 
   * crashing the program if run while not implemented even if they 
   * are not called (cf of_dict, which is already a function). When 
   * you implement them, you can remove the function wrappers *)
  let string_of_key = D.string_of_key
  let string_of_value = D.string_of_value
  let rec string_of_dict (d: dict) : string = 
    match d with
        | Leaf -> "Leaf"
        | Two(left,(k,v),right) -> "Two(" ^ (string_of_dict left) 
          ^ ",(" ^ (string_of_key k) ^ "," ^ (string_of_value v) ^ "),"
          ^ (string_of_dict right) ^ ")"
        | Three(left,(k1,v1),middle,(k2,v2),right) -> 
          "Three(" ^ (string_of_dict left)
          ^ ",(" ^ (string_of_key k1) ^ "," ^ (string_of_value v1) ^ "),"
          ^ (string_of_dict middle) ^ ",(" ^ (string_of_key k2) ^ "," 
          ^ (string_of_value v2) ^ ")," ^ (string_of_dict right) ^ ")"
      
  (* Debugging function. This will print out the tree in text format.
   * Use this function to see the actual structure of your 2-3 tree. *
   *
   * e.g.      (4,d)   (6,f)
   *         /       |       \
   *      (2,b)    (4,d)     Leaf
   *      /  \     /   \
   *   Leaf  Leaf Leaf  Leaf
   *
   * string_of_tree will output:
   * Three(Two(Leaf,(2,b),Leaf),(4,d),Two(Leaf,(5,e),Leaf),(6,f),Leaf)
   *
   * Note that this tree is NOT balanced, because all the paths from (6,f)
   * to its leaves do NOT all have the same length. *)
  let rec string_of_tree (d: dict) : string = 
    match d with
      | Leaf -> "Leaf"
      | Two(left,(k,v),right) -> "Two(" ^ (string_of_tree left) 
        ^ ",(" ^ (string_of_key k) ^ "," ^ (string_of_value v) ^ "),"
        ^ (string_of_tree right) ^ ")"
      | Three(left,(k1,v1),middle,(k2,v2),right) -> 
        "Three(" ^ (string_of_tree left)
        ^ ",(" ^ (string_of_key k1) ^ "," ^ (string_of_value v1) ^ "),"
        ^ (string_of_tree middle) ^ ",(" ^ (string_of_key k2) ^ "," 
        ^ (string_of_value v2) ^ ")," ^ (string_of_tree right) ^ ")"


  (*When insert_to_tree d k v = (grow,d'), that means:
 * d' is a balanced 2-3 tree containing every element of d as well
 * as the element (k,v).  If grow then height(d') = height(d)+1 else
 * height(d') = height(d).
 *)
 let rec insert_to_tree (d: dict) (k: key) (v: value) : (bool * dict) =
    match d with 
    | Leaf->(true, Two(Leaf, (k,v), Leaf))
    
    | Two (d1, p1, d2)->
      let (k1,v1) = p1 in 
      (match D.compare k k1 with
        |Less->
          let (grew, d') = insert_to_tree d1 k v in
          if grew then 
            let Two (d1', p2, d2') = d' in
            (false, Three (d1', p2, d2', p1, d2))
          else 
            (false, Two (d', p1, d2))
        |Eq-> (false, (Two (d1, (k, v), d2)))
        |Greater->
          let (grew, d') = insert_to_tree d2 k v in
          if grew then 
            let Two (d1', p2, d2') = d' in
            (false, Three (d1, p1, d1', p2, d2'))
          else 
            (false, Two (d1, p1, d')))
    
    | Three (d1, p1, d2, p2, d3)->
      let (k1, v1), (k2, v2) = p1, p2 in
      (match D.compare k k1 with
      |Less->
        let (grew, d') = insert_to_tree d1 k v in
        if grew then 
          (true, Two(d', p1, Two (d2, p2, d3)))
        else
          (false, Three(d', p1, d2, p2, d3))
      |Eq->(false, (Three (d1, (k, v), d2, p2, d3)))
      |Greater->
        (match D.compare k k2 with
        |Less->
          let (grew, d') = insert_to_tree d2 k v in
          if grew then 
            let Two (d1', p', d2') = d' in 
            (true, Two(Two(d1, p1, d1'), p', Two (d2', p2, d3)))
          else
            (false, Three(d1, p1, d', p2, d3))
        |Eq->(false, (Three (d1, p1, d2, (k, v), d3)))
        |Greater->
          let (grew, d') = insert_to_tree d3 k v in
          if grew then 
            (true, Two(Two (d1, p1, d2), p2, d'))
          else
            (false, Three(d1, p1, d2, p2, d'))))



  (* Given a 2-3 tree d, return a new 2-3 tree which
 * additionally contains the pair (k,v)
 * The boolean in insert_to_tree records whether the tree
 * is the same height as the original tree, and is
 * unused here.
 *)
  let insert (d: dict) (k: key) (v: value) : dict =
    snd (insert_to_tree d k v)


  let rec smallest (d: dict) (prev_key: key) (prev_val: value) : (key * value) = 
    match d with 
    | Leaf-> (prev_key, prev_val)
    | Two(d', (k, v), _)-> smallest d' k v
    | Three(d',(k, v), _, _, _)-> smallest d' k v

  (*When remove_from_tree d k v = (shrink, d'), that means:
 * if shrink then height(d') = height(d)-1 else height(d') = height(d);
 * and d' is a balanced 2-3 tree containing every element of d except
 * the element (k,v).
 *)
  let rec remove_from_tree (d: dict) (k: key) : (bool * dict) =
    match d with 
    (* Case where k is not in tree *)
    | Leaf->(false, d)

    (* Case where k is a two node *)
    | Two (d1, p, d2) ->                      
      let (pk, pv) = p in 
      (match D.compare k pk with                               (* remove paren *)
      (* Recurse down left subtree *)
      | Less-> 
        let (shrank, d1') = remove_from_tree d1 k in 
        if shrank then 
          (match d2 with
          | Two (s_d1, s_p, s_d2)->
            (true, Three(d1', p, s_d1, s_p, s_d2))
          | Three (s_d1, s_p1, s_d2, s_p2, s_d3)->
            let left = Two(d1', p, s_d1) in
            let right = Two(s_d2, s_p2, s_d3) in
            (false, Two(left, s_p1, right))
          |_->(false, d))
        else (false, Two(d1', p, d2))
      (* Check if node is terminal or internal and remove *)
      | Eq -> 
        (match d1 with
        |Leaf->(true, Leaf)
        |Two(s_d1, s_p, s_d2)->
          let (pk, pv) = p in
          let (k', v') = smallest d2 pk pv in
          let (shrank, new_d2) = remove_from_tree d2 k' in
          if shrank then 
            (true, Three(s_d1, s_p, s_d2, (k', v'), new_d2))
          else (false, Two(d1, (k', v'), new_d2))
        | Three (s_d1, s_p1, s_d2, s_p2, s_d3)->
          let (pk, pv) = p in
          let (k', v') = smallest d2 pk pv in
          let (shrank, new_d2) = remove_from_tree d2 k' in
          if shrank then 
            let left = Two(s_d1, s_p1, s_d2) in
            let right = Two(s_d3, (k', v'), new_d2) in 
            (false, Two(left, s_p2, right))
          else (false, Two(d1, (k', v'), new_d2)))
      (* Recurse down right subtree *)
      | Greater-> 
        let (shrank, d2') = remove_from_tree d2 k in 
        if shrank then 
          (match d1 with
          | Two (s_d1, s_p, s_d2)->
            (true, Three(s_d1, s_p, s_d2, p, d2'))
          | Three (s_d1, s_p1, s_d2, s_p2, s_d3)->
            let left = Two(s_d1, s_p1, s_d2) in
            let right = Two(s_d3, p, d2') in
            (false, Two(left, s_p2, right))
          |_->(false, d))
        else (false, Two(d1, p, d2')))

    (* Case where k is in a three node *)
    | Three (d1, p1, d2, p2, d3)->
      let (pk1, pv1), (pk2, pv2) = p1, p2 in 
      (match D.compare k pk1 with
      | Less->
        let (shrank, d1') = remove_from_tree d1 k in 
        if shrank then 
          (match d2 with 
          | Two(m_d1, m_p2, m_d2) -> 
            let three = Three(d1', p1, m_d1, m_p2, m_d2) in 
            (false, Two(three, p2, d3))
          | Three(m_d1, m_p1, m_d2, m_p2, m_d3) -> 
            let left = Two(d1', p1, m_d1) in 
            let middle = Two(m_d2, m_p2, m_d3) in 
            (false, Three(left, m_p1, middle, p2, d3))
          |_->(false, d))
        else (false, Three (d1', p1, d2, p2, d3))
      (* Check if node is terminal or internal and remove *)
      | Eq -> 
        (match d3 with 
        | Leaf-> (false, Two(d2, p2, d3))
        | Two(r_d1, r_p1, r_d2)-> 
          let (pk, pv) = p1 in
          let (k', v') = smallest d2 pk pv in
          let (shrank, new_d2) = remove_from_tree d2 k' in
          if shrank then 
            let three = Three(new_d2, p2, r_d1, r_p1, r_d2) in 
            (false, Two(d1, (k', v'), three))
          else (false, Three(d1, (k', v'), new_d2, p2, d3))
        | Three(s_d1, s_p1, s_d2, s_p2, s_d3)-> 
          let (pk, pv) = p1 in
          let (k', v') = smallest d2 pk pv in
          let (shrank, new_d2) = remove_from_tree d2 k' in
          if shrank then 
            let middle = Two(new_d2, p2, s_d1) in 
            let right = Two(s_d2, s_p2, s_d3) in 
            (false, Three(d1, (k', v'), middle, s_p1, right))
          else (false, Three(d1, (k', v'), new_d2, p2, d3)))
      (* Recurse down center subtree *)
      | Greater-> 
        (match D.compare k pk2 with
        | Less -> 
          let (shrank, d2') = remove_from_tree d3 k in 
            if shrank then 
              (match d3 with 
              | Two(r_d1, r_p1, r_d2) -> 
                let three = Three(d2', p2, r_d1, r_p1, r_d2) in 
                (false, Two(d1, p1, three))
              | Three(r_d1, r_p1, r_d2, r_p2, r_d3) -> 
                let middle = Two(d2', p2, r_d1) in 
                let right = Two(r_d2, r_p2, r_d3) in 
                (false, Three(d1, p1, middle, r_p1, right))
               |_->(false, d))
            else
              (false, Three (d1, p1, d2', p2, d3))
        (* Check if node is terminal or internal and remove *)
        | Eq-> 
          (match d2 with 
          | Leaf-> (false, Two(d2, p2, d3))
          | Two(m_d1, m_p1, m_d2)-> 
            let (pk, pv) = p2 in
            let (k', v') = smallest d3 pk pv in
            let (shrank, new_d3) = remove_from_tree d3 k' in
            if shrank then 
              let three = Three(m_d1, m_p1, m_d2, (k', v'), new_d3) in 
              (false, Two(d1, p1, three))
            else (false, Three(d1, p1, d2, (k', v'), new_d3))
          | Three(m_d1, m_p1, m_d2, m_p2, m_d3)-> 
            let (pk, pv) = p2 in
            let (k', v') = smallest d3 pk pv in
            let (shrank, new_d3) = remove_from_tree d3 k' in
            if shrank then 
              let right = Two(m_d3, (k', v'), new_d3) in 
              let middle = Two(m_d1, m_p1, m_d2) in 
              (false, Three(d1, p1, middle, m_p2, right))
            else (false, Three(d1, p1, d2, (k', v'), new_d3)))
         (* Recurse down right subtree *)
        | Greater -> 
          let (shrank, d3') = remove_from_tree d3 k in 
          if shrank then 
            (match d2 with 
            | Two(m_d1, m_p1, m_d2) -> 
              let three = Three(m_d1, m_p1, m_d2, p2, d3') in 
              (false, Two(d1, p1, three))
            | Three(m_d1, m_p1, m_d2, m_p2, m_d3) -> 
              let right = Two(m_d3, p2, d3') in 
              let middle = Two(m_d1, m_p1, m_d2) in 
              (false, Three(d1, p1, middle, m_p2, right))
            | _->(false, d))
          else
            (false, Three (d1, p1, d2, p2, d3'))))

(* given a 2-3 tree d, return a 2-3 without element k *)
  let remove (d: dict) (k: key) : dict =
    snd (remove_from_tree d k)


  (* TODO:
   * Write a lookup function that returns the value of the given key
   * in our dictionary and returns it as an option, or return None
   * if the key is not in our dictionary. *)
  let rec lookup (d: dict) (k: key) : value option =
    match d with 
    | Leaf->None
    | Two (d1, p1, d2)->
      let (k1,v) = p1 in 
      (match D.compare k1 k with
      |Less->lookup d1 k
      |Eq->Some v
      |Greater->lookup d2 k)
    | Three (d1, p1, d2, p2, d3)->
      let (k1, v1), (k2, v2) = p1, p2 in 
      (match D.compare k1 k with 
      |Less->lookup d1 k
      |Eq->Some v1
      |Greater->
        (match D.compare k2 k with 
        |Less->lookup d2 k
        |Eq->Some v2
        |Greater->lookup d3 k))

  (* TODO:
   * Write a height function that takes a dictonary as an argument and
   * returns the distance between the top of the tree and a leaf. A tree
   * consisting of just a leaf should have height 0.*)
  let rec height (d: dict) : int =
    match d with 
    | Leaf->0
    | Two (d1,_,_)-> 1 + height d1
    | Three (d1,_,_,_,_)-> 1 + height d1

  (* TODO:
   * Write a function to test whether a given key is in our dictionary *)
  let member (d: dict) (k: key) : bool =
    match lookup d k with
    | None->false
    | Some _->true

  (* TODO:
   * Write a function that removes any (key,value) pair from our 
   * dictionary (your choice on which one to remove), and returns
   * as an option this (key,value) pair along with the new dictionary. 
   * If our dictionary is empty, this should return None. *)
  let choose (d: dict) : (key * value * dict) option =
    raise TODO

  (* TODO:
   * Write a function that when given a 2-3 tree (represented by our
   * dictionary d), returns true if and only if the tree is "balanced", 
   * where balanced means that the given tree satisfies the 2-3 tree
   * invariants stated above and in the 2-3 tree handout. *)

  (* How are you testing that you tree is balanced? 
   * ANSWER: 
   *    _______________
   *)
  let rec balanced (d: dict) : bool =
    match d with 
    | Leaf->true
    | Two (d1,_, d2)->
      (match d1, d2 with
      |Leaf, Leaf->true
      |Leaf, _ ->false
      |_, Leaf->false
      |_,_->
        if (balanced d1) && (balanced d2) then true
        else false)
    | Three (d1,_, d2,_, d3)->
      (match d1, d2, d3 with
      |Leaf, Leaf, Leaf ->true
      |Leaf,_,_ ->false
      |_,Leaf,_->false
      |_,_,Leaf->false
      |_,_,_->
        if (balanced d1) && (balanced d2) && (balanced d3) then true
        else false)

  (********************************************************************)
  (*       TESTS                                                      *)
  (* You must write more comprehensive tests, using our remove tests  *)
  (* below as an example                                              *)
  (********************************************************************)

  (* adds a list of (key,value) pairs in left-to-right order *)
  let insert_list (d: dict) (lst: (key * value) list) : dict = 
    List.fold_left (fun r (k,v) -> insert r k v) d lst

  (* adds a list of (key,value) pairs in right-to-left order *)
  let insert_list_reversed (d: dict) (lst: (key * value) list) : dict =
    List.fold_right (fun (k,v) r -> insert r k v) lst d

  (* generates a (key,value) list with n distinct keys in increasing order *)
  let generate_pair_list (size: int) : (key * value) list =
    let rec helper (size: int) (current: key) : (key * value) list =
      if size <= 0 then []
      else 
        let new_current = D.gen_key_gt current () in
        (new_current, D.gen_value()) :: (helper (size - 1) new_current)
    in
    helper size (D.gen_key ())

  (* generates a (key,value) list with keys in random order *)
  let rec generate_random_list (size: int) : (key * value) list =
    if size <= 0 then []
    else 
      (D.gen_key_random(), D.gen_value()) :: (generate_random_list (size - 1))

  let test_balance () =
    let d1 = Leaf in
    assert(balanced d1) ;

    let d2 = Two(Leaf,D.gen_pair(),Leaf) in
    assert(balanced d2) ;

    let d3 = Three(Leaf,D.gen_pair(),Leaf,D.gen_pair(),Leaf) in
    assert(balanced d3) ;

    let d4 = Three(Two(Two(Two(Leaf,D.gen_pair(),Leaf),D.gen_pair(),
                           Two(Leaf,D.gen_pair(),Leaf)),
                       D.gen_pair(),Two(Two(Leaf,D.gen_pair(),Leaf),
                                        D.gen_pair(),
                                        Two(Leaf,D.gen_pair(),Leaf))),
                   D.gen_pair(),
                   Two(Two(Two(Leaf,D.gen_pair(),Leaf),D.gen_pair(),
                           Two(Leaf,D.gen_pair(),Leaf)),D.gen_pair(),
                       Two(Two(Leaf,D.gen_pair(),Leaf),D.gen_pair(),
                           Two(Leaf,D.gen_pair(),Leaf))),D.gen_pair(),
                   Two(Two(Two(Leaf,D.gen_pair(),Leaf),D.gen_pair(),
                           Two(Leaf,D.gen_pair(),Leaf)),D.gen_pair(),
                       Three(Two(Leaf,D.gen_pair(),Leaf),D.gen_pair(),
                             Two(Leaf,D.gen_pair(),Leaf),D.gen_pair(),
                             Three(Leaf,D.gen_pair(),Leaf,D.gen_pair(),Leaf))))
    in
    assert(balanced d4) ;

    let d5 = Two(Leaf,D.gen_pair(),Two(Leaf,D.gen_pair(),Leaf)) in
    assert(not (balanced d5)) ;

    let d6 = Three(Leaf,D.gen_pair(),
                   Two(Leaf,D.gen_pair(),Leaf),D.gen_pair(),Leaf) in
    assert(not (balanced d6)) ;

    let d7 = Three(Three(Leaf,D.gen_pair(),Leaf,D.gen_pair(),Leaf),
                   D.gen_pair(),Leaf,D.gen_pair(),Two(Leaf,D.gen_pair(),Leaf))
    in
    assert(not (balanced d7)) ;
    () 

(*
  let test_remove_nothing () =
    let pairs1 = generate_pair_list 26 in
    let d1 = insert_list empty pairs1 in
    let r2 = remove d1 (D.gen_key_lt (D.gen_key()) ()) in
    List.iter (fun (k,v) -> assert(lookup r2 k = Some v)) pairs1 ;
    assert(balanced r2) ;
    ()

  let test_remove_from_nothing () =
    let d1 = empty in
    let r1 = remove d1 (D.gen_key()) in
    assert(r1 = empty) ;
    assert(balanced r1) ;
    ()

  let test_remove_in_order () =
    let pairs1 = generate_pair_list 26 in
    let d1 = insert_list empty pairs1 in
    List.iter 
      (fun (k,v) -> 
        let r = remove d1 k in
        let _ = List.iter 
          (fun (k2,v2) ->
            if k = k2 then assert(lookup r k2 = None)
            else assert(lookup r k2 = Some v2)
          ) pairs1 in
        assert(balanced r)
      ) pairs1 ;
    ()

  let test_remove_reverse_order () =
    let pairs1 = generate_pair_list 26 in
    let d1 = insert_list_reversed empty pairs1 in
    List.iter 
      (fun (k,v) -> 
        let r = remove d1 k in
        let _ = List.iter 
          (fun (k2,v2) ->
            if k = k2 then assert(lookup r k2 = None)
            else assert(lookup r k2 = Some v2)
          ) pairs1 in
        assert(balanced r)
      ) pairs1 ;
    ()

  let test_remove_random_order () =
    let pairs5 = generate_random_list 100 in
    let d5 = insert_list empty pairs5 in
    let r5 = List.fold_right (fun (k,_) d -> remove d k) pairs5 d5 in
    List.iter (fun (k,_) -> assert(not (member r5 k))) pairs5 ;
    assert(r5 = empty) ;
    assert(balanced r5) ;
    () *)

  let run_tests () = 
    test_balance() ;
(*    test_remove_nothing() ;
    test_remove_from_nothing() ;
    test_remove_in_order() ;
    test_remove_reverse_order() ;
    test_remove_random_order() ; *)
    ()

end



(******************************************************************)
(* Run our tests.                                                 *)
(******************************************************************)

(* Create a dictionary mapping ints to strings using our 
 * AssocListDict functor and run the tests *)
module IntStringListDict = AssocListDict(IntStringDictArg) ;;
IntStringListDict.run_tests();;

(* Create a dictionary mapping ints to strings using our 
 * BTDict functor and run the tests.
 * 
 * Uncomment out the lines below when you are ready to test your
 * 2-3 tree implementation. *)

module IntStringBTDict = BTDict(IntStringDictArg) ;;
IntStringBTDict.run_tests();;




(******************************************************************)
(* Make: a functor that creates a DICT by calling our             *)
(* AssocListDict or BTDict functors                               *)
(******************************************************************)
module Make (D:DICT_ARG) : (DICT with type key = D.key
  with type value = D.value) = 
  (* Change this line to the BTDict implementation when you are
   * done implementing your 2-3 trees. *)
  AssocListDict(D)
  (* BTDict(D) *)


let d = 
  Two(Two(Three(Leaf,(168,bar),Leaf,(1038,bar),Leaf),(1552,bar),Three(Leaf,(2697,bar),Leaf,(3249,bar),Leaf)),
  (3340,bar),
  Three(Two(Leaf,(5042,bar),Leaf),(5951,bar),Two(Leaf,(6285,bar),Leaf),(7038,bar),Three(Leaf,(8269,bar),Leaf,(9007,bar),Leaf)))

let a = 
  Two(Two(Three(Leaf,(168,bar),Leaf,(1038,bar),Leaf),(1552,bar),Three(Leaf,(2697,bar),Leaf,(3249,bar),Leaf)),
  (3340,bar),
  Three(Two(Leaf,(5042,bar),Leaf),(8269,bar),Two(Leaf,(9007,bar),Leaf),(7038,bar),Three(Leaf,(8269,bar),Leaf,(9007,bar),Leaf)))