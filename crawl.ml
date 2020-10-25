open Util ;;    
open CrawlerServices ;;
open Order ;;
open Pagerank ;;


module MoogleRanker
  = InDegreeRanker (PageGraph) (PageScore)
  (*
     = RandomWalkRanker (PageGraph) (PageScore) (struct 
       let do_random_jumps = Some 0.20
       let num_steps = 1000
     end)
  *)

(* Dictionaries mapping words (strings) to sets of crawler links *)
module WordDict = Dict.Make(
  struct 
    type key = string
    type value = LinkSet.set
    let compare = string_compare
    let string_of_key = (fun s -> s)
    let string_of_value = LinkSet.string_of_set

    (* These functions are for testing purposes *)
    let gen_key () = ""
    let gen_key_gt x () = gen_key ()
    let gen_key_lt x () = gen_key ()
    let gen_key_random () = gen_key ()
    let gen_key_between x y () = None
    let gen_value () = LinkSet.empty
    let gen_pair () = (gen_key(),gen_value())
  end)

(* A query module that uses LinkSet and WordDict *)
module Q = Query.Query(
  struct
    module S = LinkSet
    module D = WordDict
  end)

let print s = 
  let _ = Printf.printf "%s\n" s in
  flush_all();;


(***********************************************************************)
(*    PART 1: CRAWLER                                                  *)
(***********************************************************************)

(* TODO: Build an index as follows:
 * 
 * Remove a link from the frontier (the set of links that have yet to
 * be visited), visit this link, add its outgoing links to the
 * frontier, and update the index so that all words on this page are
 * mapped to linksets containing this url.
 *
 * Keep crawling until we've
 * reached the maximum number of links (n) or the frontier is empty. *)
let rec crawl (n:int) (frontier: LinkSet.set)
    (visited : LinkSet.set) (d:WordDict.dict) : WordDict.dict =
  if n == 0 then d else
  match LinkSet.choose frontier with
  |None->d
  |Some(elt, set)->
    if LinkSet.member visited elt then crawl n set visited d else
    let visited' = LinkSet.insert elt visited in
    (match get_page elt with
    |None->crawl (n-1) set visited' d
    |Some page->
      let words = page.words in
      let d' = List.fold_left (fun xs x -> 
        let linkSet' = 
          (match WordDict.lookup d x with
          |Some set->LinkSet.insert elt set
          |None->LinkSet.singleton elt)
        in
        WordDict.insert xs x linkSet') d words 
      in
      (match page.links with
      |[]->crawl (n-1) set visited' d'
      |links-> 
        let frontier' = (List.fold_left (fun xs x -> LinkSet.insert x xs) set links) in
        crawl (n-1) frontier' visited' d'))
;;

let crawler () = 
  crawl num_pages_to_search (LinkSet.singleton initial_link) LinkSet.empty
    WordDict.empty
;;

(* Debugging note: if you set debug=true in moogle.ml, it will print out your
 * index after crawling. *)
