open Util ;;    
open CrawlerServices ;;
open Order ;;
open Nodescore ;;
open Graph ;;


(* Dictionaries mapping links to their ranks. Higher is better. *)
module RankDict = Dict.Make(
  struct 
    type key = link
    type value = float
    let compare = link_compare
    let string_of_key = string_of_link
    let string_of_value = string_of_float
    let gen_key () = {host=""; port=0; path=""}
    let gen_key_gt x () = gen_key ()
    let gen_key_lt x () = gen_key ()
    let gen_key_random () = gen_key ()
    let gen_key_between x y () = None
    let gen_value () = 0.0
    let gen_pair () = (gen_key(),gen_value())
  end)

module PageSet = Myset.Make(
  struct 
    type t = page
    let compare = (fun a b -> link_compare (a.url) (b.url))
    let string_of_t = string_of_page
    let gen () = {url={host=""; port=0; path=""}; links=[]; words=[]}
    let gen_lt x () = gen ()
    let gen_gt y () = gen ()
    let gen_random () = gen ()
    let gen_between x y () = None
  end)

module LinkSet = Myset.Make(
  struct 
    type t = link
    let compare = link_compare
    let string_of_t = string_of_link
    let gen () = {host=""; port=0; path=""}
    let gen_lt x () = gen ()
    let gen_gt y () = gen ()
    let gen_random () = gen ()
    let gen_between x y () = None
  end)

module PageGraph = Graph (
  struct
    type node = link
    let compare = link_compare
    let string_of_node = string_of_link
    let gen () = {host=""; port=0; path=""}
  end)

module PageScore = NodeScore (
  struct
    type node = link
    let string_of_node = string_of_link
    let compare = link_compare
    let gen () = {host=""; port=0; path=""}
  end)

(* Given a bunch of pages, convert them to a graph *)
let graph_of_pages (pages : PageSet.set) : PageGraph.graph =
  (* Only want graph nodes for pages we actually crawled *)
  let crawled_links = 
    PageSet.fold (fun page s -> LinkSet.insert page.url s)
      LinkSet.empty pages 
  in
  let add_links page graph =
    let add_link g dst =
      if LinkSet.member crawled_links dst then
        PageGraph.add_edge g page.url dst
      else g 
    in
      List.fold_left add_link graph page.links
  in
    PageSet.fold add_links PageGraph.empty pages
      
(* The rest of the world wants a RankDict, not a NodeScore. *)

let dict_of_ns (ns : PageScore.node_score_map) : RankDict.dict =
  PageScore.fold (fun node score r -> RankDict.insert r node score) 
    RankDict.empty ns

(* A type for modules that can compute nodescores from graphs *)
module type RANKER = 
sig
  module G: GRAPH
  module NS: NODE_SCORE
  val rank : G.graph -> NS.node_score_map
end


(* Each node's rank is equal to the number of pages that link to it. *)
module InDegreeRanker  (GA: GRAPH) (NSA: NODE_SCORE with module N = GA.N) : 
  ((RANKER with module G = GA) with module NS = NSA) =
struct
  module G = GA
  module NS = NSA
  let rank (g : G.graph) = 
    let add_node_edges ns node =
      let neighbors = match G.neighbors g node with
        | None -> []
        | Some xs -> xs
      in
        List.fold_left (fun ns' neighbor -> NS.add_score ns' neighbor 1.0) 
          ns neighbors 
    in
    let nodes = (G.nodes g) in
      List.fold_left add_node_edges (NS.zero_node_score_map nodes) nodes
end



(*****************************************************************)
(* Eigenvalue Ranker                                            *)
(*****************************************************************)


module type WALK_PARAMS = 
sig
  (* Should we randomly jump somewhere else occasionally? 
    if no, this should be None.  Else it should be the probability of 
    jumping on each step *)
  val do_random_jumps : float option
end

module EigenvalueRanker (GA: GRAPH) (NSA: NODE_SCORE with module N = GA.N) 
  (P : WALK_PARAMS) : 
  ((RANKER with module G = GA) with module NS = NSA) =
struct

  module G = GA
  module NS = NSA

  let dot_product (x:float list) (y:float list) : float = 
    List.fold_left2 (fun a b c -> a +. (b *. c)) 0.0 x y

  let multiply (matrix:float list list) (vector:float list) : float list = 
    List.map (fun v -> dot_product v vector) matrix 

  let get_neighbors (g:G.graph) (node:G.node): float list = 
    let pages = List.rev (G.nodes g) in
    let (link_vector:float list) = [] in
    match G.neighbors g node with 
    | Some neighbors -> 
      let n = 1. /. float_of_int (List.length neighbors) in
      List.fold_left (fun vector page -> 
        if List.mem page neighbors then n::vector
        else 0.0::vector)
        link_vector pages
    | None ->
      let n =  1. /. float_of_int (List.length pages) in
      List.fold_left (fun vector _ -> n::vector) link_vector pages

  let converges (r: float list) (r': float list): bool = 
    let mag = dot_product r r in 
    let distance = List.fold_left2 (fun a b c -> a +. ((b-.c)*.(b-.c))) 0.0 r r' in 
    (distance*.distance) < ((mag*.mag)/.1000000.)

  let rank (g : G.graph) =
    let pages = G.nodes g in
    let n = float_of_int (List.length pages) in
    let link_matrix : float list list = 
      List.map (fun page -> get_neighbors g page) pages in 
    let u = List.fold_left (fun vector _ -> (1.0)::vector) [] pages in 
    let d = match P.do_random_jumps with 
      | None -> 0.0
      | Some (d:float) -> d
    in
    let rec get_rank (r:float list) : float list = 
      let u' = (List.map (fun x -> (d/.n) *. x) u) in
      let l' = multiply link_matrix r in
      let r' = List.map2 (+.) u' (List.map (fun x -> (1.-.d) *. x) l') in
      if converges r r' then r' 
      else get_rank r'
    in
    let ranks = get_rank (List.fold_left (fun vector _ -> (1.0/.n)::vector) [] pages) in 
    let zeroMap = NS.zero_node_score_map pages in 
    let score = List.fold_left2 (fun map page rank -> NS.set_score map page rank) zeroMap pages ranks in
    NS.normalize score
end



(*******************  
TESTS BELOW  *******************)

module TestInDegreeRanker =
struct 
  module G = NamedGraph
  let g = G.add_edge G.empty "a" "b";;
  let g2 = G.add_edge g "a" "c";;
  
  module NS = NodeScore (struct
                           type node = string 
                           let compare = string_compare
                           let string_of_node = fun x -> x
                           let gen () = ""
                         end);;

  module Ranker = InDegreeRanker (G) (NS);;
  let ns = Ranker.rank g2;;
  (* let _ = Printf.printf "NS: %s\n" (NS.string_of_node_score_map ns) ;; *)
  assert ((NS.get_score ns "a") = Some 0.0);;
  assert ((NS.get_score ns "b") = Some 1.0);;
  assert ((NS.get_score ns "c") = Some 1.0);;
  assert ((NS.get_score ns "d") = None);;

  let g3 = G.add_edge g2 "b" "c";;
  let ns2 = Ranker.rank g3;;
  assert ((NS.get_score ns2 "a") = Some 0.0);;
  assert ((NS.get_score ns2 "b") = Some 1.0);;
  assert ((NS.get_score ns2 "c") = Some 2.0);;

end

module TestEigenvalueRanker =
struct 
  module G = NamedGraph
  let g = G.from_edges [("a","b") ; 
                        ("b","c") ;
                        ("c","d") ;
                        ("d","a") ;
                        ("e","f") ;
                        ("f","g") ;
                        ("g","h")]
  
  module NS = NodeScore (struct
                           type node = string 
                           let compare = string_compare
                           let string_of_node = fun x -> x
                           let gen () = ""
                         end);;

  module Ranker = EigenvalueRanker (G) (NS) 
    (struct
       let do_random_jumps = None
     end)

    let ns = Ranker.rank g
    let _ = Printf.printf "Testing EigenvalueRanker:\n NS: %s\n" 
    (NS.string_of_node_score_map ns)
end


