let _parse_line line : cell array =
  let rec impl i style_acc cell_acc current_style collecting_style =
    if i >= String.length line then List.rev cell_acc
    else
      let sym = line.[i] in
      if not collecting_style then
        if sym = '\027' then
          impl (i + 1) (sym :: []) cell_acc current_style true
        else
          impl (i + 1) style_acc
            ({ sym; style = current_style } :: cell_acc)
            current_style collecting_style
      else if sym = 'm' then
        let style = string_of_chars (List.rev (sym :: style_acc)) in
        let new_style =
          if style = "\027[0m" then "" else current_style ^ style
        in
        impl (i + 1) [] cell_acc new_style false
      else impl (i + 1) (sym :: style_acc) cell_acc current_style true
  in
  Array.of_list (impl 0 [] [] "" false)

module Heap = struct
  module Make (M : Map.OrderedType) = struct
    type t = Node of M.t * t * t | Empty

    let is_empty = function Empty -> true | _ -> false

    let rec size heap =
      match heap with Empty -> 0 | Node (_, l, r) -> 1 + size l + size r

    let rec _mem value heap =
      match heap with
      | Empty -> false
      | Node (node_value, l, r) ->
          if node_value = value then true
          else if node_value > value then false
          else if _mem value l then true
          else _mem value r
    (* 
    let heapify start_heap ~comparitor =
      let rec aux heap =
        match heap with Empty -> Empty | Node (v, l, r) -> Empty
      in
      aux start_heap
      *)

    let rec pop ~comparitor = function
      | Empty -> Empty
      | Node (_, l, r) -> (
          match (l, r) with
          | Empty, _ -> r
          | _, Empty -> l
          | Node (lv, _, _), Node (rv, _, _) when comparitor lv rv ->
              Node (lv, pop l ~comparitor, r)
          | Node (_, _, _), Node (rv, _, _) -> Node (rv, l, pop r ~comparitor))

    (*
    let delete value start_heap ~comparitor =
      if not (mem value start_heap) then raise (Invalid_argument "value not found in heap when deleting") else
      let rec delete_aux heap found =
        match heap with
        | Empty -> Empty
        | Node (nv, lh, lr) ->
            let found = nv = value in
            let parent_val, child_val = if found then nv, value else value nv in
            if found then
              (match delete_aux lh found with
                | Node (cnv, _, _) when comparitor nv cnv
                | n -> n
              )
      in
      delete_aux start_heap
      *)

    let rec insert new_value heap ~comparitor =
      match heap with
      | Empty -> Node (new_value, Empty, Empty)
      | Node (node_value, lh, rh) -> (
          let pv, cv =
            if comparitor new_value node_value then (new_value, node_value)
            else (node_value, new_value)
          in
          match (lh, rh) with
          | Empty, _ -> Node (pv, Node (cv, Empty, Empty), rh)
          | _, Empty -> Node (pv, lh, Node (cv, Empty, Empty))
          | Node _, Node _ ->
              if size lh < size rh then Node (pv, insert cv lh ~comparitor, rh)
              else Node (pv, lh, insert cv rh ~comparitor))

    let peek = function
      | Empty -> raise (Invalid_argument "Trying to peek into empty heap")
      | Node (v, _, _) -> v

    let peek_opt = function Empty -> None | Node (v, _, _) -> Some v

    let rec map func heap =
      match heap with
      | Empty -> Empty
      | Node (v, l, r) -> Node (func v, map func l, map func r)

    let rec mem value heap =
      match heap with
      | Empty -> false
      | Node (node_value, l, r) ->
          if node_value = value then true
          else if node_value > value then false
          else if mem value l then true
          else mem value r
  end

  module Min_Heap (M : Map.OrderedType) = struct
    include Make (M)

    let comparitor = fun a b -> M.compare a b <= 0
    let pop = pop ~comparitor
    let insert = insert ~comparitor
  end

  module Max_Heap (M : Map.OrderedType) = struct
    include Make (M)

    let insert = insert ~comparitor:(fun a b -> M.compare a b >= 0)
  end
end
