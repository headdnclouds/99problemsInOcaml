

(*----------------------------------------EASY--------------------------------------*)
(* 1. Write a function last : 'a list -> 'a option that returns the last element of a list. (easy)*)
let last liste =
  let rec treatListe liste elem =
    match liste with
      [] -> None
    | [x] -> Some x
    | (x::reste) -> treatListe reste x in
  treatListe liste "" ;;

last [ "a" ; "b" ; "c" ; "d" ];;
last[];;

(* 2. Find the last but one (last and penultimate) elements of a list. (easy)*)
let rec last_two liste =
  match liste with
    [] -> None
  | [x;y] -> Some (x,y)
  | (x::reste) -> last_two reste;;

last_two [ "a" ; "b" ; "c" ; "d" ];;
last_two["a"];;

(* 3. Find the k'th element of a list. (easy)*)
let at k liste =
   let rec aux liste index =
       match liste with
           [] -> None
         | (x::reste) -> if (index == k) then Some x else traiterListe reste (index+1)
   in
   aux liste 1
;;

at 3 [ "a" ; "b"; "c"; "d"; "e" ];;
(*- : string option = Some "c" *)
at 3 [ "a" ];;
(* - : string option = None *)

(* 4. Find the number of elements of a list. (easy)*)
let length liste =
    let rec aux liste count =
       match liste with
            [] -> count
          | (x::reste) -> aux reste (count+1)
    in
    aux liste 0;;

length [ "a" ; "b" ; "c"];;
(*- : int = 3*)
length [];;
(*- : int = 0*)

(* 5. Reverse a list. (easy)*)
let rev liste =
  let rec aux liste newListe =
    match liste with
      [] -> newListe
    | (x::reste) -> aux reste (x::newListe)
  in
  aux liste [];;

rev ["a" ; "b" ; "c"];;
(*- : string list = ["c"; "b"; "a"]*)

(* 6. Find out whether a list is a palindrome. (easy)*)
let is_palindrome liste =
    let reversedListe = rev liste in
    liste=reversedListe;;

is_palindrome [ "x" ; "a" ; "m" ; "a" ; "x" ];;
(*- : bool = true*)
not (is_palindrome [ "a" ; "b" ]);;
(*- : bool = true*)

(*----------------------------------------MEDIUM--------------------------------------*)
(* 7. Flatten a nested list structure. (medium)*)
(* There is no nested list type in OCaml, so we need to define one
     first. A node of a nested list is either an element, or a list of
     nodes. *)
type 'a node =
  | One of 'a
  | Many of 'a node list;;
(*type 'a node = One of 'a | Many of 'a node list*)

let flatten nestedList =
  let rec aux newList = function
      [] -> newList
    | (One x :: rest) -> aux (x::newList) rest
    | (Many liste:: rest) -> aux (aux newList liste) rest
  in
  List.rev(aux [] nestedList);;

flatten [ One "a" ; Many [ One "b" ; Many [ One "c" ; One "d" ] ; One "e" ] ];;
(*- : string list = ["a"; "b"; "c"; "d"; "e"]*)

(* 8. Eliminate consecutive duplicates of list elements. (medium)*)

