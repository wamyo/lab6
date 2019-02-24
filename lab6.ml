(*
                              CS51 Lab 6
     Variants, algebraic types, and pattern matching (continued)
 *)

(* Objective: This lab is intended to reinforce core concepts in
   typing in OCaml, including:

     Algebraic data types
     Using ADTs to enforce invariants
     Implementing polymorphic ADTs
 *)

(*======================================================================
                          Part 1: Camlville
                  Variants and invariants revisited

In this lab you'll continue to use algebraic data types to create
several data structures.

First, we'll complete instructive examples on variant types and
enforcing invariants to model residences in Camlville, which can be
houses or apartments. Then, we'll revisit an example from the reading
to implement a polymorphic list as an algebraic data type. Building
from the reading, we'll add some more functionality to this type.

NOTE: As was the case last time, since we ask that you define types in
this lab, you must complete certain exercises before this will compile
with the testing framework on the course grading server. Exercises 1
and 9 are required for compilation.

........................................................................
Exercise 1: There are two kinds of residences in Camlville.  A house
in Camlville has a street name, zip code, and mailbox number. An
apartment in Camlville has a street name, zip code, mailbox number
(consistent for all units in the apartment), and a unit number.

Define a variant type to capture the idea of a residence. What
representation and types make sense? Is there one right answer or
several possibilities?

We will start by assuming there are no restrictions on the zip code,
address and unit other than those given by their types (zip codes must
be strings and mailboxes and unit numbers must be integers). Though
zipcodes on first glance are numbers, they are generally not treated
as numbers. What would be the conceptual meaning of averaging
zipcodes? They also can contain leading zeros (Cambridge,
02138). Consequently, we choose to represent zipcodes as strings.

However, there are only four streets in Camlville (regardless of
zipcode) which are the following:

    Stephansplatz
    Stiftgasse
    Mass Ave
    Main Street

How might you use ADTs to enforce this invariant on street names?
(Try this exercise first. If you can't get it to compile against our
unit tests, see http://tiny.cc/lab6-1 for our solution.)
......................................................................*)


type street =
  | Stephansplatz
  | Stiftgasse
  | MassAve
  | MainStreet ;;

type address = {mailbox : int; street : street; zip_code : string} ;;

type residence =
  | House of address
  | Apartment of int * address ;;


(* After implementing the residence type, please compare with our type
definition at http://tiny.cc/lab6-1. Consider the tradeoffs we may
have considered if you find our definition differs from your own.

To compile against our unit tests, please change your definition to
match ours.  You may comment out your original type definition if you
would like to keep it.

Valid zip codes in Camlville are given as five digits. For example,
12345, 63130, and 02138 are valid zipcodes, but -0004, 2138, and F69A
are not. We'll represent zip codes with strings, but will want to be
able to validate them appropriately. In this lab, we'll use the
"valid_" validation convention from lab 5. *)

(*......................................................................
Exercise 2: Define a function valid_zip that takes in a string and
returns a bool indicating whether or not the string represents a valid
zip code. You may find the function Pervasives.int_of_string_opt and
the String module to be useful.

You do not have to worry about properly handling strings interpreted
as non-base-10 numbers. For example, "0x100" (hexadecimal) may or may
not pass your test but "abcde" definitely should not.
......................................................................*)

let valid_zip (zip : string) : bool =
  String.length zip = 5
  && match int_of_string_opt zip with
    | None -> false
    | Some x -> x >= 0 ;;

(*......................................................................
Exercise 3: Define a function, valid_residence, that enforces proper
zipcodes, and mailbox and unit numbers above 0. It should return true
if it is valid and false otherwise.
......................................................................*)

let valid_residence (res : residence) : bool =
  let valid_address ({mailbox; street; zip_code} : address) : bool =
    valid_zip zip_code && mailbox > 0 in
  match res with
  | House addr -> valid_address addr
  | Apartment(apt_unit, addr) -> valid_address addr && apt_unit > 0 ;;

(*......................................................................
Exercise 4: Time to get neighborly. Define a function that takes two
residences and outputs a bool indicating whether or not they are
neighbors. In Camlville, a neighbor is someone living on the same
street in the same zipcode.

Note: By this definition, a residence is considered to be its own
neighbor.
......................................................................*)

let extract_address (r : residence) : address =
  match r with
  | House addr
  | Apartment(_, addr) -> addr ;;

let neighbors (place1 : residence) (place2 : residence) : bool =
  let addr1, addr2 = extract_address place1, extract_address place2 in
  addr1.street = addr2.street && addr1.zip_code = addr2.zip_code ;;

(*......................................................................
Exercise 5: Lucky 7

Bob is superstitious and wants his street number to be as close to 7
as possible, believing that proximity to this number will bring him
good fortune. (Yes, Bob is an idiot.) Given Bob has only two
residences to choose from, write a function that determines which
residence he will pick. If the two street numbers are equidistant from
7, assume he will always prefer the first one. He has no other
preferences.
......................................................................*)
let close_to_seven (r1 : residence) (r2 : residence) : residence =
  let addr1, addr2 = extract_address r1, extract_address r2 in
  if abs (addr1.mailbox - 7) <= abs (addr2.mailbox - 7) then r1
  else r2 ;;

(*......................................................................
Exercise 6: Bob has recently gotten a raise, so now he has a whole
list of residences to choose from. He has the same preferences,
including that if any number of residences are equidistant to street
number 7, he will choose whichever appeared first in the list.

Implement a function, choose_residence that takes in a list of
residences and outputs the residence Bob should choose given.  Keep in
mind that Bob is picky and so some circumstances may arise in which
there are no houses on Bob's list. For that reason, you should return
a residence option type.
......................................................................*)
let choose_residence =
  let compare_res (best_choice : residence option) (curr_option : residence)
    : residence option =
    match best_choice with
    | None -> Some curr_option
    | Some best -> Some (close_to_seven best curr_option) in
  List.fold_left compare_res None ;;
(*......................................................................
Exercise 7: When buyers purchase a new residence in Camlville, they
must register the street name and address with the town hall, which
creates a record of the residence location and owner.

Implement a function to perform this bookkeeping. It should accept a
residence and a name (which should be a string) and return the
corresponding entry to be made as type town_record, defined below. The
town works hard to prevent fraudulent residences from being entered
into historical records and has asked you to do the same by raising an
Invalid_argument exception when appropriate.
......................................................................*)

type town_record = { residence : residence; name : string } ;;

let record_residence (address : residence) (name : string) : town_record =
  failwith "record_residence not implemented" ;;
(*......................................................................
Exercise 8: Neighbor search.

As part of Bob's promotion, he has been moved to the next floor up at
work. He doesn't yet know any of his coworkers, and so he decides to
search through Camlville's records to determine which of them are his
neighbors. Camlville keeps extensive records, so he doesn't want to
have to look it up manually. Instead, he asks you to do it for him,
since he heard you were learning a lot of useful skills in CS51.

Given two names (strings again) and a list of town_records, search
though the list to determine if the two people are neighbors, as
defined above, and return a bool. Return a
Failure exception in the event that one of Bob's coworkers
does not appear in his list of records.
......................................................................*)

let named_neighbors =
  fun _ -> failwith "neighbors2 not implemented" ;;

(*======================================================================
                         Part 2: Binary trees

Binary trees are a data structure composed of nodes that store a value
from some base type (say, int) as well as a left and a right
subtree. To found this recursive definition, a binary tree can also be
a leaf. For purpose of this lab, we'll say that the leaves store no
further data, though many definitions of binary trees do store data at
the leaves.

Defined in this way, trees resemble lists, except they have two
"tails" rather than one.

........................................................................
Exercise 9: Define a polymorphic binary tree data type called
bintree. Then take a look at the definition that we were expecting at
http://tiny.cc/lab6-2 and make sure that your definition is consistent
with it so that your further work in the lab will be consistent with
our unit tests.
......................................................................*)

type 'a bintree = NotImplemented ;;

(*......................................................................
Exercise 10: Define a function leaf_count : 'a bintree -> int, which
returns the number of leaves in a binary tree.
......................................................................*)

let leaf_count =
  fun _ -> failwith "leaf_count not implemented" ;;

(*......................................................................
Exercise 11: Define a function "find", such that "find tree value"
returns true if value is stored at some node in the tree and false
otherwise.
......................................................................*)
let find = fun _ -> failwith "find not implemented" ;;

(*......................................................................
Exercise 12: Define a function "min_value", such that "min_value tree"
returns the minimum value stored in a tree as an option type, and None
if the tree has no stored values. Use the < operator for comparing
values stored in the nodes of the tree.
......................................................................*)

let min_value (tree : 'a bintree): 'a option =
  failwith "min_value not implemented" ;;

(*......................................................................
Exercise 13: Define a function "map_tree", such that "map_tree fn
tree" returns a tree structured just like its argument tree but
applying the function fn to each of the values in the tree. You'll
want to think carefully about the type of map_tree to maximize its
polymorphism.
......................................................................*)

let map_tree =
  fun () -> failwith "map_tree not implemented" ;;
