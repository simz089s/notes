# ___COMP 302 Programming Languages and Paradigms___
## __Functional programming__
---


# 2017-09-08

## Insert in Binary Search Tree

```ocaml
let rec insert ((x, dx) as e) t = match t with
  | Empty -> Node (e, Empty, Empty)
  | Node ( (y, dy), l, r ) ->
    if x = y then Node ( e, l, r )
    else (if x < y then Node ((y, dy), insert e l, r)
          else Node ((y, dy), l, insert e r)
         )
```

## Lookup

Lookup 'a
-> ('a x 'b) tree
-> b option

### Thm
For all trees _t_, keys _x_, and data _dx_, lookup _x_ (insert (x, dx) t) =>* Some _dx_

Proof by structural induction on the tree _t_

**Base case** _t_ = Empty \
lookup x (insert (x, dx) Empty) \
**by insert** => lookup x (Node ((x, dx), Empty, Empty)) **by lookup** => Some dx

**Case** _t_ = Node ( (y, dy), **l**, **r** ) \
**Ind. Hyp. 1** : For all _x_, _dx_, lookup x (insert (x, dx) l) =>* Some dx \
**Ind. Hyp. 2** : For all _x_, _dx_, lookup x (insert (x, dx) r) =>* Some dx

To Show : lookup x (insert (x, dx) Node ((y, dy), l, r))

x < y : **by insert** => lookup x (Node ((y, dy), insert (x, dx) l, r)) \
**by lookup** => lookup x (insert (x, dx) l) \
**by IH1** => Some dx

**Subcase x=y** lookup x (insert (x ,dx) Node ((y, dy), l, r)) \
**by insert** => lookup x (Node ((x, dx), l, r)) \
**by lookup** => Some dx
...

## Collect
'a tree -> 'a list

...

## Exercises
- Write a cake datatype
- Check if a tree is a BST

---

# 2017-09-29

## Higher-order functions

### Sum, square, cubes, exp...
```ocaml
let square n = n * n
let cube n = n * n * n
let exp2 n =

let rec sum f (a, b) =
  if a > b then 0
  else f (a) + sum f (a+1, b)

let id x = x

let sumInts' (a, b) = sum id (a, b)
let sumSquare' (a, b) = sum square (a, b)
let sumCubes' (a, b) = sum cube (a, b)
let sumExp' (a, b) = sum exp2 (a, b)
```

### Anonymous functions, lambda expressions

**Keywords:**
- _fun_
- _function_

### Going further

```ocaml
(* (int -> int) -> int * int -> int *)
let rec sum f (a, b) inc =
  if (a > b) then 0 else (f a) + sum f (inc(a), b) inc

let rec product f (a, b) inc =
  if (a > b) then 1 else (f a) * product f ()

(* comb : is how we combine -- either * or +
 * f    : is what we do to the a
 * inc  : is how we increment a to get to b
 * base : is what we return when a > b *)
let rec series comb f (a, b) inc base =
  if a > b then base
  else series comb f (inc(a), b) inc (comb base (f a))
(* ('a -> 'a -> 'a) -> ('c -> 'b) -> 'c * 'c -> ('c -> 'c) -> 'a -> 'a *)
```

###  Integrals

```ocaml
(* (float -> float) -> (float * float) -> (float -> float) -> float *)
(* ^f                                     ^inc                      *)
let integral f (a, b) dx =
  dx *. sum f (a +. (dx /. 2.0), b) (fun x -> x +. dx)
  (*           ^f                   ^f *)
```

---

# 2017-10-03

## Higher-order functions (cont.)

### Partial application

**Important functions:**
- map
- filter
- fold (reduce)

---

# 2017-10-05

## Church and the Lambda Calculus!

```
True  = \x.\y.x # Takes two arguments and throws out second one
False = \x.\y.y # throws out first one
```

```ocaml
(* We can also bind variabl to functions. *)
let area : float -> float = function r -> pi *. r *. r

(* or more conveniently, we write usually *)
let area (r:float) = pi *. r *. r
```

Curry function
- take as input a function _f:('a * 'b) -> 'c_
- returns as a result a function _'a -> 'b -> 'c_

```ocaml
(* int * int -> int *)
let plus' (x,y) = x + y
let plus x y = x + y

(* curry : (('a * 'b) -> 'c) -> 'a -> 'b -> 'c *)
(* Note : Arrows are right-associative.        *)
let curry f = (fun x y -> f (x, y))
let curry f =  fun x y -> f (x, y)

let curry_version2 f x y = f (x,y)

let curry_version3 = fun f -> fun x -> fun y -> f (x,y)

fun x y -> plus' (x,y) (* = <fun> *)
```

- ### We never evaluate body of function. See _fun_ we're done.

```ocaml
let uncurry f = fun (x,y) -> f x y

(* swap : ('a * 'b -> 'c) -> 'b * 'a -> 'c *)

let swap f = fun (b,a) -> f (a,b)

(* swap plus' ===> fun (b,a) -> plus' (a,b) *)

let deriv (f, dx) = fun x -> (f (x +. dx) -. f x) /. dx
```

What is the result of evaluating _curry plus'_ ?

=> It's a function!

Result: _fun x y -> plus' x y_

### Generating functions!

```ocaml
let plusSq x y = x * x + y

let horriblyExpensiveThing(x) = x * x

(* How about forcing computation of (x * x) for efficiency? *)

let betterPlusSq x =
  let x = horriblyExpensiveThing(x)
  in fun y -> x + y
```

What is the result of _betterPlusSq 3_ ?

=> _let x = **horriblyExpensiveThing 3** in fun y -> x + y -> fun y -> 9 + y_

---

# 2017-10-06

## Midterm questions

- What is the type? Error? Etc.
- Programming in OCaml
  - Higher-order functions using map, for_all, filter, exists
- Induction proof!
- See _midterm_higher_order_practice.ml_

---

# 2017-10-12

```ocaml
let rec split h = match h with
  | Empty -> Empty, Empty
  | Hand (c, Empty) -> h, Empty
  | Hand (c1, Hand (c2, h')) ->
    let first, second = split h' in
    Hand (c1, first), Hand (c2, second)

type ingredient = Nuts | Gluten | Eggs | Milk
type cup_cake = Cup_cake of float * float * int * ingredient list

let allergen_free allergens l =
  List.filter (function Cup_cake (_, _, _, ings) ->
    List.for_all (function ing ->
      not @@ List.exists (fun al -> al = ing)
    allergens)
  ings) l

let allergen_free2 allergens l =
  List.filter (fun (_, _, _, als) ->
    not @@ List.exists (fun i ->
      List.mem i als)
    allergens) l

```

---

# 2017-10-13

## Multiparadigm languages and imperative programming in OCaml

### Mutable variables (ref)

```ocaml
let x = ref 0
```

> Allocates a reference cell with the name x in memory and initializes it to 0

```ocaml
let x = ref 0
let y = ref 0

(* x =  y true
 * x == y false *)

(* Read a value *)
!x

(* Records *)
let {contents = x} = r

(* Update var *)
x := 3

(* int ref = {contents = 0}*)
let r = ref 0
let s = ref 0

r = s (* true *)
r == s (* false *)

let () = r := 3
!r

let rot (a,b,c) = (c,b,a)

let rotten (a,b,c) = let t = !a in a := !c ; c := t ; (a,b,c)

let triple = (ref 1, ref 2, ref 3)

let triple_rott = rotten triple

let () = for i = 1 to 3 do print_int i
```

- ref function (get messy) -> once defined, fun type is fixed
- shadow refs

---

# 2017-10-17

- An expression has a type
- An expression evaluates to a value (or diverges).


- Type
  - (""return"" type e.g. int -> float * (int -> float))
- Value
  - (""return"" value e.g. <fun>, fun x -> x, 5, 5.0)
- Effect
  - (actual side effect e.g. changes value of x, nothing)

Scratchpad dump:

```ocaml
# let fn = fun x -> x := 3

val fn : int Pervasives.ref -> unit = <fun>

# let set_get_val x y = x := y ; !x

val set_get_val : 'a Pervasives.ref -> 'a -> 'a = <fun>

# let set_get_ref x y = x := y ; x

val set_get_ref : 'a Pervasives.ref -> 'a -> 'a Pervasives.ref = <fun>

# type 'a rlist = Empty | RCons of 'a * ('a rlist) ref

type 'a rlist = Empty | RCons of 'a * 'a rlist Pervasives.ref

# let l1 = ref (RCons (4, ref (RCons (5, ref Empty))))

val l1 : int rlist Pervasives.ref =
  {contents = RCons (4, {contents = RCons (5, {contents = Empty})})}

# let l2 = ref (RCons (3, l1))

val l2 : int rlist Pervasives.ref =
  {contents =
    RCons (3,
     {contents = RCons (4, {contents = RCons (5, {contents = Empty})})})}

(* Cyclic list! *)
# let () = l1 := !l2
```

---

# 2017-10-19

```ocaml
type 'a rlist = Empty | RCons of 'a * ('a rlist) ref

let l1 = ref (RCons (4, ref Empty))
let l2 = ref (RCons (5, l1))

let rec append l1 l2 = match l1 with
  | [] -> l2
  | x :: xs -> x :: (append xs l2)

let rec rapp (r1 : 'a refList) (r2 : 'a refList) = match r1 with
  | {contents = Empty} -> r1 := !r2
  | {contents = RCons (x, xs)} -> rapp xs r2

type counter_obj = {tick : unit -> int ; reset : unit -> unit}

let global_counter = ref 0

let makeCounter () =
  let counter = ref 0 in
    {tick  = (fun () -> (counter := !counter + 1 ; !counter)) ;
     reset = (fun () ->  counter := 0) }

let c = makeCounter ()
let timer = c.tick ()
let () = c.reset ()
```

---

# 2017-10-20

## Exceptions

```ocaml
let head_of_empty_list =
  let head (x::t) = x in
    head []
```

> Warning 8: this pattern-matching is not exhaustive.  
Here is an example of a value that is not matched:  
[]  
Exception: Match_failure ("//toplevel//", 9, 9).

1. Signal Error
   - raise Domain
2. Handle an exception
   - try \<exp\> with Domain -> \<exp\>

```ocaml
exception Domain of string

...
  ... raise (Error "Invalid Input")
...

try ...
with Error "Invalid Input" -> ...
   | Error msg -> ...

type 'a btree = Empty | Node of 'a btree * 'a * 'a btree

let rec findOpt t k = match t with
  | Empty -> None
  | Node (l, (k', d), r) ->
    if k = k' then Some d
    else
      (match findOpt l k with
         | None -> findOpt r k
         | Some d -> Some d)

exception Not_Found = Not_found

let rec find t k = match t with
  | Empty -> raise Not_Found
  | Node (l, (k', d), r) ->
    if k = k' then d
    else (try find l k with Not_Found -> find r k)

(* Pre-order DFS search *)
```

---

# 2017-10-24

## Backtracking

### Coin change problem

- change : int list -> int -> int list
  - list of coins -> amt -> [c1; c3; c5]
  - c1+c3+c5 = amt

**Assumptions:**
- List of coins is ordered
- Each coin in our list can be used as often as needed

```
change [6; 5; 2] 9      |    change [2] 1
change [6; 5; 2] 3      |
[5; 2; 2]               |
```

```ocaml
(* No idea why this function is here. *)
let listToString l = match l with
  | [] -> ""
  | l ->
    let rec toString l = match l with
      | [h]  -> string_of_int h
      | h::t -> string_of_int h ^ ", " ^ toString t
    in
      toString

(* Part actually relevant to class *)
exception Change

let rec change coins amt =
  if amt = 0 then []
  else
    (match coins with
      | [] -> raise Change (* Fail and raise the exception *)
      | coin::cs ->
        (if coin > amt then change cs amt
         else (* coin <= amt *)
          try coin::(change coins (amt - coin))
          with Change -> change cs amt
        )
    )
```

---

# 2017-10-26

## Modules

```ocaml
module type Stack =
  sig
    type stack
    type el
    val empty : unit -> stack
    val is_empty : stack -> bool
    val pop : stack -> stack option
    val push: el -> stack -> stack
    val top : stack -> el option
    val size: stack -> int
    val stack2list : stack -> el list
  end

module Stack : (STACK with type el = int) =
  struct
    type el = int
    type stack = int list

    let empty () : stack = []

    let push i (s : stack) : stack = i::s

    let is_empty s = match s with
      | [] -> true
      | _::_ -> false

    let pop s = match s with
      | [] -> None
      | _::t -> Some t

    let top s = match s with
      | [] -> None
      | h::_ -> Some h

    let rec length s acc = match s with
      | [] -> acc
      | h::t -> length t 1+acc

    let size s = length s 0

    let stack2list(s:stack) = s
  end

module IS = Stack
module FS = FloatStack
```

> Declarations can be more specific in the signature than what the module actually implements

> Tying a module to a module type, we are hiding information!

- Values (functions declarations) or stuff you don't expose in the signature cannot be seen outside (encapsulation, interface separation)

---

# 2017-10-27

## Currency module

```ocaml
module type CURRENCY =
sig
  type t
  val unit : t
  val plus : t -> t -> t
  val prod : float -> t -> t
  val toString : t -> string
end;;

module Float =
struct
  type t = float
  let unit = 1.0
  let plus = ( +. )
  let prod = ( *. )
  let toString (*x*) = string_of_float (*x*)
end;;

module Euro = (Float : CURRENCY);;
module USD = (Float : CURRENCY);;
module CAD = (Float : CURRENCY);;
module BitCoins = (Float : CURRENCY);;

let euro x = Euro.prod x Euro.unit
let usd x = USD.prod x USD.unit
let cad x = CAD.prod x CAD.unit
let bitcoins x = BitCoins.prod x BitCoins.unit

(* What if declare CURRENCY type in Float module instead of
 * Euro, USD, etc. modules? *)

module Float : CURRENCY =
...

module Euro = Float;;
module USD = Float;;
...

(* They now share... (GASP) *)

module type CLIENT =
  sig
    type t (* account *)
    type currency
    val deposit : t -> currency -> currency
    val withdraw : t -> currency -> currency
    val print_balance : t -> string
  end ;;

module type BANK =
  sig
    include CLIENT (* Inheritance *)

    val create : unit -> t

  end

(* Parameterize a module Old_Bank with the functionality
 * provided by the module type CURRENCY *)

(* This is a *** FUNCTOR *** !
 * Takes in another module
 * No need to implement multiple times
 * Just use this and update this when needed *)
module Old_Bank (M : CURRENCY) : (BANK with type currency = M.t) =
struct
  type currency = M.t
  type t = { mutable balance = currency }

  let zero = M.prod 0.0 M.unit
  let neg = N.prof (-1.0)

  let create() = { balance = zero }

  let deposit c x =
    if x > zero then
      c.balance <- M.plus c.balance x;
      c.balance

  let withdraw c x =
    if c.balance > x then
      deposit c (neg x)
    else
      c.balance

  let print_balance c = M.toString (c.balance)
end ;;

(* Old post offices used to be banks *)

module Post = Old_Bank (Euro);;
(* Make the client a bank but only see client stuff
 * Only expose client functionality (defined in CLIENT) *)
module Post_Client : (CLIENT with type currency = Post.currency and type t = Post.t)
= Post;;

module Citybank = Old_Bank (USD);;
module Citybank_Client : (CLIENT with type currency = Citybank.currency and type t = Citybank.t)
= Citybank;;
```

---

# 2017-10-31

## Continuations

### Can every recursive function be written tail recursively?

- Yes! Using continuations.

> A continuation is a stack of functions modelling the call stack i.e. the work we still need to do upon returning.

If we have 1 :: ___ then what goes in the blank/hole? A function! Just like in math.

- app_tl l k code
  - 'a list -> 'a list -> ('a list -> 'a list)

```ocaml
append [1;2] [3;4]
1 :: append [2] [3;4]
1 :: 2 :: append [] [3;4]
c = fun r -> 1 :: 2 :: r
c k -> 1 :: 2 :: [3;4]

(* How do we get to the end of the list and then start
 * pushing everything back?
 *
 * What is the initial value (base case)? The identity
 * function! *)

app_tl [1;2] [3;4] (fun r -> r)
-> app_tl [2] [3;4] (fun r1 -> (fun r -> r) (1 :: r1))
-> app_tl [] [3;4]
   (fun r2 ->
     (fun r1 ->
       (fun r -> r)
     (1::r1))
   (2::r2))
(* Collapsing the call stack *)
-> (fun r2 -> (fun r1 -> (fun r -> r) (1::r1)) (2::r2)) [3;4]
-> (fun r1 -> (fun r -> r) (1::r1)) [2;3;4]
-> (fun r -> r) [1;2;3;4] -> [1;2;3;4]

(* The trade-off is efficiency for small to medium sized
 * lists (!!!) but succeeds for larger sizes *)

let rec append l k = match l with
  | [] -> k
  | h :: t -> h :: append t k

let rec app_tl l k c = match l with
  | [] -> c k (* calling the continuation - passing to the call stack k *)
  | h :: t -> app_tl t k (fun r -> h :: c r)

let rec map l f = match l with
  | [] -> []
  | h :: t -> (f h) :: map t f

let map' l f =
  let rec map_tl l f c =
    match l with
    | [] -> c []
    | h :: t -> map_tl t f (fun r -> c ((f h) :: r))
  in
  map_tl l f (fun r -> r)
```

---
