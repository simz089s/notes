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
