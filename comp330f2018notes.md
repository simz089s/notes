# ___COMP 330 : Theory of Computation___

---

---

---

# 2018-09-05

## Course Outline + Introduction

* Instructor's Office Hours: Mon 1:30---3:00 and Wed 11:00---12:30
* Instructor's Office: McConnell (North Wing) 105N
* Midterm Mc
* ...

## <u>Equivalence Relations</u>


# 2018-09-07

## Partial Order

> A binary relation

It might happen that two elements cannot be compared.

If every pair can be compared
(iv) forall s,t s<=t OR t<=s TOTAL ORDER
(DOES NOT ALWAYS HOLD)

### Well-founded order

x is _minimal_ means there is nothing strictly smaller than x

x is _least_ means x is smaller than anything else.

### Well-foundedness : (W,<=)
For every _non-empty_ subset U of W
$$ (U \subset W \& U \neq \phi) $$
$$ \exists u_0 \in U\ s.t.\ u_0\ is\ minimal\ in\ U. $$

#### Example
(N,<=) is well founded  
(R,<=) is _NOT_ well founded

A well founded _TOTAL_ order is called a well order.

If an order is not well founded there are infinitely long strictly descending sequences.

Induction works if and only if your order is well founded.

### Thm
* An order is inductive if and only if it is well founded

#### Proof
Assume (S,<=) is well founded.

Suppose P(.) is any predicate
& _suppose_ forall x in S (forall y in S y<x => P(y)) => P(x)

Want To Show forall x in S. P(x).

Let U = {x in S | ¬P(x)} (WTS U = \0).  
U has a minimal element u_0
