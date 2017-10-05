# ___COMP 251 Algorithms and Data Structures___

---

# 2017-10-05

## Greedy algorithm

### Interval scheduling
- _i_ and _j_ are **compatible** () when f <= g or
- subset of requests is **compatible**
-
-

1. _s(i)_ earliest request

> ____|--| |--| |--| |--|\
|------------------| <--accept\
-----------------------> t

2. _f(i) - s(i)_ is the smallest
> |---X---| |---X---|\
_____|--V--|\
-----------------------> t

3. For each request compute the ~~ of requests ~~

4. #### Greedy algo
   - This one works
   - sort requests so that _f(i_1) <= f(i_2) <= f(i_3)_
     ```
     A = \0                     # Set of total requests
     for (j = 1 to r) {         # O(n)
       if (j compatible with A) # O(1)
        A <- A \/ {j}
     }
     return A
     ```

   - Running time: _O(n logn)_ for sorting\
     _j* f(j) >= f(j*)_\
     _for all i in A f(i) <= f(j*)_\
     _______|-j-|\
     ____|-j-|\
     |-j*-|

##### Theorem
> This greedy alg. returns the optimal solution

|A| = |O| - optimal solution ~~

**Stays ahead**

_|A| = k_ , _|O| = m_

is ordered by the starting and finishing time

_for every j in O, f(i in A) <= f(j)_

###### Lemma
> _For all z? <= k, f(i\_z) <= f(j\_z)_\
_z=1, f(i\_?) <=< f(j\_?)_\
_____ A^ ______ O^

Proof Contradiction k < m

i_1 ... i_k ~~ m=k??

j_1 ... j_k j_k+1 ...

Apply the lemma with z=k ~~

|--i_--| (|--??--|)\
|-----| |------|

f(i_k) <= f(j_k)

>

Sort O by starting time
  - it's also sorted by finishing time.

Holds for all optimal solutions.

##### Classroom scheduling

- Minimum number of classroom?
  - Find point in time with the most parallel/overlapping interval.


###### Def
> depth - the maximum number of requests that have common point in the timeline

**Ex.** depth = 3

###### Claim
> The number of resources is at least d {I_1, .., I_d} - requests with depth d\
at least d

---
