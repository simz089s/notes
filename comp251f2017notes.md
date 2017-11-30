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

# 2017-11-02

## Compression (zip)

### Prefix codes searching

- ABL tree

~~  
frequencies...  
~~

- Huffman coding

Optimal tree construction

__*THM*__ : _Huffman Code_ has the optimal ABL among all prefix codes.

__*Proof*__ : Let's consider an optimal code __*c*__ that satisfies the proportions mentioned in observations _I_ and _II_.

#### The Proof is by induction :

**Base case** : Only one letter, In this case the best we can do is to assign a 1-bit string to this letter and that is what _Huffman Code_ does.

**I.H.** _Huffman Code_ is optimal if we have __*m*__ letters.

**Induction step** : We want to show that Huffman Code is optimal for _m+1_ letters.

Let c be an optimal code as described above. Consider the tree of __*c*__ :

Optimal tree (not Huffman tree, at least not proven yet)
~~~
o-----.--.--.--.
|     |  |  |
.--.  .
|
.--.--b
|  |
.  a
~~~
> The two least frequent letters a,b are as in the picture.

Consider the same text but replace occurences of both _**a**_ and _**b**_ with a new character __*|ab|*__. The new text has _**m**_ characters.

~~~
ex : a b c c c a b
           |
     # # c c c # #
~~~

Let's remove the leaves _**a**_,_**b**_ from the optimal tree and assign _**|ab|**_ to their parent (now a leaf)

#### Call the new code _**c'**_

~~~
       ABL
        |
\sum_x f_x |c(x)|

c(x) = c(x')

~~~

~~
By induction hypothesis the Huffman coding applied to the new text (the one _**|ab|**_ character) leads to a code with

_**ABL <= ABL(c')**_

Now we compare Huffman Coding of the new text to the old text.

~~~
o-----.--.--.--.
|     |  |  |
.--.  .
|
.--#  --b
|
.  |
   a
~~~
> Huffman Code of new file

ABL (Huffman original)

= ABL (Huffman for the #) + _f_a_ + _f_b_

_**I.H.**_ ABL (Huffman _f\_.._ #) <= ABL(c') :

showed earlier >= ABL(c') = ABL(c) + _f_a_ + _f_b_  
 ||  
\\/  
ABL(Huffman original) <= ABL(c)

---

## Divide and conquer

- Break up the input into several parts
- Solve each part _recursively_
- Combine the solutions to sub-problems into a solution for the original problem

### Example : _Merge Sort_

- Divide the array into two equal parts.
- Sort each part recursively.
- Merge the two parts into one sorted array

#### Sort the letters of ALGORITHM

~~~
A L G O R | I T H M S
~~~
> Divide : O(1)
~~~
A G L O R | H I M S T
~~~
> recurse and sort parts : 2*T(n/2)
~~~
A G H I L M O R S T
~~~
> Merge : O(n)

_T(n) = 2*T(n/2) + O(n)_

~~~
T(n) ---- T(n/2) ---- T(n/4)  ^
|         |                   | 2*n/2
|         T(n/4)              | 4*n/4
T(n/2) -- T(n/4)              | log(n)
|                             |
|                             |
T(n/4)                        |

...
     ---- T(1)
|
|
T(1)                            n/2*2

|_________________________|
n leaves (n is a power of 2)
~~~
> Merging log = n * log n  
Running Time = O(n * lg n + n) = O(n * log n)

_**Thm**_ :
~~~
T(n) <= 2*T(n/2) + c*n  n>1  
        c               n=1
~~~
then _T(n) <= c n log n_ [n is a power of 2]

_**Pf**_ : Assume n=2^m [m in ???]

We use induction on _**m**_.

_**Base**_ : m = 0 : T(2^a) = T(1) <= c

_**I.H.*__ T(2^m) <= c 2^m log 2^m = c 2^m m

_**Induction Step**_ :

T(2^{m+1}) <= 2 T(2^m) ~~
~~
~~

---
