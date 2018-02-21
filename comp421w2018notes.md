# ___COMP 421 : Database Systems___

---

---

---

# 2018-01-15

## E/R
    Entity-Relationship diagrams

### Common mistakes
* One-many relationships (do not need to keep track of every individual relation)
* Accommodate exceptional scenarios (don't assume best or standard case all the time, this is about real world not theoretical)

> ...

### Weak Entities
- Must combine partial keys for uniqueness
- Sports team example (team name + shirt number)
- Depend on other
- Thick outline in E/R diagram
  - Weak entity set in **bold**
  - Relationship set to supporting entity set with key and participating constraint (**bold and arrowed**)
  - Relationship set in **bold**
  - Partial key in weak entity set with **dashed line**
- with respect to other entity set

### Relationships vs Entity Sets
- Payments example, instead of payments relationship, create payments entity sets to be able to have many-many
- Ternary (payment as relationship set) vs 3 binary relationship sets (payments as entity set)

---

---

# 2018-02-21

## Data Storage (cont'd cont'd cont'd)

- Multi-attribute indexing
- Static hashing
- Direct vs Indirect (difference?)

...

## Query Evaluation

- Parser
- Query optimizer
- Plan executor

--

- Query decomposition
  - Queries are composed of **few basic operators**
    - Selection
    - Projection
    - Order By
    - Join
    - Group By
    - Intersection
    - ...

  - Several alternative algorithms for implementing each relational operator
  - Not a single "best" algorithm; efficiency of each implementation depends on factors like size of the ...

--

- Access Path
  - The method used to retrieve a set of tuples from a relation
    - Basic: file scan
    - index plus matching selection condition (index can be used to retrieve only the tuples that satisfy condition)
    - Partitioning and others

--

## Cost model

- In order to compare different alternatives we must estimate how expensive a specific execution is
- Input for cost model:
  - the query,
  - database statistics (e.g., distribution of values, etc.),
  - resource availability and costs: buffer size, I/O delay, disk bandwidth, CPU costs, network bandwidth, ...
- Out cost model
  - Number of I/O = pages retrieved from disk
    - assumption that the root and all intermediate nodes of the B+ are in main memory: only leaf pages may not be in main memory!!!
    - A page P **might be retrieved several times** if many pages are accessed between two accesses of P

## Basic query processing

- Select, project, ...
- Join
  - Nested loop join
  - Sort-merge join
  - ...

---

---
