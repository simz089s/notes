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
