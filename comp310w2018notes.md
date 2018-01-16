# ___COMP 310 : Operating Systems___

---

---

---

# 2018-01-16

## Multiprogramming

~~~
One program counter
|
|=>|A|
    |
|<==|   Switch PC
|
|=>|B|
~~~

### Uniprogramming
- No need for _process_ concept
  * No swapping, batch (one after the other)
- `|--A--|--B--|--C--|`

### Multiprogramming
- Switch
~~~
A |--|        |--|        |--|
B     |--|        |--|        |--|
C         |--|        |--|        |--|
----------------------------------------->
~~~

### Process creation
> Four principle events

- Sys init (daemons (long running bg processes), etc.)
- Exec of process creation syscall
- User request create new proc
- Initiation of batch job

- Process an have child processes

### Process representation
- Info : state and control
- Process control block (PCB)
  * Identifier
  * State Vector = Info necessary to run process p
  * Status
  * Creation fee
  * Priority
  * Other info

Separate per process (even for open files, other res, etc.)
~~~
PCB
---------------
CPU_State
---------------
Processor_ID
---------------
Memory
---------------
Open_Files
---------------
Other_Resources
---------------
Type   |   List
---------------
Parent | Child
---------------
Priority
---------------
...
---------------
~~~

### Create process
- Two ways:
  * Build one from scratch **(Windows)**
    - load code and data into mem
    - create (empty) a _dynamic memory workspace (heap)_
    - create + init _proc ctrl block_
    - make proc known to proc scheduler (dispatcher)
  * Clone existing one **(UNIX)**
    - stop curr proc and save its state
    - copy _code, data, heap_ and _PCB_
    - make proc known to proc scheduler (dispatcher)

#### UNIX proc creation
- `fork()` syscall
  * `fork()` creates identical copy of calling proc
  * after `fork()`, _parent_ proc continues running concurrently w/ its _child_ proc **competing equally for CPU**
- _**Note on App Launch**_
  * When launch app through GUI or term
    - New proc created by forking prev running proc (e.g. shell)
    - ~~
- `fork()` creates id copy of mem but different mem and proc and may request more mem, diverge after that (no sharing) (see next point)
- `fork()` called once, returns twice (for parent and child)
  * Parent gets child PID, child gets 0
  * `if ( fork() != 0 ) { // Parent resumes } else { exec() // Change memory image; // Child resumes }`

### After process creation
- Proc can experience various conditions:
  * No resources to run (processor, mem)
  * Wait for res or event (ex. input)
  * Completed the task and exit
  * Temp suspend (external to proc) waiting for condition

---

---
