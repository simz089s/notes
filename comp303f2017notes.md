# ___COMP 303 Software Design Patterns___

---

# 2017-09-05

## Bad design
- Compatibility
- Competing constraints
- Fault tolerance
- Security
- Over-engineering

## Construction difficulty
- More tech
- Risks
- Machinery
- Scalability!

## Why learn design?
- Not reinventing the wheel
- Refined techniques
- Being able to organize a project at a higher level (cooking metaphor)
- Developing is organizing

---

```
Individual reading
Lectures
Practice exercises
4 lab tests
2 midterms
1 final
-------------------
10 modules
```

**Read notes and syllabus**

## Lab tests
- Go to lab, given problem, code a solution, show it.
- Must be signed.

## Modules

- **GitHub**

## Office hours
- Tue 10am...
- McConnell in multiprof office

---

## How is software developed?
- Waterfall (plan-based)
- RUP (iterative)
- Scrum (agile)
- Collaborative

### Version control!
> CVS (old)!
Git!

- Head = latest version/snapshot
- Tag version/snapshot

Centralized vs Distributed VCS

---

# 2017-09-07

## Good design
- Dimensions (performance, usability, fault tolerance)
- Not search for perfect (impossible) but acceptable, according to certain characteristics.
- Following coding style = good

### Encapsulation = good!
Otherwise hard to trace bug, mess of code, hard to manipulate what could be objects, difficult to control correctness and avoid bad values...

### How to truly hide implementation?
Avoid encoding and instead try to represent object as its actual concept.

Usability/clarity (car example: two _ints_, which one represents which?)

## Type
> set of values and operations on them (domain!)

### Enums!
Based on objects, syntactic sugar, but hugely useful. Represent values/types without using awkward encoding (_int_) or writing classes.

### How to deal with null ?

### When do you need...
getters? Setters? Public? Private? Package? Static? Etc.

---

# 2017-09-28

## Flyweight

```java
public enum Party
{
  Bloc("Bloc Quebecois"),
  Liberal("Liberal Party of Canada"),
  NDP("New Democratic Party of Canada"),
  Conservative("Conservative Party of Canada"),
  Green("Green Party of Canada");

  private String aName;

  private Party(String pName)
  {
    aName = pName;
  }



  public static getParty(String pHandle)
  {
    for ( Party party : Party.values() )
    {
      if ( party.name().equals(pHandle) )
      {
        return party;
      }
    }
    return null;
  }

  @Override
  public String toString()
  {
    return name();
  }


  /*public static void main(String[] args)
  {
    Party party = Party.valueOf(args[0]);
  }*/
}
```

## Solitaire

### WorkingStack
```java
package comp303m03;

// ...
```

### Game Engine (Model)
_God Object_, _Game Engine_, abstract solution to control state of game

- Collection of states, purely reactive
- Change state, then return
- Just information

_GameInfo_ separate interface (ISP strategy pattern)

---

# 2017-10-03

Function vs Method

Meh

## Testing

> A test is the execution of a program ... against an **oracle**

### Automated testing

#### JUnit

**Eclipse:**

- Java project -> right-click -> Properties -> Build path -> Add library

```java
@Test
public void testCanMoveTo()
{
  assertFalse();
}
```

```java
package comp303m04;

import static org.junit.Assert.assertEquals;

import org.junit.Test;

public class TestAbs
{
  @Test
  public void testZero()
  {
    assertEquals(0, Math.abs(0));
  }

  @Test
  public void testNegative()
  {
    assertEquals(1, Math.abs(-1));
  }
}
```

### Annotation Types

- For typed languages
- @
- Meta-annotation types

```java
import java.lang.annotation.*;

@Retention(RetentionPolicy.RUNTIME)
@Target(ElementType.TYPE)
public @interface Immutable
{
  String inspector() default "Martin";
  String date();
}

@Immutable(date="2016-02-01", inspector="Martin")
public final class Card { ... }
```

### Reflection/Metaprogramming

> What: The data a program deals with is not domain objects but part of the program itself.

> Why: Some programming problems require a solution that involves looking at or changing the problem itself.

Class class class of classes of classes of class class CLASSES

> class

> class

> class

```java
public class Reflector
{
  public static void main(String[] args) throws Exception // Don't do this
  {
    Card card = Card.getCard(Rank.ACE, Suit.CLUBS);
    Class<?> clazz1 = Class.forName("comp303m04.Card");
    Class<Card> clazz2 = Card.class;
    Class<?> clazz3 = clazz2.getSuperclass();
    Class<?> clazzzz = Class.class;

    List<Method> methods = new ArrayList<>();
    for (Method method : Card.class.getDeclaredMethods())
    {
      if (method.getParameterTypes().length == 0)
      {
        methods.add(method);
      }
    }
    Collections.shuffle(methods);
    Method elected = methods.get(0);

    System.out.println(elected.getName() + " -> " + elected.invoke(card).toString());
  }
}
```

### Test order

> @Before

- Each test independent (fresh)
  - Reset object each test
- Fixture (starting point for each test)

### Test with Exception

```java
/**
 * @param pIndex The index of the stack to peek
 * @return The card on top of the stack at index pIndex
 * @pre !aStacks.get(Index.isEmpty());
 */
Card peek(SuitStackIndex pIndex)
{
  assert !aStacks.get(pIndex).isEmpty();
  return aStacks.get(pIndex).peek();
}
```

> How do we test the case of aStacks.get(pIndex).isEmpty() == true?

```java
/**
 * @param pIndex The index of the stack to peek
 * @return The card on top of the stack at index pIndex
 * @throws IllegalArgumentException if isEmpty(pIndex);
 */
Card peek(SuitStackIndex pIndex) throws IllegalArgumentException
{
  if (isEmpty(pIndex))
  {
    throw new IllegalArgumentException();
  }
  return aStacks.get(pIndex).peek();
}
```

The exception is part of the interface!

> INTERFACE!!

As such we must test it, unlike with precondition where we just exclude its possibility.
