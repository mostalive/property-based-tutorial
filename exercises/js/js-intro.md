% Property Based Testing Hands On - Javascript introduction
% (c) QWAN - Quality Without a Name - www.qwan.eu
% May 2015

- Rob Westgeest - rob@qwan.eu
- Marc Evers - marc@qwan.eu
- Willem van den Ende - willem@qwan.eu


# Prerequisites

You need NodeJS (and NPM) and a terminal plus your favorite editor to do
the Javascript exercise. 
We use the __jsverify__ library in this tutorial. There are many
libraries available in your favorite languages, but the basic principles are
the same.

```bash
https://github.com/jsverify/jsverify
```

You need the following node modules for these exercises: 

```bash
npm install jsverify
npm install underscore
```

# Property based testing concepts

We will introduce the following concepts 
- __properties__ - functions that capture invariants of the system under
  tets
- __arbitraries__, __generators__ and __shrinking__ - functions that produce
  arbitrary input data and minimal counter examples
- restricting input values

# First steps at properties and property based testing

Let's start with a simple example using some math functions, to introduce the basic concepts.
An invariant of the square root and square functions is:

- a number's square is equal to that number: sqrt( x * x ) = x 

In an example based test approach, we would write a small number examples showing that this holds.
We now want to 'proof' that the mentioned invariant holds for all numbers.

Create a new file firststeps.js with:

```javascript
// Import the JSVerify library:
var p = require("jsverify"); 

// Define the 'system under test':

function squared(x) {
  return x * x; 
}
```

We want to show that for all numbers, the following holds:
Math.sqrt(squared(n)) === n, so we define a _property_ by adding to the
file:

```javascript
var squareRootOfNSquaredEqualsN = p.forall(p.integer(), function (n) {
  return Math.sqrt(squared(n)) === n;
});
```

And now comes the magic:

```javascript
var options = { tests: 100, quiet: false };

p.check(squareRootOfNSquaredEqualsN, options);
```

Run the file with: node firststeps.js

What happens?

JSVerify generates input test data and checks if the property holds
(returns true) for all the generated input values. If it finds a case
for which the property does not hold, it fails and returns the counter example.

Currently it is configured to generate 100 test cases. You can specify a
different number using the tests property of the options.

How does it know to generate test data? p.integer() does the job here.
It is an _arbitrary_ that can generate random integers. More about
generators later on.

The test fails because the invariant does not hold for negative numbers.
We should restrict the generated input to natural numbers only (>=0).
Replace p.integer() by p.nat() and run it again.

JSVerify provides arbitraries for all the basic types:

- p.integer() - integers
- p.nat() - natural numbers
- p.number() - doubles
- p.string() - strings (can be empty, can contain non-ascii characters)
- p.asciistring - strings with ascii characters (note: this is a property,
  not a function)

## Another property

Now, define another simple property, for getting the feel of it. Define
as an invariant that the square root of a number is always less than (or equal to?) that number.

You can add an extra property definition and an extra check-call to the same file.

```javascript
var squareRootNIsLessThanN = ...

p.check(squareRootNIsLessThanN, options);
```

Try it out. Play with the conditions to make it fail on purpose and look
at the counter examples that are reported.

## What we have learned

We have learned about _properties_ which are invariants of the code
under test and about _arbitraries_ that generate random, valid inputs for
the code under test.

_Arbitraries vs generators_: in texts about property based testing, you
will encounter both 'arbitraries' and 'generators'. A generator is a
function that can generate arbitrary data of a specific type. An
arbitrary combines a generator with a shrinking function. Shrinking is
used when a counter example is found, to reduce the counter example to
the smallest one that still fails the property.

# A more complicated exercise

Let's put our teeth in a slightly more interesting example, to learn
more about property based testing.

We want to create a smart sorting/grouping feature for restaurant menus.
Our input is a randomly ordered list of food items, each having a course. for example:

```javascript
 [ {course: "starters", dish: "pomodoro soup"}, {course: "main dish", dish: "salmon"},  
{course: "starters", dish: "veggy soup"}, {dish: "nuts"}, 
{course: "main dish", dish: "cannelloni"} ]
```

We want to sort and order this mess so that:
- items are grouped by course
- items without course are put at the end and get the course "other"
- groups of items should be put in this specific order: "main dish", "side dish", "drinks", "other"
- items are sorted alphabetically within the groups

## First property: a sorted result

Let's build our sorting & grouping function in small steps, property by
property. Start with a stubbed function:

```javascript
var p = require("jsverify");

function menuSort(items) {
  return [];
}

// start with the sorting of items, first just a list of strings:

var itemsAreSortedByDish = p.forall(p.nearray(p.asciistring), 
  function (items) {
    var sorted = menuSort(items);

  // use the reduce function on array to compare consequtive pairs
    return sorted.reduce(function (isSortedUntilHere, currentItem, index, array) {
      return isSortedUntilHere && (index == 0 || array[index] > array[index-1]);
    }, true);
  });
```

Add a check call to verify this property. What happens? Implement the
sorting function:

```javascript 
function complicatedSort(values) {
  return values.sort(function (a,b) {
    return a.localeCompare(b);
  });
}
```

Run the test again. What happens now? Make it work!

We are using the p.nearray combinator for arbitraries. Based on a provided
arbitrary (asciistring for ASCII strings in this case), it returns a new arbitrary
that generates arrays of ASCII strings.

## More interesting arbitraries 

We don't want plain strings as inputs, we want to work with more structured
objects instead. How do we generate those?

We already saw how to compose an arbitrary from an array arbitrary and a
string arbitrary. There are more combinators available.

```javascript
var menuItems = p.record({course: p.asciistring, dish: p.asciistring})
```

The record combinator generator generates JSON objects according to the 'specs'. In
this case, we generate objects with a course property of type string and a dish property of type string.

Don't forget to update the sorting function: a.dish.localeCompare(b.dish) 

and the property checks: array[index].dish

## Refining the property and the arbitrary

The current definition of the property checks if all menu items are
sorted, but we want to have them sorted per course. Let's group the
sorted menu items by course and check per group of menu items whether
they're sorted.

```javascript
// 1. We're going to use some useful stuff from the underscore.js library,
// add it at the start of the file:
var _ = require("underscore");

// 2. Extract an 'isSorted' function that checks if an array of menuItems is sorted:
function isSorted(menuItems) { 
  return menuItems.reduce(function(prev, curr, index, array) {
          return prev && (index === 0 || array[index].dish >= array[index-1].dish); 
        }, true);
}

var valuesAreSortedPercourse = p.forall(p.nearray(menuitems), function(values) {
      var sorted = complicatedSort(values);

// 3. Use 'groupBy' from Underscore to get a json object with an array per value of property "course":
      var sortedPerCourse = _.groupBy(sorted, "course");

// 4. Use the Underscore 'values' function to get an array of all the
// arrays, and for each one we can check it is sorted:
      return _.values(sortedPerCourse).every(isSorted);
});
```

Run the tests; does it work?

## Conditional properties

Usually, not all inputs are sensible or valid. We want to restrict checking properties to valid input.

We only want to use non-empty strings for menu items. We could
solve this by adding an extra filtering condition to our property that
short-circuits the property to true for invalid input values:

```javascript
// Let's keep count of the number of inputs we're dropping, so add this
line somewhere at the start of the file:
var droppedInputs = 0;

// ...
// Add a validity check to the property:
  if (values.some(function (menuItem) { return menuItem.dish === ""; })) {
    droppedInputs = droppedInputs + 1;
    return true;
  }

// ...
// Log the number of dropped inputs at the end of the file:
console.log(droppedInputs);
```

Run the tests. Observe what happens.

The disadvantage of filtering input data is that you drop quite a lot of
data and your test becomes less meaningful because of the low number of
actual samples tested. 

An alternative approach is to put restriction on your arbitraries. JSVerify
offers the 'suchthat' function to restrict generated values: 

```javascript
var edibles = p.suchthat(p.asciistring, function (s) { return s.length > 0; });
var courses = p.elements(["main", "drinks", ""]);
var menuitems = p.record({course: courses, dish: edibles})
```

Run the tests and observe what happens. How many inputs are dropped
now? Remove the input check from the property.

We also want the courses for menu items to be be chosen from a limited set:

```javascript
var courses = p.elements(["main", "drinks", ""]);
var menuitems = p.record({course: courses, dish: asciistring})
```

The 'elements' arbitrary generates values from a given array.

## Defining more properties

Property based testing becomes more fun when we actually get to define
and check more properties of the system under test. Let's define
another one for our sorter/grouper.

Add a property that checks that for every menu item in the
sorted result, the course is not empty. Write the property. Make it work
by refining the sorting function.

Another property is whether the menu items are grouped by
their courses. How can you check this? Write the property.


# Integrating it in a unit testing framework

How do you integrate property based tests with your other automated
tests?

Property based testing libraries normally provide integration with
popular testing frameworks. Jsverify integrates for example with the
Javascript testing library Mocha:

```javascript
describe('A proper menu', function () {
  it('has its items sorted by dish', function () {
    p.assert(itemsAreSortedByDish);
  });
});
```

```bash
npm install mocha

mocha <your javascript file.js>
```

