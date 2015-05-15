% Property Based Testing Hands On - Javascript money exercise
% (c) QWAN - Quality Without a Name - www.qwan.eu
% May 2015

- Rob Westgeest - rob@qwan.eu
- Marc Evers - marc@qwan.eu
- Willem van den Ende - willem@qwan.eu

# Modelling Money

In this exercise, we are going to build a class representing Money, step by step. 
Representing money in software might sound easy, but it is actually non trivial and is often done wrong. 
Doing it wrong can result in small or big rounding errors or just losing
a few millions on a bad day...

## Getting started

Create a new .js file, with the jsverify boilerplate:

```javascript 
var p = require("jsverify");
var _ = require("underscore");

var options = {
  tests: 500,
  quiet: false 
};
```

Create a class to represent money. It is constructed with an
amount and a currency.

```javascript 
function Money(amount, currency) {
  this.amount = amount;
  this.currency = currency;
}

Money.prototype.add = function (money) {
  // ...
};
```

Start with something simple: when we add two amounts of the same
currency, we get a new amount. Our property needs two money objects, so
we pass two arbitraries to forall and define a property function with two
arguments:

```javascript 
var moneyOfSameCurrencyAddsUp = p.forall(amount, amount, function(a1, a2) { ... }
```

Create the *amount* arbitrary. Generating the currency is simple, e.g.: 

var currencies = p.elements(["EUR", "USD", "GBP"]);

How do we generate Money instances? We use the *pair* arbitrary
combinator to generate amount-currency pairs and use jsverify's *smap* function to transform the pair into a Money object (and back again).
'smap' stands for symmetric map, and requires two functions, to do a mapping and to perform the inverse mapping:

```javascript 
var amount = p.pair(p.nat(), currencies).smap(
  function (x) { return  new Money(x[0], x[1]); }, 
  function (money) { return [money.amount, money.currency]; } );
```

Now we can generate Money objects! For now we use natural number for the
amount.

Why do we need to provide a function to convert a money object back to
its components? JSVerify needs the inverse for *shrinking* counter
examples: if a counter example is found, jsverify will try to shrink the
counter example to the smallest example that recreates the failure. This
will help in better understanding the counter example. 

Now add the behaviour that Money can only be added to money of the same
currency. Throw an exception when the currencies are different. Make
sure your property handles this correctly.

```javascript 
var incompatibleCurrencies = {
  name: 'cannot add money from different currencies',
  toString: function () { return this.name; }
}

Money.prototype.add = function (money) {
  if (this.currency !== money.currency) {
    throw incompatibleCurrencies;
  }
  // ...
};
```

Add a new property that checks that adding is not possible whenever the
currencies are different.

## Money allocation

We want to be able to allocate money: divide it in equal
parts, without money getting lost in rounding. 
An example: dividing €100 in 3 should yield [€33, €33, €34] (assuming whole numbers for now).

Define a property that states that no money gets lost when dividing an
amount.

Note that we should make the integer assumption of the amounts more
explicit, otherwise Javascript will just use doubles.

```javascript 
function Money(amount, currency) {
  this.amount = Math.floor(amount);
  ...
```

## Converting from string

We want to be able to create money objects from strings; we support EUR
and USD. Some legal values:
- 100 (default is EUR)
- EUR 10
- USD 300
- USD 40- (negative value)
- EUR 10.000 (. as thousand separator for euros)

And some illegal values:

- USD 4.999 (USD has , as thousand separator and . as cents separator)
- 1,000 (illegal because EUR is default and EUR uses . as thousand
  separator)
- HFL 50 (illegal currency)
- EUR 10bla

In our domain model, we work with valid objects. This part focuses on
processing and validation outside, 'untrusted' data that is transformed
into domain objects (e.g. data coming in from a web form). We want this transformation to be robust, so that it can
handle any data and only results in valid domain objects when it is able
to (and we don't need to do defensive programming in our domain).

There are actually two concerns we need to address:

- Given the input string is valid, will our code create a valid Money
  object with the correct contents? (correctness)
- Is the conversion code robust for every possible input string? The
  code should always either return a valid Money object or a rejection of the input. 
  It should not throw unexpected exceptions or create invalid Money
  objects (robustnesss)

Let's capture these in properties.

Define conversion function that returns a JSON object that is either { money: <valid money
object> } or { error: 'some error message' }

Start e.g. with something like:

```javascript
function moneyFromString(input) {
  var theMoney = new Money(...);
  return { money: theMoney };
}
```

We can start with either correctness or robustness first. It would be
interesting to try both approaches and see whether and how it drives you
to different design decisions.

### Correctness

Given correct input, the conversion should produce valid Money objects. 
How would you define this as property? 

Write an arbitrary that creates valid input strings. You can again use smap to create valid strings
from primitive values.

Hints and tips:
- If you let your arbitrary generate a tuple (array) of an input string together
  with the corresponding amount and currency values, it will be easier.
  for your property function to check correctness: __['EUR 100', 'EUR', 100]__
- Take baby steps; start e.g. with regular input with a currency; add support for the default currency; add
  support for thousand separators.
- Make sure your test fails for the right reason at every step. 

### Robustness

Given any input, the conversion should always produce either a valid
Money object or a validation error: { money: ... } or { error: 'some message' }

Define a property that captures this.

Write an arbitrary that creates all kinds of input strings. Is the
'asciistring' arbitrary suited for this? Why (not)? 

Corner cases that almost look like valid input are particularly
interesting, like "EUR 12jsde", "HFL 30", and "300 EUR". Adapt your arbitrary so that 
it will generate cases like these.

Refine your conversion function and make sure the correctness property
is also satisfied.

Hints and tips:
- jsverify provides the 'oneof' function that combines two arbitraries
  into one that generates values taking values from both arbitraries.

### Reflection

How did defining the properties and the arbitraries influence you in
thinking about the problem?

Where would you like to put the property logic? To what extent is it
test code, to what extent should/could it be part of production code?

What happens if you would start with robustness first and then correctness? 
Do the exercise again, but start with robustness and see if this leads
you to different design decisions.

