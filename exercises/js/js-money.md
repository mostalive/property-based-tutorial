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
we pass two generators to forall and define a property function with two
arguments:

```javascript 
var moneyOfSameCurrencyAddsUp = p.forall(amount, amount, function(a1, a2) { ... }
```

Create the *amount* generator. Generating the currency is simple, e.g.: 

var currencies = p.elements(["EUR", "USD", "GBP"]);

How do we generate Money instances? We use the generator combinator *pair*
to generate amount-currency pairs and use jsverify's *smap* function to transform the pair into a Money object (and back again).
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

In our domain model, we work with valid objects. This part focuses on
processing and validation outside, 'untrusted' data that is transformed
into domain objects (e.g. data coming in from a web form). We want this transformation to be robust, so that it can
handle any data and only results in valid domain objects when it is able
to (and we don't need to do defensive programming in our domain).

There are two aspects we would like to check:

- For valid strings, is a valid Money object with the correct contents
  created?
- For any string, is the conversion robust? In other words: does the
  conversion always produce either a valid Money object or a rejection
of the input? 



