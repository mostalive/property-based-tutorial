% Property Based Testing Hands On - Vending Machine Coinbox exercise
% (c) QWAN - Quality Without a Name - www.qwan.eu
% May 2015

- Rob Westgeest - rob@qwan.eu
- Marc Evers - marc@qwan.eu
- Willem van den Ende - willem@qwan.eu

# A vending machine with a coin box

In this exercise, we are focusing on a (drinks) vending machine, and specifically the cash register part of such a machine. 

Start with modelling a coin box, in which customers can insert different
coins and after making a selection, the appropriate amount is checkout
out. In the end, the coin box should also correcly handle change: any money left over is returned to the customer, using appropriate coins.

## Getting started

Let's start simple. We want to create a Coinbox object with 'insert' and 'checkout'
functions. We can model the coin box as having two collections of coins:
the 'inbox' (coins inserted but not yet processed) and the 'vault'
(coins collected by the coin box after doing checkouts. The coin box
keeps the coins until it is emptied by the owner. The coins from the
vault can also be used to return the correct change, depending on what
types of coins are needed.

We can create representations of all kinds of different coins, but it is
sufficient for now to just model two or three (like 10 cents and 50
cents). 

Starts with a few simple invariants, like:
- the total value of the coins in the inbox should always be >= 0
- the total value of the coins in the vault should always be >= 0

Create a new .js file and add the jsverify boilerplate. Create the
properties one by one, first letting them fail. Write the simplest version of the Coinbox that satisfies a property.

## Mutating state: doing checkouts

Now proceed with implementing checkouts. For now, assume that you do a checkout of all the coins present in the inbox.

a property concerning checkouts: first, a checkout
empties the inbox. Write a property for this, see it fail for the right reason.

Second, a checkout should not modify the total value of the coins in the inbox and vault.

Write the property, see it fail for the right reason. Implement the
checkout function for Coinbox.

Observe closely what happens. Do you get failures? Are they what you
expect? If not, what happens?

Hints and tips
- It might be useful to define toString functions for your Coinbox and
  Coins.
- If you implement the coin box having mutable state, you might run
  into confusing feedback from jsverify with counter examples you might not
  understand. If a test fails, jsverify will use the object to shrink
  and produce a minimal counter example; this does not work correctly if
  you modify the generated object. How can you fix this?

## Inserting coins

Let's add an insert function for the customer to insert one coin. 

What property/properties can you define regarding inserting coins?

Code these properties and implement the insert. Make sure all other
properties keep on being satisfied!

## Possible next steps

If you want to continu with this exercise, you can work on for example:

- Conditional checkout: pass a price to the checkout
  function and only check out if the inbox contains a sufficient amount
  of money.

- Refine the checkout so that a specific amount is checked out and the
  remaining amount is returned as change. How will you make the coin box
  return the right coins? 

