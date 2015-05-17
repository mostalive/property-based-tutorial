var p = require("jsverify");
var _ = require("underscore");

var options = {
  tests: 5000,
  quiet: false 
};

/*
 vending machine; has coin box
 insert money, checkout, give change
 correctly handle different coins!

start with: inbox and vault; both contain a bunch of coins (arrays of coins)

you can start with simple properties/invariants like:
- total money inserted (value of coins in inbox) should always be >=0 
- value of coins in vault should always be >= 0

Start with just a few types of coins

And then:
- the total value of inbox+vault remains the same after a checkout 

Observations:
- beware of mutable objects; if you change a generated object, jsverify will produce confusing feedback
- when using floats, summing might occasionally lead to rounding errors

Possible extensions:
- inserting money
- checkout exact amount

- checkout of lower amount
- add a change return bin

*/
function sumOfCoins(coins) {
  return coins.reduce(function (sum, coin, index, values) {
    return sum + coin.value;
  }, 0);
};

function Coinbox(inbox, vault) {
  this.inbox = inbox;
  this.vault = vault;
};

Coinbox.prototype.moneyInserted = function () {
  return sumOfCoins(this.inbox);
};

Coinbox.prototype.balance = function () {
  return sumOfCoins(this.vault);
};

Coinbox.prototype.checkout = function () {
  return new Coinbox([], this.vault.concat(this.inbox));
};

Coinbox.prototype.insert = function (coin) {
  return new Coinbox(this.inbox.concat([coin]), this.vault);
};

Coinbox.prototype.total = function () {
  return this.moneyInserted() + this.balance();
};

Coinbox.prototype.toString = function () {
  return 'COINBOX[' + this.inbox + '/' + this.vault + ']';
};

function Coin(name, value) {
  this.name = name;
  this.value = value;
};

Coin.prototype.toString = function () {
  return '(' + this.name + ')';
};

var cent = new Coin("Cent", 1);
var fiveCents = new Coin("Five Cents", 5);
var tenCents = new Coin("Ten Cents", 10);

var allCoins = [cent, fiveCents, tenCents];

var aCoin = p.elements(allCoins);

var aCoinbox = p
  .pair(p.array(aCoin), p.array(aCoin))
  .smap(function (x) { return new Coinbox(x[0], x[1]); },
        function (box) { return [box.inbox, box.vault]; },
        function (box) { return '' + box;} );

var inboxBalanceAlwaysPositive = p.forall(aCoinbox, function (coinbox) {
  return coinbox.moneyInserted() >= 0;
});

var vaultBalanceAlwaysPositive = p.forall(aCoinbox, function (coinbox) {
  return coinbox.balance() >= 0;
});

var sumOfVaultAndInboxRemainsSameAfterCheckout = p.forall(aCoinbox, function (coinbox) {
  var before = coinbox.total();
  newCoinbox = coinbox.checkout();
  var after = newCoinbox.total();
  return before === after;
});

var inboxClearedAfterCheckout = p.forall(aCoinbox, function (coinbox) {
  return coinbox.checkout().moneyInserted() === 0;
});

var insertingAddsToTotalAmount = p.forall(aCoin, aCoinbox, function (coinToInsert, coinbox) {
  var newCoinbox = coinbox.insert(coinToInsert);
  return coinbox.total() + coinToInsert.value === newCoinbox.total();
});

p.check(inboxBalanceAlwaysPositive, options);
p.check(vaultBalanceAlwaysPositive, options);
p.check(sumOfVaultAndInboxRemainsSameAfterCheckout, options);
p.check(inboxClearedAfterCheckout, options);
p.check(insertingAddsToTotalAmount, options);

