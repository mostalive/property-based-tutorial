var p = require("jsverify");
var _ = require("underscore");

var options = {
  tests: 5000,
  quiet: false 
};

var incompatibleCurrencies = {
  name: 'cannot add money from different currencies',
  toString: function () { return this.name; }
}

function Money(amount, currency) {
  this.amount = Math.abs(Math.round(amount));
  this.currency = currency;
}

Money.prototype.add = function (money) {
  if (this.currency !== money.currency) {
    throw incompatibleCurrencies;
  }
  return new Money(this.amount + money.amount, this.currency);
};

Money.prototype.create = function (amount) { return new Money(amount, this.currency); };

Money.prototype.allocate = function (shares) {
  var myShare = Math.floor(this.amount/shares);
  var allocations = [];
  for (i=0; i < shares-1; i++) {
    allocations.push(this.create(myShare));
  }
  allocations.push(this.create(this.amount-((shares-1)*myShare)));
  return allocations;
};

Money.prototype.toString = function () {
  return this.currency + ' ' + this.amount;
}

function moneyFromString(input) {


  var theMoney = new Money(parseInt(input.substr(3)), input.substr(0, 3));
  return { money: theMoney };
}

function isValid(money) {
  return possibleCurrencies.indexOf(money.currency) !== -1 &&
    money.amount % 1 === 0;
}

var possibleCurrencies = ["EUR", "USD"];

var currencies = p.elements(possibleCurrencies);
var amount = p.pair(p.nat(), currencies).smap(function (x) { return  new Money(x[0], x[1]); }, function (money) { return [money.amount, money.currency]; } );


var moneyOfSameCurrencyAddsUp = p.forall(amount, amount, function(a1, a2) {
  if (a1.currency !== a2.currency) {
    return true;
  }
  return a1.add(a2).amount === a1.amount + a2.amount;
});

var canNeverAddDifferentCurrencies = p.forall(amount, amount, function(a1, a2) {
  if (a1.currency === a2.currency) {
    return true;
  }

  try {
    a1.add(a2);
    return false;
  } catch (expected) {
    return true;
  }
});

var shares = p.suchthat(p.nat, function (n) { return n > 2; });

var allocatingDoesNotMakeMoneyDisappear = p.forall(amount, shares, function(money, share) {
  if (money.amount < 3) {
    return true;
  }
  var allocations = money.allocate(share);
  var restored = allocations.reduce(function (sum, current, index, array) {
    return sum.add(current);
  });
  return restored.amount === money.amount;
});

var anyInput = p.asciistring;

var parsingFromStringIsRobust = p.forall(anyInput, function(input) {
  var result = moneyFromString(input);
  if (result.hasOwnProperty('money')) {
    return isValid(result.money);
  } else if (result.message === 'invalid input') {
    return true;
  } else {
    return false;
  }
});

var validStringInput = p.pair(p.nat(), p.oneof(currencies, p.asciistring)).smap(function (x) { return [x[1] + ' ' + x[0], x[0], x[1]]; }, function (bla) { return [bla[1], bla[2]]; } );

var parsingValidStringResultsInValidMoney = p.forall(validStringInput, function(input) {
  var result = moneyFromString(input[0]);
  return (result.money.currency === input[2] && result.money.amount === input[1]); 
});

//p.check(moneyOfSameCurrencyAddsUp, options);
//p.check(canNeverAddDifferentCurrencies, options);
//p.check(allocatingDoesNotMakeMoneyDisappear, options);
//p.check(parsingFromStringIsRobust, options);
//p.check(parsingValidStringResultsInValidMoney, options);

describe('A proper menu', function () {
  it('has its items sorted by value', function () {
    p.assert(parsingValidStringResultsInValidMoney);
  });
});

