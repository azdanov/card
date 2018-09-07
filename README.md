# Haskell Card Validator

[![Build Status](https://travis-ci.com/azdanov/haskell-card-validator.svg?branch=master)](https://travis-ci.com/azdanov/haskell-card-validator)

Validate a card number. The algorithm follows these steps:

* Double the value of every second digit beginning with the rightmost.
* Add the digits of the doubled values and the undoubled digits from the original number.
* Calculate the modulus of the sum divided by 10.

If the result equals 0, then the number is valid.
