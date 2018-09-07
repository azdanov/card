# Haskell Card Validator

[![Build Status](https://travis-ci.com/azdanov/haskell-card-validator.svg?branch=master)](https://travis-ci.com/azdanov/haskell-card-validator)

Validate a card number. The algorithm follows these steps:

* Double the value of every second digit beginning with the rightmost.
* Add the digits of the doubled values and the undoubled digits from the original number.
* Calculate the modulus of the sum divided by 10.

If the result equals 0, then the number is valid.

## Usage

This project uses `stack`. You can read a [Quick Start Guide](https://docs.haskellstack.org/en/stable/README/#quick-start-guide) to begin.

```sh
stack exec card-exe

Enter a Card Number:
4556945538735694
Number 4556945538735694 is invalid.


stack exec card-exe

Enter a Card Number:
5256283618614517
Number 5256283618614517 is valid.
```

## Testing

HSpec is used for testing. To run the testing suite execute:

```sh
stack test
```