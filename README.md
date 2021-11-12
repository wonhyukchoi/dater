# dater

A simple DSL for calculating dates, with a toplevel interpreter implemented in `Haskell`.

No, unfortunately, this is not about dating.

# The Language
## Formal grammar
```
S     ::= DATE OP DATE
OP    ::= + | -
DATE  ::= (YEAR, MONTH, DAY)
```
## Description
A `dater` program consists of just one command: adding or subtracting of two dates.
Each date consists of a year, month, and a day.
These do not necessarily need to be actual dates, however; you can, for instance, add a week:
```
dater$> (2021, 11, 12) + (0,1,0)
(2021, 11, 19)
```
## Syntactic Sugar
Four syntactic sugar functions exist to make `dater` easier to use.
```
* day(x)  => (0,0,x)
* week(y) => (0,y,0)
* year(z) => (z,0,0)
* today   => Date object corresponding to today's date
```

Let today be November 12th, 2021.
Then, in action, we have:
```
dater$> today + day(5)
(2021, 11, 17)
```

# Usage
The recommended way to use the language is with the toplevel interpreter in this repository.

## Installation
First, you will need [The Haskell Stack](https://docs.haskellstack.org/en/stable/README/).
Then, simply run
```
stack install
```
This will copy the `dater` binary to `~/.local/bin/` on Linux systems.

## Running the toplevel
```
~$ dater
Welcome to dater, date calculations done easy!
dater$>
```
Then you are good to go!
