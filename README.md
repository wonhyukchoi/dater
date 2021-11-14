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
A `dater` program consists of just one command: adding dates or finding the difference two dates.
Each date consists of a year, month, and a day.

The `+` operator will add the two dates:
```
dater$> (2000, 3, 2) + week(1)
2000/3/9
```

The `-` operator will find the distance between two dates.
```
dater$> (2021, 11, 12) - (2022, 1, 1)
50
```


## Syntactic Sugar
Five syntactic sugar functions exist to make `dater` easier to use.
```
* day(x)   => (0,0,x)
* week(y)  => (0,0,7y)
* month(z) => (z,0,0)
* year(w)  => (w,0,0)
* today    => Date object corresponding to today's date
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

#### Acknowledgements
The `Parsec` portions of the lexer/parser were adapted from [Stephen Diehl](https://www.stephendiehl.com/llvm/#full-source-1).
