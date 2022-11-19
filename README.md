# dater

A simple DSL for calculating dates, with a toplevel interpreter implemented in `Haskell`.

No, unfortunately, this is not about dating.

# The Language
## Formal grammar
```
S     ::= DATE | DATE NUMOP YMD | DATE DIFF DATE
NUMOP ::= + | -
DIFF  ::= <>
DATE  ::= YMD | TODAY
```

## Description
A `dater` program consists of just one command: adding dates or finding the difference two dates.
Each date consists of a year, month, and a day.

### The add operator

The add `+` operator takes a `y/m/d` date and a `y/m/d` date shift to produce a new date.
```
dater $> 2000/3/2 + 1/0/3
2001/3/5
```

### The subtraction operator

The add `-` operator takes a `y/m/d` date and a `y/m/d` date shift to produce a new date.
When `today` is 2022/11/19:
```
dater $> today - 0/22/33
2020/12/17
```

### The difference operator
The difference `<>` operator will find the distance between two dates.
```
dater $> 2021/11/12 <> 2022/1/1
0/1/20
```

#### Syntactic Sugar
Five syntactic sugar functions exist to make the `dater` easier to use.
```
* day   x  => 0 0 x
* week  y  => 0 0 7y
* month z  => 0 z 0
* year  w  => w 0 0
* today    => today's date
```

Example:
```
dater $> 2012/12/22 + week 10
2013/3/2
```

# Usage
The recommended way to use the language is with the toplevel interpreter in this repository.

## Installation
First, you will need [The Haskell Stack](https://docs.haskellstack.org/en/stable/README/).
Then, simply run
```
stack install
```
This will copy the `dater` binary to your path (`~/.local/bin/` on Linux systems).

## Running the toplevel
```
➜  ~ dater
Welcome to dater, date calculations done easy!
Example Usages:
dater $> today +  week 9
dater $> 2022/11/22 - 0/11/1
dater $> today <> 1997 / 4 / 10

dater $>
```

When you are done, exit with 
```
dater $>:q
Goodbye.
```

## Add-list

### More features!
* Use previous computation value (Python's `_` or ghci's `it`)
* Run files instead of running the REPL, e.g. `dater foo.dtr`
* Option to specify as days: `as_days today <> 1997/4/10`.
This could be a toggle option.

### Implementation details
* Better `mtl` instead of `IO (Either DateError a)`
* Migrate `Main.hs` out of `InputT`
