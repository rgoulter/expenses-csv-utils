I use this program to convert from a simple daybook format such as

```
2018-01-01 MON
Spent 10 SGD on groceries
Spent 5 SGD on stationery
```

and converts this to the ledger-cli journal format:

```
# 2018-01-01 Monday
# Spent 10 SGD on groceries
2018-01-01 on groceries
  Undescribed  10.00 SGD
  Assets:Cash:SGD

# Spent 5 SGD on stationery
2018-01-01 on stationery
  Undescribed  5.00 SGD
  Assets:Cash:SGD
```


# Motivation

For personal accounting in plain text,
tools like ledger-cli are excellent.

My personal accounting workflow currently involves
an intermediate format for recording transactions,
and then translating this to a ledger-cli journal.

e.g. I prefer to initially describe transactions like:

```
2016-01-01 MON
Spent 100 on groceries
Spent 30 on games

TUE
Spent 2 on breakfast
```

# Prerequisites

- Use haskell stack to manage installing haskell for this project.

- To run the executable specifications under `features/`,
  install ruby and gems `cucumber`, `rspec`.

# Build

## Haskell Stack

To build the program, run:

```
stack build
```

# Test

To run the in-process unit and integration tests, run:

```
stack test
```

To run the out-of-process end to end tests, run:

```
cucumber
```

# Alternative Solutions

## ledger-cli's xact

https://www.ledger-cli.org/3.0/doc/ledger3.html#xact

> The xact command simplifies the creation of new transactions. It works on the
> principle that 80% of all postings are variants of earlier postings.

An example of invoking the `xact` command:

> $ ledger xact 2004/4/9 viva food 11 tips 2.50

As far as I can tell, `xact` offers a much more comprehensive
way to add a new transaction than this project.
