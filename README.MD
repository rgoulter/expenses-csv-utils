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

# Alternative Solutions

## ledger-cli's xact

https://www.ledger-cli.org/3.0/doc/ledger3.html#xact

> The xact command simplifies the creation of new transactions. It works on the
> principle that 80% of all postings are variants of earlier postings.

An example of invoking the `xact` command:

> $ ledger xact 2004/4/9 viva food 11 tips 2.50

As far as I can tell, `xact` offers a much more comprehensive
way to add a new transaction than this project.

# Implementation

Uses Haskell's MegaParsec to parse the input file.

# Building

## Running in a Cabal Sandbox

You can run the following commands to setup a sandbox, install this project's
dependencies (to the sandbox):

```
cabal sandbox init
cabal install --only-dependencies
cabal build
```

builds to `dist/build/expenses-csv-utils/expenses-csv-utils`.  
Alternatively (in the sandbox), `cabal exec -- runghc Main.hs <file1> <file2>`
to run main.
