Translates from some "expenses file" format, to CSV file.  
Uses Haskell's MegaParsec to parse the input file.

The CSV it outputs is of form:

```
date, amount, currency, remark, category
```

## "Expenses" File

I've found apps on my phone where I enter in what expenses I've made
to be too cumbersome. But I want to be aware of how I'm spending my money.

So I've kept track of it in a textfile something like:

```
2016-01-01 MON
Spent 100 on groceries
Spent 30 on games (BoneStorm)

TUE
Spent 1 on crabby patty from Krusty Krabs
```

Where:

* I don't worry about categorisation when recording this.  
    (Intention is to categorise it at a later stage,
     hopefully with some kind of automation/assistance).

* Use a natural-language-ish format, rather than CSV.  
    (So it's easier to read, write).

* Since I spent money at least two days a week, it's easier
  (for recording) to only explicity date the Monday,
  and distinguish other days spending by name-of-day.

* The 'remark' is natural language (ish); although I prefer to use
  phrases like "on X at Y", "on X (a, b, c)", etc.
