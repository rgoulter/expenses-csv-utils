Feature: Translate Expenses File to Ledger Format
  Scenario: Translate a well-formatted expenses file
    Given an expenses file "expenses.txt"
      """
      2018-01-01 MON
      Spent 5 SGD on McDonalds
      """
     When I run the command "expenses-utils ledger --no-accounts expenses.txt journal.ledger"
     Then the standard output should be
       """
       """
      And the file "journal.ledger" should have content
       """
       # 2018-01-01 Monday
       # Spent 5 SGD on McDonalds
       2018-01-01 on McDonalds
         Undescribed  5 SGD
         Assets:Cash:SGD

       """

  Scenario: Translate a well-formatted expenses file with multiple transactions
    Given an expenses file "expenses.txt"
      """
      2018-01-01 MON
      Spent 5 SGD on McDonalds

      WED
      Spent 40 SGD on ez-link top up
      """
     When I run the command "expenses-utils ledger --no-accounts expenses.txt journal.ledger"
     Then the standard output should be
       """
       """
      And the file "journal.ledger" should have content
       """
       # 2018-01-01 Monday
       # Spent 5 SGD on McDonalds
       2018-01-01 on McDonalds
         Undescribed  5 SGD
         Assets:Cash:SGD

       # 2018-01-03 Wednesday
       # Spent 40 SGD on ez-link top up
       2018-01-03 on ez-link top up
         Undescribed  40 SGD
         Assets:Cash:SGD

       """

  Scenario: Translate a well-formatted expenses file using different default currency
    Given an expenses file "expenses.txt"
      """
      2018-01-01 MON
      Spent 5 on McDonalds

      Using VND as the default currency

      WED
      Spent 10k on snacks
      """
     When I run the command "expenses-utils ledger --no-accounts expenses.txt journal.ledger"
     Then the standard output should be
       """
       """
      And the file "journal.ledger" should have content
       """
       # 2018-01-01 Monday
       # Spent 5 SGD on McDonalds
       2018-01-01 on McDonalds
         Undescribed  5 SGD
         Assets:Cash:SGD

       # 2018-01-03 Wednesday
       # Spent 10k VND on snacks
       2018-01-03 on snacks
         Undescribed  10,000 VND
         Assets:Cash:VND

       """

  Scenario: Output the numbers in a human-readable format
    Given an expenses file "expenses.txt"
      """
      2018-01-01 MON
      Spent 2,000 SGD on new computer
      """
     When I run the command "expenses-utils ledger --no-accounts expenses.txt journal.ledger"
     Then the standard output should be
       """
       """
      And the file "journal.ledger" should have content
       """
       # 2018-01-01 Monday
       # Spent 2k SGD on new computer
       2018-01-01 on new computer
         Undescribed  2,000 SGD
         Assets:Cash:SGD

       """

  Scenario: Preserves cents in expenses file
    Given an expenses file "expenses.txt"
      """
      2018-01-01 MON
      Spent 1.5 on ice cream
      """
     When I run the command "expenses-utils ledger --no-accounts expenses.txt journal.ledger"
     Then the standard output should be
       """
       """
      And the file "journal.ledger" should have content
       """
       # 2018-01-01 Monday
       # Spent 1.5 SGD on ice cream
       2018-01-01 on ice cream
         Undescribed  1.50 SGD
         Assets:Cash:SGD

       """

  Scenario: Prompts for input if given no ledger journals
    Given an expenses file "expenses.txt"
      """
      2018-01-01 MON
      Spent 5 on McDonalds
      """
     When I run the command "expenses-utils ledger expenses.txt journal.ledger"
     Then the standard output should be
       """
       2018-01-01 Monday
       Spent 5 SGD on McDonalds

       Debitted account:
       """

  Scenario: Uses input from prompt for output ledger file
    Given an expenses file "expenses.txt"
      """
      2018-01-01 MON
      Spent 5 on McDonalds
      """
     When I run the command "expenses-utils ledger expenses.txt journal.ledger"
      And input "Expenses:Food"
     Then the file "journal.ledger" should have content
       """
       # 2018-01-01 Monday
       # Spent 5 SGD on McDonalds
       2018-01-01 on McDonalds
         Expenses:Food  5 SGD
         Assets:Cash:SGD

       """

  Scenario: Account used from journal data in case of exact unambiguous match
    Given an expenses file "expenses.txt"
      """
      2018-01-01 MON
      Spent 5 on McDonalds
      """
      And a ledger file "ledger.dat"
      """
      2017-12-01 on McDonalds
        Expenses:Food  5 SGD
        Assets:Cash:SGD
      """
     When I run the command "expenses-utils ledger -j ledger.dat expenses.txt journal.ledger"
     Then the standard output should be
       """
       """
      And the file "journal.ledger" should have content
       """
       # 2018-01-01 Monday
       # Spent 5 SGD on McDonalds
       2018-01-01 on McDonalds
         Expenses:Food  5 SGD
         Assets:Cash:SGD

       """

  Scenario: Prompts for input in case of ambiguous match
    Given an expenses file "expenses.txt"
      """
      2018-01-01 MON
      Spent 5 on McDonalds
      """
      And a ledger file "ledger.dat"
      """
      2017-12-01 on McDonalds
        Expenses:Food  5 SGD
        Assets:Cash:SGD

      2017-12-02 on McDonalds
        Expenses:Dessert  1.50 SGD
        Assets:Cash:SGD
      """
     When I run the command "expenses-utils ledger -j ledger.dat expenses.txt journal.ledger"
     Then the standard output should be
       """
       2018-01-01 Monday
       Spent 5 SGD on McDonalds

       Suggested Accounts:
        (1) Expenses:Dessert
        (2) Expenses:Food
       Debitted account:
       """

  Scenario: Uses suggested input number from prompt for output ledger file
    Given an expenses file "expenses.txt"
      """
      2018-01-01 MON
      Spent 5 on McDonalds
      """
      And a ledger file "ledger.dat"
      """
      2017-12-01 on McDonalds
        Expenses:Food  5 SGD
        Assets:Cash:SGD

      2017-12-02 on McDonalds
        Expenses:Dessert  1.50 SGD
        Assets:Cash:SGD
      """
     When I run the command "expenses-utils ledger -j ledger.dat expenses.txt journal.ledger"
      And input "2"
     Then the file "journal.ledger" should have content
       """
       # 2018-01-01 Monday
       # Spent 5 SGD on McDonalds
       2018-01-01 on McDonalds
         Expenses:Food  5 SGD
         Assets:Cash:SGD

       """
