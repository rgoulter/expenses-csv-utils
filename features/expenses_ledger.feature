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
         Undescribed  5.00 SGD
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
         Undescribed  5.00 SGD
         Assets:Cash:SGD

       # 2018-01-03 Wednesday
       # Spent 40 SGD on ez-link top up
       2018-01-03 on ez-link top up
         Undescribed  40.00 SGD
         Assets:Cash:SGD

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
         Undescribed  2,000.00 SGD
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
