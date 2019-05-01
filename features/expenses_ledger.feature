Feature: Translate Expenses File to Ledger Format
  Scenario: Translate a well-formatted expenses file
    Given an expenses file "expenses.txt"
      """
      2018-01-01 MON
      Spent 5 SGD on McDonalds
      """
     When I run the command "expenses-utils ledger" with "expenses.txt" and "journal.ledger"
     Then the standard output should be
       """
       """
      And the file "journal.ledger" should have content
       """
       # 2018-01-01 Monday
       # Spent 5.0 SGD on McDonalds
       2018-01-01 on McDonalds
         Undescribed  5.0 SGD
         Assets:Cash:SGD

       """
