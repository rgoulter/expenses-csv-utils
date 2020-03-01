Feature: Query Attributes of Expenses File Syntax
  Scenario: Print the earliest date of a simple file
    Given an expenses file "expenses.txt"
      """
      2018-01-01 MON
      Spent 5 SGD on McDonalds
      """
     When I run the command "expenses-utils query earliest expenses.txt"
     Then the standard output should be
       """
       2018-01-01

       """

  Scenario: Print the latest date of a simple file
    Given an expenses file "expenses.txt"
      """
      2018-01-01 MON
      Spent 5 SGD on McDonalds

      2018-06-30
      Spent 5 SGD on McDonalds
      """
     When I run the command "expenses-utils query latest expenses.txt"
     Then the standard output should be
       """
       2018-06-30

       """

  Scenario: Print the latest date with a day-of-week directive
    Given an expenses file "expenses.txt"
      """
      2018-01-01 MON
      Spent 5 SGD on McDonalds

      THU
      Spent 5 SGD on McDonalds
      """
     When I run the command "expenses-utils query latest expenses.txt"
     Then the standard output should be
       """
       2018-01-04

       """
