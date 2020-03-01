Feature: Check Expenses File Syntax
  Scenario: Check a well-formatted expenses file
    Given an expenses file "expenses.txt"
      """
      2018-01-01 MON
      Spent 5 SGD on McDonalds
      """
     When I run the command "expenses-utils check expenses.txt"
     Then the standard output should be
       """
       """

  Scenario: Outputs error for malformed file
    Given an expenses file "expenses.txt"
      """
      2018-01-01 MON
      Sent 5 SGD on McDonalds
      """
     When I run the command "expenses-utils check expenses.txt"
     Then the standard output should be
       """
       offset=15:
       unexpected "Sent"
       expecting Date directive or Expense directive
       
       
       """
