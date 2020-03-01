Feature: Translate Expenses File to CSV Format
  Scenario: Translate a well-formatted expenses file
    Given an expenses file "expenses.txt"
      """
      2018-01-01 MON
      Spent 5 SGD on McDonalds
      """
     When I run the command "expenses-utils csv expenses.txt output.csv"
     Then the standard output should be
       """
       """
      And the file "output.csv" should have content
       """
       "2018-01-01","5","SGD","on McDonalds"
       """

  Scenario: Outputs error for malformed file
    Given an expenses file "expenses.txt"
      """
      2018-01-01 MON
      Sent 5 SGD on McDonalds
      """
     When I run the command "expenses-utils csv expenses.txt output.csv"
     Then the standard output should be
       """
       offset=15:
       unexpected "Sent"
       expecting Date directive or Expense directive
       
       
       """
