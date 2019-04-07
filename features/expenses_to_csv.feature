Feature: Translate Expenses File to CSV Format
  Scenario: Translate a well-formatted expenses file
    Given an expenses file "expenses.txt"
      """
      2018-01-01 MON
      Spent 5 SGD on McDonalds
      """
     When I run the command "expenses-csv-utils" with "expenses.txt" and "output.csv"
     Then the standard output should be
       """
       """
      And the file "output.csv" should have content
       """
       "2018-01-01","5.0","SGD","on McDonalds"
       """

  Scenario: Outputs error for malformed file
    Given an expenses file "expenses.txt"
      # NOTE: currently, program can't handle typo on last line?!
      """
      2018-01-01 MON
      Sent 5 SGD on McDonalds
      
      """
     When I run the command "expenses-csv-utils" with "expenses.txt" and "output.csv"
     Then the standard output should be
       """
       offset=15:
       unexpected "Sent"
       expecting Date directive or Expense directive
       
       
       """
