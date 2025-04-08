IDENTIFICATION DIVISION.
       PROGRAM-ID. CALCULATRICE.

       DATA DIVISION.
           WORKING-STORAGE SECTION.
               77 num1 PIC 9999 VALUE 0.           *> First value
               77 num2 PIC 9999 VALUE 0.           *> Second value
               77 operator PIC X VALUE SPACE.      *> Operator (+, -, *, /)
               77 result PIC S9999V99 VALUE 0.     *> Result
               77 valid-input PIC X VALUE "N".     *> Input validity indicator

       PROCEDURE DIVISION.
           DISPLAY "=== Simple Calculator ===".

           *> Entering the first value
           PERFORM UNTIL valid-input = "Y"
               DISPLAY "Please enter a value for num1: "
               ACCEPT num1
               IF num1 NUMERIC
                   MOVE "Y" TO valid-input
               ELSE
                   DISPLAY "Invalid input. Please enter a numeric value."
               END-IF
           END-PERFORM.

           *> Reset the indicator
           MOVE "N" TO valid-input.

           *> Entering the second value
           PERFORM UNTIL valid-input = "Y"
               DISPLAY "Please enter a value for num2: "
               ACCEPT num2
               IF num2 NUMERIC
                   MOVE "Y" TO valid-input
               ELSE
                   DISPLAY "Invalid input. Please enter a numeric value."
               END-IF
           END-PERFORM.

           *> Choose an operator
           DISPLAY "Please enter an operator (+, -, *, /): "
           ACCEPT operator

           *> Calculation based on the operator
           EVALUATE operator
               WHEN "+"
                   ADD num1 TO num2 GIVING result
               WHEN "-"
                   SUBTRACT num2 FROM num1 GIVING result
               WHEN "*"
                   MULTIPLY num1 BY num2 GIVING result
               WHEN "/"
                   IF num2 = 0
                       DISPLAY "Error: Division by zero is not allowed."
                   ELSE
                       DIVIDE num1 BY num2 GIVING result
                   END-IF
               WHEN OTHER
                   DISPLAY "Invalid operator: " operator
           END-EVALUATE.

           *> Display the result if the operator is valid
           IF operator = "+" OR operator = "-" OR operator = "*" OR (operator = "/" AND num2 NOT = 0)
               DISPLAY "Result = " result
           END-IF.

           STOP RUN.
