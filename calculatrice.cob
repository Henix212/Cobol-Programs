       IDENTIFICATION DIVISION.
       PROGRAM-ID. CALCULATRICE.
       DATA DIVISION.
           WORKING-STORAGE SECTION.
               77 a PIC 9999.
               77 b PIC 9999.
               77 op PIC XXX.
               77 result PIC S9999V99 VALUE 0.
       PROCEDURE DIVISION.
              DISPLAY "Please enter a value for a: ".
              ACCEPT a.
              DISPLAY "Please enter a value for b: ".
              ACCEPT b.
              DISPLAY "Please enter an operator (+, -, *, /): ".
              ACCEPT op.
              EVALUATE op
                WHEN "+"
                     ADD a TO b GIVING result
                WHEN "-"
                     SUBTRACT b FROM a GIVING result
                WHEN "*"
                     MULTIPLY a BY b GIVING result
                WHEN "/"
                     IF b = 0
                         DISPLAY "Division by zero is not allowed."
                     ELSE
                         DIVIDE a BY b GIVING result
                     END-IF
                WHEN "DIV"
                     IF b = 0
                         DISPLAY "Division by zero is not allowed."
                     ELSE
                         DIVIDE a BY b GIVING result
                     END-IF
                WHEN OTHER
                     DISPLAY "Invalid operator : " op
              END-EVALUATE.
              DISPLAY "Result = " result.
       STOP RUN.
