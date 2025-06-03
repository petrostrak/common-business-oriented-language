       IDENTIFICATION DIVISION. 
       PROGRAM-ID. Accept-Multiply-Display.
       AUTHOR. Petros Trakadas.
       DATE-WRITTEN. 3rd June 2025.
      *>   Accepts two numbers from the user, multiplies them together
      *>   and then displays the result.
       DATA DIVISION. 
       WORKING-STORAGE SECTION. 
       01 Num1 PIC 9.
       01 Num2 PIC 9.
       01 Result PIC 99.
       PROCEDURE DIVISION.
       Begin.
           DISPLAY "Enter a single digit number: " WITH NO ADVANCING 
           ACCEPT Num1
           DISPLAY "Enter a single digit number: " WITH NO ADVANCING
           ACCEPT Num2 
           MULTIPLY Num1 BY Num2 GIVING Result 
           DISPLAY "Result is = "Result 
           STOP RUN.
