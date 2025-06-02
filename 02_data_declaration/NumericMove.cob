       IDENTIFICATION DIVISION. 
       PROGRAM-ID. NumericMove.
       AUTHOR. Petros Trakadas.
       DATE-WRITTEN. 2nd June 2025.
       DATA DIVISION. 
       WORKING-STORAGE SECTION. 
       01 NumOfEmployees PIC 999V.
       01 Salary PIC 9(4)V9(2).
       01 CountyName PIC X(9).
       PROCEDURE DIVISION.
       Begin.
           MOVE 12.4 TO NumOfEmployees 
           DISPLAY "12.4 moved to NumOfEmployees " NumOfEmployees
           MOVE 6745 TO NumOfEmployees 
           DISPLAY "6745 moved to NumOfEmployees " NumOfEmployees
           MOVE NumOfEmployees TO Salary 
           DISPLAY "NumOfEmployees moved to Salary " Salary 
           MOVE "PETROS" to CountyName 
           DISPLAY "PETROS moved to CountyName " CountyName 
           MOVE ALL "@" to CountyName 
           DISPLAY "moved all '@' to CountyName " CountyName 
           STOP RUN.
           
