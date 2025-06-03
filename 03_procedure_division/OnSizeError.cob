       IDENTIFICATION DIVISION. 
       PROGRAM-ID. OnSizeError.
       AUTHOR. Petros Trakadas.
       DATE-WRITTEN 3rd June 2025.
       DATA DIVISION. 
       WORKING-STORAGE SECTION. 
       01 Num1 PIC 9.
       01 Num2 PIC 9.
       01 Num3 PIC 9.
       01 Num4 PIC 9.
       01 FinalResult PIC 9.
       PROCEDURE DIVISION.
       Begin.
           MOVE 2 TO Num1, Num2, Num3, Num4
           COMPUTE FinalResult = Num1 * Num2 * Num3 * Num4 
           ON SIZE ERROR DISPLAY "Alert: FinalResult too small to hold "
                                                              "result" 
           END-COMPUTE
           STOP RUN.

