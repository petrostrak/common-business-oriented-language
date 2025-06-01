       IDENTIFICATION DIVISION. 
       PROGRAM-ID. MoveRulesExample.
       AUTHOR. Petros Trakadas.
       DATE-WRITTEN. 1st June 2025.
       DATA DIVISION. 
       WORKING-STORAGE SECTION. 
       01 Source1 PIC X(6) VALUE "ABCDEF".
       01 Dest1 PIC X(3) VALUE ZEROS.
       01 Source2 PIC X(3) VALUE "ABC".
       01 Dest2 PIC X(6) VALUE "DEFGHI".
       PROCEDURE DIVISION.
       Begin.
           DISPLAY "Source(1) is " Source1 
           DISPLAY "Destination(1) is " Dest1 
           MOVE Source1 TO Dest1 
           DISPLAY "Destination(1) after MOVE " Dest1 
           DISPLAY "Source(2) is " Source2 
           DISPLAY "Destination(2) is " Dest2 
           MOVE Source2 TO Dest2 
           DISPLAY "Destination(2) after MOVE " Dest2
           STOP RUN.
