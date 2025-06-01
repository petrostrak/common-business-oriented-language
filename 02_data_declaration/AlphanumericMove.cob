       IDENTIFICATION DIVISION. 
       PROGRAM-ID. AlphanumericMove.
       AUTHOR. Petros Trakadas.
       DATE-WRITTEN. 1st June 2025.
       DATA DIVISION. 
       WORKING-STORAGE SECTION. 
       01 Surname PIC X(8) VALUE "TRAKADAS".
       PROCEDURE DIVISION.
       Begin.
           DISPLAY "Surname is " Surname 
           MOVE "GIOTIS" TO Surname  
           DISPLAY "Surname is now " Surname 
           MOVE "PAPADOPOULOS" TO Surname 
           DISPLAY "Surname is now " Surname
           STOP RUN.
