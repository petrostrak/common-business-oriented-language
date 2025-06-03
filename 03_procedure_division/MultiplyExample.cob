       IDENTIFICATION DIVISION. 
       PROGRAM-ID. MultiplyExample.
       AUTHOR. Petros Trakadas.
       DATE-WRITTEN. 3rd June 2025.
       DATA DIVISION. 
       WORKING-STORAGE SECTION. 
       01 Fees       PIC 9(3)V99 VALUE 052.24.
       01 Members    PIC 9(4)    VALUE 1024.
       01 TotalFees  PIC 9(5)V99 VALUE ZEROS.
       PROCEDURE DIVISION.
       Begin.
           MULTIPLY Fees BY Members GIVING TotalFees
           DISPLAY TotalFees 
           STOP RUN.
