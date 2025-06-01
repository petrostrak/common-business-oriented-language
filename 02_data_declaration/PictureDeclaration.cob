       IDENTIFICATION DIVISION. 
       PROGRAM-ID. PictureDeclaration.
       AUTHOR. Petros Trakadas.
       DATE-WRITTEN. 1st June 2025.
       DATA DIVISION. 
       WORKING-STORAGE SECTION. 
       01 Num1 PIC 9(3) VALUE ZEROS.
       01 Num2 PIC 9(3) VALUE 15.
       01 TaxRate PIC V9(2) VALUE .35.
       01 CustomerName PIC X(15) VALUE "Mike".
       PROCEDURE DIVISION.
       Begin.
           DISPLAY "Num1 is " Num1 
           DISPLAY "Num2 is " Num2 
           DISPLAY "Tax rate is " TaxRate 
           DISPLAY "Customer Name is " CustomerName 
           STOP RUN.
           