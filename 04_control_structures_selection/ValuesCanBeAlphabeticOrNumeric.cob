       IDENTIFICATION DIVISION. 
       PROGRAM-ID. ValuesCanBeAlphaOrNumeric.
       AUTHOR. Petros Trakadas.
       DATE-WRITTEN. 5th June 2025.

       DATA DIVISION. 
       WORKING-STORAGE SECTION. 
       01 InputCar PIC X.
           88 Vowel       VALUE "A", "E", "I", "O", "U".
           88 Consonant   VALUE "B" THROUGH "D", "F", "G", "H", 
                                "J" THROUGH "N", "P" THROUGH "T", 
                                "V" THROUGH "Z".
           88 Digit       VALUE "0" THROUGH "9".
           88 ValidChar   VALUE "A" THROUGH "Z", "0" THROUGH "9".

       PROCEDURE DIVISION.
       Begin.
           DISPLAY "Enter a character :- " WITH NO ADVANCING 
           ACCEPT InputCar 
           IF ValidChar 
              DISPLAY "Input OK"
           ELSE 
              DISPLAY "Invalid character entered"
           END-IF 
           IF Vowel 
              DISPLAY "Vowel entered"
           END-IF 
           IF Consonant 
              DISPLAY "Consonant entered"
           END-IF 
           IF Digit 
              DISPLAY "Digit entered"
           END-IF 
           STOP RUN.
           