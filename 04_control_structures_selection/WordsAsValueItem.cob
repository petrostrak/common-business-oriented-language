       IDENTIFICATION DIVISION. 
       PROGRAM-ID. WordsAsValueItem.
       AUTHOR. Petros Trakadas.
       DATE-WRITTEN. 5th June 2025.

       DATA DIVISION. 
       WORKING-STORAGE SECTION. 
       01 MakeOfCar PIC X(10).
           88 VWGroup VALUE "skoda", "seat", "audi", "vw".
           88 GermanMade VALUE "vw", "audi", "mercedes", "bmw", 
                        "porsche".

       PROCEDURE DIVISION.
       Begin.
           DISPLAY "Enter the make of car - " WITH NO ADVANCING
           ACCEPT MakeOfCar
           IF VWGroup AND GermanMade
              DISPLAY "Your car is made in Germany by the " 
                       "Volkswagen Group."
           ELSE
              IF VWGroup
                 DISPLAY "Your car is made by the Volkswagen Group."
              END-IF
             IF GermanMade
                DISPLAY "Your car is made in Germany."
             END-IF
           END-IF
           STOP RUN.           