       IDENTIFICATION DIVISION. 
       PROGRAM-ID. SingleConditionName.
       AUTHOR. Petros Trakadas.
       DATE-WRITTEN. 4th June 2025.
       DATA DIVISION. 
       WORKING-STORAGE SECTION. 
       01 CityCode PIC 9 VALUE ZERO.
           88 CityIsAthens VALUE 2.

       PROCEDURE DIVISION.
       Begin.
           DISPLAY "Enter a city code (1-6) - " WITH NO ADVANCING 
           ACCEPT CityCode 

           IF CityIsAthens THEN
              DISPLAY "Hey, you're home"
           END-IF 
           STOP RUN.
           
           