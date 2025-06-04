       IDENTIFICATION DIVISION. 
       PROGRAM-ID. MultiConditionNames.
       AUTHOR. Petros Trakadas.
       DATE-WRITTEN. 4th June 2025.
       DATA DIVISION. 
       WORKING-STORAGE SECTION. 
       01 CityCode PIC 9 VALUE ZERO.
           88 CityIsAthens VALUE 1.
           88 CityIsPatra VALUE 2.
           88 CityIsTrikala VALUE 3.
           88 CityIsLarissa VALUE 4.
           88 CityIsSaloniki VALUE 5.
           88 CityIsKavala VALUE 6.

       PROCEDURE DIVISION.
       Begin.
           DISPLAY "Enter a city code (1-6) - " WITH NO ADVANCING 
           ACCEPT CityCode
           IF CityIsAthens THEN
               DISPLAY "Hey, we're in the capital."
           END-IF 
           IF CityIsPatra THEN
               DISPLAY "Hey, we're in Patra."
           END-IF 
           IF CityIsTrikala THEN
               DISPLAY "Hey, we're in Trikala."
           END-IF 
           IF CityIsLarissa THEN
               DISPLAY "Hey, we're in Larissa."
           END-IF 
           IF CityIsSaloniki THEN
               DISPLAY "Hey, we're in Saloniki."
           END-IF 
           IF CityIsKavala THEN
               DISPLAY "Hey, we're in Kavala."
           END-IF 
           STOP RUN.

