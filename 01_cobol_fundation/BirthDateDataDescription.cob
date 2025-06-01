       IDENTIFICATION DIVISION. 
       PROGRAM-ID. BirthDate.
       DATA DIVISION. 
       WORKING-STORAGE SECTION. 
       01 BirthDate.
           02 YearOfBirth.
              03 CenturyOB PIC 99.
              03 YearOB PIC 99.
           02 MonthOfBirth PIC 99.
           02 DayOfBirth PIC 99.
       PROCEDURE DIVISION.
       DisplayBirthDate.
           MOVE 19860510 TO BirthDate 
           DISPLAY "Month is " MonthOfBirth 
           DISPLAY "Century of birth is " CenturyOB 
           DISPLAY "Year of birth is " YearOfBirth 
           DISPLAY DayOfBirth "/" MonthOfBirth "/" YearOfBirth 
           MOVE ZEROS TO YearOfBirth 
           DISPLAY "Birth date is " BirthDate.
           STOP RUN.
