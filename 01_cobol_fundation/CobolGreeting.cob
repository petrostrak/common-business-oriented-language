       IDENTIFICATION DIVISION. 
       PROGRAM-ID. CobolGreeting.
      *Program to display COBOL greetings
       DATA DIVISION. 
       WORKING-STORAGE SECTION. 
       01 IterNum  PIC 9 VALUE 5.

       PROCEDURE DIVISION.
       DisplayGreeting.  
           DISPLAY "Greetings from COBOL".

       BeginProgram.
           PERFORM DisplayGreeting IterNum TIMES.
           STOP RUN.
           