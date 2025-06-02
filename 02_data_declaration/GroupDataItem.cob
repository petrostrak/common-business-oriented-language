       IDENTIFICATION DIVISION. 
       PROGRAM-ID. StudentRecord.
       AUTHOR. Petros Trakadas.
       DATE-WRITTEN. 2nd June 2025.
       DATA DIVISION. 
       WORKING-STORAGE SECTION. 
       01 StudentRec.
           02 StudentId PIC 9(7).
           02 StudentName.
              03 Forename PIC X(9).
              03 Surname PIC X(12).
           02 DateOfBirth.
              03 YOB PIC 9(4).
              03 MOB PIC 9(2).
              03 DOB PIC 9(2).
           02 CourseID PIC X(5).
           02 GPA PIC 9V9(2).
       PROCEDURE DIVISION.
       Begin.
           MOVE 1205621 TO StudentId 
           MOVE "Petros" TO Forename  
           MOVE "Trakadas" TO Surname
           MOVE 1986 TO YOB 
           MOVE 05 TO MOB 
           MOVE 10 TO DOB 
           MOVE "LM051" TO CourseID
           MOVE 2.55 TO GPA 
           DISPLAY StudentRec 
           DISPLAY "Student date of birth is " DOB "/" MOB "/" YOB
           DISPLAY "Student name = " Surname "," SPACE Forename
           STOP RUN.
