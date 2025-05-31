# Common Business-oriented Language
A repo deticated for learing COBOL.

## Structure of COBOL Programs
A COBOL program is divided into distinct parts called divisions. A division may contain one or more sections.
A section may contain one or more paragraphs. A paragraph may contain one or more sentences, and a sentence one or more statements.
```
PROGRAM
    DIVISION (s)
        SECTION (s)
            Paragraphs (s)
                Sentences (s)
                    Statement (s)
```
### Divisions
The division is the major structural element in COBOL. There are four divisions: the `IDENTIFICATION DIVISION`, the `ENVIRONMENT DIVISION`, the `DATA DIVISION`, and the `PROCEDURE DIVISION`.

### Sections 
A section is made up of one or more paragraphs. A section begins with the section name and ends where the next section name is encountered or where the program text ends.
```
    SelectTexasRecords SECTION.
    FILE SECTION.
    CONFIGURATION SECTION.
    INPUT-OUTPUT SECTION.
```

In the first 3 divsions (`IDENTIFICATION DIVISION`, `ENVIRONMENT DIVISION` and `DATA DIVISION`), sections are an organizational structure defined by the language. But in the `PROCEDURE DIVISION`, where you write the program's executable statements, sections and paragraphs are used to identify blocks of code that can be executed using the `PERFORM` or the `GO TO`.

### Paragraphs
A paragraph consists of one or more sentences. A paragraph begins with a paragraph name and ends where the next section name or paragraph name is encountered or where the program text ends. In the first 3 divsions (`IDENTIFICATION DIVISION`, `ENVIRONMENT DIVISION` and `DATA DIVISION`), paragraphs are an organizational structure defined by the language. But in the `PROCEDURE DIVISION`, paragraphs are used to identify blocks of code that can be executed using the `PERFORM` or the `GO TO`.

ENVIRONMENT DIVISION Entries Required for a File Declaration:
```
ENVIRONMENT DIVISION.
INPUT-OUTPUT SECTION.
FILE-CONTROL.
    SELECT ExampleFile ASSIGN TO "Example.Dat"
           ORGANIZATION IS SEQUENTIAL.
```

PROCEDURE DIVISION with Two Paragraphs (Begin and DisplayGreeting):
```
PROCEDURE DIVISION.
Begin.
   PERFORM  DisplayGreeting 10 TIMES.
   STOP RUN.
DisplayGreeting.
   DISPLAY "Greetings from COBOL".
```

### Sentences
A sentence consists of one of more statements and is terminated by a period. There must be at least one sentence, and hence one period in a paragraph.

```
SUBTRACT Tax FROM GrossPay GIVING NetPay.   // This sentence happens to be also a statement.

MOVE .21 TO VatRate
COMPUTE VatAmount = ProductCost * VatRate
DISPLAY "The VAT amount is - " VatAmount.   // This sentece consists of 3 statements.
```

### Statements
In COBOL, language statements are referred to as verbs. A statement starts with the name of the verb and is followed by the operand or operands on which the verb acts.

```
DISPLAY "Enter name " WITH NO ADVANCING
ACCEPT  StudentName
DISPLAY "Name entered was " StudentName
```

## Major COBOL verbs and cateforization by type
| Arithmetic  | File Handling | Flow of Control | Assignment & I-O | Table Handling | String Handling |
|------------|:-------------:|----------------|-----------------|---------------|----------------|
| COMPUTE    | OPEN          | IF             | MOVE            | SEARCH        | INSPECT        |
| ADD        | CLOSE         | EVALUATE       | SET             | SEARCH ALL    | STRING         |
| SUBTRACT   | READ          | PERFORM        | INITIALIZE      | SET           | UNSTRING       |
| MULTIPLY   | WRITE         | GO TO          | ACCEPT          |               |                |
| DIVIDE     | DELETE        | CALL           | DISPLAY         |               |                |
|            | REWRITE       | STOP RUN       |                 |               |                |
|            | START         | EXIT PROGRAM   |                 |               |                |
|            | SORT          |                |                 |               |                |
|            | RETURN        |                |                 |               |                |
|            | RELEASE       |                |                 |               |                |