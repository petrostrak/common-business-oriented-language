# Common Business Oriented Language
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

## The 4 Divisions
At the top of the COBOL hierarchy are the four divisions. These divide the program into distinct structural elements. Although some of the divisions may be omitted, the sequence in which they are specified is fixed and must be as follows. Just like section names and paragraph names, division names must be followed by a period.

```
IDENTIFICATION DIVISION.    // Contains information about the program
ENVIRONMENT DIVISION.       // Contains environment information
DATA DIVISION.              // Contains data descriptions
PROCEDURE DIVISION.         // Contains the program algorithms
```

### IDENTIFICATION DIVISION
The purpose of the IDENTIFICATION DIVISION is to provide information about the program to you, the compiler, and the linker. The `PROGRAM-ID` paragraph is the only entry required. Other paragraphs may be `AUTHOR` and `DATE-WRITTEN`.

```
PROGRAM-ID. UserAssignedProgramName.
```

When a number of independently compiled programs are combined by the linker into a single executable run- unit, each program is identified by the name given in its PROGRAM-ID. When control is passed to a particular program by means of a CALL verb, the target of the CALL invocation is the name given in the subprogram’s PROGRAM-ID

```
CALL "PrintSummaryReport".
```
```
IDENTIFICATION DIVISION. 
PROGRAM-ID. PrintSummaryReport. 
AUTHOR. Petros Trakadas. 
DATE-WRITTEN. 31st May 2025.
```

### ENVIRONMENT DIVISION
The ENVIRONMENT DIVISION is used to describe the environment in which the program works. The idea is
to make it easy to change the program when it has to run on a different computer or one with different peripheral devices or when the program is being used in a different country.

The ENVIRONMENT DIVISION consists of two sections
* CONFIGURATION SECTION

    The SPECIAL-NAMES paragraph allows you to specify such environmental details as what alphabet to use, what currency symbol to use and what decimal point symbol to use.
* INPUT-OUTPUT SECTION

    The FILE-CONTROL paragraph lets you connect internal file names with external devices and files.

```
IDENTIFICATION DIVISION.
PROGRAM-ID. ConfigurationSectionExamples.
AUTHOR. Petros Trakadas. 
DATE-WRITTEN. 31st May 2025.
ENVIRONMENT DIVISION.
CONFIGURATION SECTION.
SPECIAL-NAMES.
    DECIMAL-POINT IS COMMA.
    SYMBOLIC CHARACTERS ESC CR LF // Lets you assign a name to one of the unprintable characters.
                    ARE 28 14 11.
INPUT-OUTPUT SECTION.
FILE-CONTROL.
    SELECT StockFile ASSIGN TO "D:\DataFiles\Stock.dat"
           ORGANIZATION IS SEQUENTIAL.
```

### DATA DIVISION
The DATA DIVISION is used to describe most of the data that a program processes. The DATA DIVISION is divided into four sections:

*   The `FILE SECTION` describes the data that is sent to, or comes from, the computer’s data storage peripherals. These include such devices as card readers, magnetic tape drives, hard disks, CDs, and DVDs.
*   The `WORKING-STORAGE SECTION` describes the general variables used in the program.
*   The `LINKAGE SECTION` is used only in subprograms.
*   The `REPORT SECTION` is used only when generating reports.

```
IDENTIFICATION DIVISION.
PROGRAM-ID.  SimpleDataDeclarations.
AUTHOR.  Petros Trakadas.
DATE-WRITTEN. 31st May 2025.
DATA DIVISION.
WORKING-STORAGE SECTION.
01  CardinalNumber      PIC 99      VALUE ZEROS.
01  IntegerNumer        PIC S99     VALUE -14.
01  DecimalNumber       PIC 999V99  VALUE 543.21.
01  ShopName            PIC X(30)   VALUE SPACES.
01  ReportHeading       PIC X(25)   VALUE "=== Employment Report ==="
```

## Data Hierarchy
Even though data hierarchy is too complicated topic to deal with so soon, a preview of it is given in BirthDate below:
```
01  BirthDate.
    02  YearOfBirth.
        03 CenturyOB    PIC 99
        03 YearOB       PIC 99
    02  MonthOfBirth    PIC 99
    02  DayOfBirth      PIC 99
```
```
|                 BirthDate                     |
|      YearOfBirth    |             |           |
| CenturyOB |  YearOB |MonthOfBirth |DayOfBirth |
| 1   | 9   | 8  | 6  |   1  |   0  |  0  |  5  |
```

### PROCEDURE DIVISION
The PROCEDURE DIVISION is where all the data described in the DATA DIVISION is processed and produced. It is here that you describe your algorithm. The PROCEDURE DIVISION is hierarchical in structure. It consists of sections, paragraphs, sentences, and statements. Tshere must be at least one paragraph, one sentence, and one statement in the PROCEDURE DIVISION.

Whereas the paragraph and section names in the other divisions are defined by the language, in the PROCEDURE DIVISION they are chosen by you. The names chosen should reflect the function of the code contained in the paragraph or section.

#### Shortest COBOL Program
```
IDENTIFICATION DIVISION.
PROGRAM-ID. ShortestProgram.
PROCEDURE DIVISION.
DisplayPrompt.
     DISPLAY "I did it".
```

## COBOL Coding Rules
Traditionally, COBOL programs were written on coding sheets, punched on to punch cards, and then loaded into the computer via a card reader. 

*   On the coding sheet, the first six character positions are reserved for sequence numbers.
*   The seventh character position is reserved for the continuation character or for an asterisk that denotes a comment line. 
*   The actual program text starts in column 8. The four positions from 8 to 11 are known as Area
A, and the positions from 12 to 72 are called Area B.
*   The area from position 73 to 80 is the identification area; it was generally used to identify the program. 

> [!IMPORTANT]  
> All division names, section names, paragraph names, file description entries and 01 level numbers `must` startin Area A. All other sentences must startin Area B.

### Name Construction
All user-defined names in COBOL must adhere to the following rules:
*   They must contain at least 1 character and not more than 30 characters.
*   They must contain at least one alphabetic character and must not begin or end with a hyphen.
*   They must be constructed from the characters A to Z, the numbers 0 to 9, and the hyphen. Because the hyphen can be mistaken for the minus sign, a word cannot begin or end with a hyphen.
*   Names are not case-sensitive. SalesDate is the same as salesDate or SALESDATE.
*   None of the many COBOL reserved words may be used as a user-defined name. 

```
TotalPay
Gross-Pay
PrintReportHeadings
Customer10-Rec
```

## Compile and run .cob files
```
cobc -x example-program.cob
./example-program
```

### Breaking down a COBOL program
```
IDENTIFICATION DIVISION. 
PROGRAM-ID. DoCalc.
AUTHOR. Petros Trakadas.
DATE-WRITTEN. 31st May 2025.
DATA DIVISION. 
WORKING-STORAGE SECTION. 
01 FirstNum     PIC 9       VALUE ZEROS.    // 0-9 initialized to zero
01 SecondNum    PIC 9       VALUE ZEROS.    // 0-9 initialized to zero
01 CalcResult   PIC 99      VALUE 0.        // 0-99 initialized to zero
01 UserPrompt   PIC X(38)   VALUE           // alphanumeric of up to 38 chars initialized to the string below
    "Please enter two single digit numbers".
PROCEDURE DIVISION.
CalculateResult.
    DISPLAY UserPrompt
    ACCEPT FirstNum 
    ACCEPT SecondNum 
    COMPUTE CalcResult = FirstNum + SecondNum
    DISPLAY "Result is = " CalcResult 
    STOP RUN.
```

> [!NOTE]  
> In COBOL, every data-item declaration starts with a level number. Level numbers are used to represent [data hierarchy](#data-hierarchy). Because all the items in this example program are independent, elementary data items, they have a level number of 01. This in turn is followed by a storage declaration fo the data item. The storage declaration defines the type and size of the storage required. To do this, COBOL uses a kind of “declaration by example” strategy. An example, or picture (hence PIC), is given of the maximum value the data item can hold. The symbols used in the picture declaration indicate the basic type of the item (numeric = 9, alphanumeric = X, alphabetic = A), and the number of symbols used indicates the size.
> `01 FirstNum PIC 9 VALUE ZEROS.` indicates that FirstNum can hold a cardinal number with value from 0 - 9. 
> If that data item was required to hold an integer number, the picture would have to be defined as `PIC S9` (signed numeric).
> This picture clause is followed by `VALUE` clause specifying that FirstNum starts with an initial value of zero. In COBOL, unless a variable is explicitly given an initial value, its value is undefined.


> [!CAUTION]
> Numeric data items must be given an explicit numeric starting value by means of the VALUE clause, using the INITIALIZE verb, or by assignment. if a numeric data item with an undefined value is used in a calculation, the pro- gram may crash. Of course, a data item with an undefined value may receive the result of a calculation because in that case any non-numeric data is overwritten with the calculation result.