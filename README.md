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

## Data Declaration
COBOL programs use 3 categories of data:
*   Literals
*   Figurative constants
*   Data items (variables)

Unlike other programming languages, COBOL does not support user-defined constants.

### Literals
A literal is a data item that consists only of the data item value itself. It cannot be referred to by a name. By definition, literals are constants in that they cannot be assigned a different value.
There are two types of literal:
*   Alphanumeric (text/string) literals 

"Petros Trakadas", "1528", "-1528", "1528.95"
*   Numeric literals

1528,   1528.95,  -1528,   +1528

### Data Items
A data item can be defined as a named location in memory in which a program can store a data value and from which it can be retrieve the stored value. A `data name`, or identifier, is the name used to identify the area of memory for the data item.
In addition to the data name, a data item must also be described in terms of its basic type (alphabetic, alphanumeric, numeric) and its size. Every data item used in a COBOL program must have a description in the [DATA DIVISION](#data-division).

## Data Type Enforcement
In COBOL, there are only three types of data: numeric, alphanumeric (text/string), and alphabetic. The distinction between these data types is only weakly enforced by the compiler. In COBOL, it is possible to assign a non-numeric value to a data item that has been declared to be numeric. Therefore, it is the responsibility of the programmer to ensure that non-numeric data is never assigned to a numeric data item intended for use in a calculation. 
> [!CAUTION]
> Attempting to perform computations on numeric data items that contain non-numeric data is a frequent cause of program crashes for beginning CoBol programmers. this can easily happen if the data item has not been initialized to a valid starting value.

### Figurative Constants
Unlike most other programming languages, COBOL does not provide a mechanism for creating user-defined, named constants. This is a serious deficiency. Named constants make program more readable and more maintainbable. In COBOL, a data item can be assigned a value, but there is no way to ensure that, somewhere in the program, some maintenance programmer has not assigned a different value to the data item.
> [!NOTE]
> This deficiency has been addressed in ISO 2002 COBOL standard by means of the CONSTANT clause entry.
>`01 SalesTaxRate CONSTANT AS .06.`

Although COBOL does not allow user-defined named constants, it does have a set of special constants called `figurative constants`. Figurative constants are special constant values that may be used wherever it is legal to use a literal value. However, unlike a literal, when a figurative constant is assigned to a data item, it fills the entire item, overwriting everything in it. Figurative constants are often used to initialize data items. For instance, MOVE SPACES TO CustomerName fills the whole data item with spaces, and MOVE ZEROS TO FinalTotal fills that data item with zeros.

|Figurative Constant|Behavior|
|----------------|-----------------|
|ZERO|Behaves like one or more instances of the literal value 0. The constants ZERO, ZEROS, and|
|ZEROS|ZEROES are all synonyms. Whichever is used, the effect is exactly the same.|
|ZEROES||
|SPACE|Behaves like one or more instances of the space character. SPACE and SPACES are|
|SPACES|synonyms|
|HIGH-VALUE|Behaves like one or more instances of the character in the highest ordinal position in the|
|HIGH-VALUES|current collating sequence (usually the ASCII character set).|
||HIGH-VALUE and HIGH-VALUES are synonyms.|
|LOW-VALUE|Behaves like one or more instances of the character in the lowest ordinal position in the|
|LOW-VALUES|current collating sequence (the null character [hex 00] in the ASCII character set).|
||LOW-VALUE and LOW-VALUES are synonyms.|
|QUOTE|Behaves like one or more instances of the quote character. However, it cannot be used to|
|QUOTES|bracket a non-numeric literal instead of the actual quote character. For instance, QUOTE|
||Freddy QUOTE cannot be used in place of "Freddy".|
||QUOTE and QUOTES are synonyms.|
|ALL literal|Allows an ordinary literal character to behave as if it were a figurative constant.|

## Elementary Data Items
An elementary item is the equivalent of a variable in other languages. It is an atomic data item that is not further subdivided. The type and size of an elementary data item are the type and size specified in its PICTURE clause. In COBOL, an elementary data item declaration consists of a line of code containing the following mandatory items:
*   A level number
*   A data-name or identifier
*   A PICTURE clause

The declaration may also take a number of optional clauses. The most common optional clause is the VALUE clause, which assigns an initial, or starting, value to a data item. Elementary data items that are not a subdivision of a group item must use a level number of 01 or 77.

> [!NOTE]
> A data item declaration may also take a number of other optional clauses such as USAGE, BLANK WHEN ZERO, and JUSTIFIED.

## Declaring Elementary Data Items
COBOL is not a typed language, so it employs a very different mechanism for describing its data items. COBOL uses what could be described as a “declaration by example” strategy. In effect, you provide the system with an example, or template, or picture of the size and type (alphabetic, numeric, alphanumeric) of the item. From this PICTURE clause, the compiler derives the information necessary to allocate the item.

## PICTURE Clause Symbols
To create the required picture, you use a set of symbols. The most common symbols used in standard PICTURE clauses are shown below

|Symbol|Meaning|
|-|-|
|A|Indicates an occurrence of any alphabetic character (a to z plus blank) at the corresponding position in|
||the picture:|
||01 ThreeLetterAcronym   PIC AAA VALUE "DNA".|
|X|Indicates an occurrence of any character from the character set at the corresponding position in the|
||picture:|
||01 Salutation           PIC XXX VALUE "Mr.".|
|9|Indicates the occurrence of a digit at the corresponding position in the picture:
||01 CardinalValue        PIC 9(4) VALUE 1234.|
|V|Indicates the position of the decimal point in a numeric value. It is often referred to as the `assumed`|
||`decimal` point because it is not part of the value but is rather information about the value:|
||01 TotalSales           PIC 9(5)V99 VALUE ZEROS.|
|S|Indicates the presence of a sign, and can only appear at the beginning of a picture:|
||01 IntegerValue         PIC S9(4) VALUE -1234.|

> [!NOTE]
> There are many more picture symbols than those listed above. Most of the remaining symbols will be introduced in [edited pictures](#edited-pictures).

### PICTURE Clause Notes
```
PIC 9(8) is equivalent to PICTURE 99999999.
PIC 9(7)V99 is equivalent to PIC 9999999V99.
PICTURE X(15) is equivalent to PIC XXXXXXXXXXXXXXX. 
PIC S9(5)V9(4) is equivalent to PIC S99999V9999. 
PICTURE 9(18) is equivalent to PIC 999999999999999999.
```
Numeric values can have a maximum of 18 digits, whereas the limit on string values (PIC X) is usually system dependent.

> [!NOTE]
> In the 2002 standard, the maximum number of digits in a numeric literal or PICTURE clause was increased from 18 digits to 31 digits.

#### Example Declarations
```
WORKING-STORAGE SECTION
|Num1|Num2|TaxRate|CustomerName|
|-|-|-|-|
|000|015|35|Mike***********|

DATA DIVISION.
WORKING-STORAGE SECTION.
01 Num1 PIC 999 VALUE ZEROS.
01 Num2 PIC 999 VALUE 15.
01 TaxRate PIC V99 VALUE .35.
01 CustomerName PIC X(15) VALUE "Mike".
```

## Assignment in COBOL
In COBOL there are only 3 basic data types:
*   Alphabetic (PIC A)
*   Alphanumeric (PIC X)
*   Numeric (PIC 9)

### The MOVE Verb
Assignment in COBOL is achieved using the `MOVE` verb.

> [!WARNING]
> The COMPUTE verb, which assigns the result of an arithmetic expression to a data item, should never be used to assign the value of one item to another.

> [!WARNING]
> The SET verb, which can be used to set a condition name to TRUE or to change the value in a table index, should only be used for these specialized purposes.

### MOVE Syntax
The MOVE metalanguage makes the verb seem simple but its operation is complicated by a set of governing rules. The metalanguage for MOVE is as follows:

`MOVE Source$#il TO Destination$#i...`

MOVE copies data from the source identifier (or literal) to one or more destination identifiers. The source and destination identifiers can be group or elementary data items.

### MOVE Rules
The major rules for the MOVE verb are given here.
*   The source and destination identifiers can be either elementary or group data items.
*   When data is copied into a destination item, the contents of the destination item are
completely replaced. The contents of the source item are undisturbed.
*   If the number of characters in the source item is too few to fill the destination item, the rest of the destination item is filled with zeros or spaces.
*   If the number of characters in the source item is too many to fit in the destination item, the characters that cannot fit are lost. This is known as truncation.
*   When the destination item is alphanumeric or alphabetic (PIC X or A), data is copied into the destination area from left to right, with space-filling or truncation on the right.
*   When the destination item is numeric or edited numeric, data is aligned along the decimal point with zero-filling or truncation as necessary.
*   When the decimal point is not explicitly specified in either the source or destination item(s), the item is treated as if it had an assumed decimal point immediately after its rightmost character.