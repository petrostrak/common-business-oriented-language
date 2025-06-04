# Data Declaration
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

![move-rules](https://github.com/petrostrak/common-business-oriented-language/blob/main/assets/move-rules.png)

*Valid and invalid MOVE combinations*

### Alphanumeric MOVEs
Remember the following rule for alphanumeric MOVEs: when the destination item is alphanumeric or alphabetic (PIC X or A), data is copied into the destination area from left to right with space-filling or truncation on the right.

### Numeric MOVEs
Remember the following rule for numeric MOVEs: when the destination item is numeric or edited numeric, data is aligned along the decimal point with zero-filling or truncation as necessary. An edited numeric data item is one that contains symbols such as $ and , and . that format data for output. They are not numeric items, and they can’t be used in calculations (except as the receiving field), but they do obey the decimal-point alignment and zero-filling rules.

```
01 SalePrice PIC 9(4)V99        SalePrice
MOVE ZEROS TO SalePrice          |0000.00|
MOVE  25.5 TO SalePrice          |0025.50|
MOVE 7.553 TO SalePrice          |0007.55|  3
MOVE 93425.158 TO SalePrice    9 |3425.15|  8
MOVE 128 TO SalePrice            |0128.00|
```

## Structured Data
In COBOL, the term elementary item describes an ordinary data item or variable. An elementary item is a data item that is atomic: it has not been further subdivided. Every elementary item must have a PICTURE clause. The PICTURE clause specifies the type and size of the storage required for the data item.

### Group Data Items
A group item in COBOL is a data item that is a collection of elementary and/or group data items. It is a heterogeneous data structure. The constituent parts of a group item may be elementary items or other group items. But ultimately, every group item must be defined in terms of its subordinate elementary items. Because a group item is ultimately defined in terms of elementary items, it cannot have a PICTURE clause, and its size is the sum of the sizes of its subordinate elementary items. A group item is simply a convenient name that you give to a collection of (ultimately) elementary items. Using that name, you can manipulate the collection. In a group item, the hierarchical relationship between the various subordinate items of the group is expressed using level numbers. The higher the level number, the lower the item is in the hierarchy and the more atomic it is. If a group item is the highest item in a data hierarchy, it is referred to as a record and uses the level number 01. The type of a group item is always assumed to be alphanumeric.

### Level Numbers
Level numbers 01 through 49 are the general level numbers used to express data hierarchy. There are also special level numbers such as 66, 77, and 88:
*   66 is used with the RENAMES clause. The RENAMES clause allows you to apply a new name to a data-name or group of contiguous data-names. It is similar to the REDEFINES clause.
*   77 is used to identify a noncontiguous, single data item in the WORKING-STORAGE or LINKAGE sections; it cannot be subdivided, and it cannot be part of a group item.
*   88 is used to implement condition names. Whereas level 66 and level 77 are not used in modern COBOL, level 88s and condition names are very important, useful, and unique weapons in COBOL’s armory.

### Data Hierarchy
Level numbers are used to express data hierarchy.