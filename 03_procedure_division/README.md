# Procedure Division
## Input and Output with ACCEPT and DISPLAY
In COBOL, the ACCEPT and DISPLAY verbs are used to read from the keyboard and write to the screen.

### The DISPLAY Verb
The DISPLAY verb is used to send output to the computer screen or to a peripheral device. A single DISPLAY can be used to display several data items or literals or any combination of these. The concatenation required by some other languages is not required for the DISPLAY verb.

The default display device is the computer screen, but you can use other devices for output by specifying a mnemonic-name with the UPON clause like so `DISPLAY PrinterSetupCodes UPON PrinterPort1`.

### The ACCEPT Verb
There are two formats for the ACCEPT verb:
*   The first gets data from the keyboard or a peripheral device.
*   The second lets you access the system date and time (that is, the date and time held in the
computer’s internal clock) by using certain system variables.

When you use the first format, ACCEPT inserts the data typed on the keyboard into the receiving data item. If the FROM option is used, the data inserted into the receiving data item comes from the device indicated by the mnemonic-name.

The default input device is the computer keyboard, but you can use other devices by specifying a mnemonic-name with the FROM clause. The mnemonic-name is connected to the actual device by an entry in the SPECIAL-NAMES paragraph, CONFIGURATION SECTION, ENVIRONMENT DIVISION.

When you use the second format, ACCEPT moves the data from one of the system variables (DATE, DAY, DAY-OF-WEEK, TIME) into the receiving data item. Two of the system variables also have optional syntactic elements that allow you to specify that the date be supplied with a four-digit year.

## Arithmetic in COBOL
In COBOL, the COMPUTE verb is used to evaluate arithmetic expressions, but there are also specific commands for adding (ADD), subtracting (SUBTRACT), multiplying (MULTIPLY), and dividing (DIVIDE).

With the exception of COMPUTE, DIVIDE with REMAINDER, and some exotic formats of ADD and SUBTRACT, most COBOL arithmetic verbs conform to the template metalanguage shown below

![arithmetics](https://github.com/petrostrak/common-business-oriented-language/blob/main/assets/arithmetics.png)

> [!NOTE]
> All the arithmetic verbs move the result of a calculation into a receiving data item according to the rules for a numeric move: that is, with alignment along the assumed decimal point and with zero-filling or truncation as necessary. In all the arithmetic verbs except COMPUTE, the result of the calculation is assigned to the rightmost data item(s). 

> [!NOTE]
> All arithmetic verbs must use numeric literals or numeric data items (PIC 9) that contain numeric data. There is one exception: data items that receive the result of the calculation but are not themselves one of the operands (do not contribute to the result) may be numeric or edited numeric.

> [!NOTE]
> Where the GIVING phrase is used, the item to the right of the word giving receives the result of the calculation but does not contribute to it. Where there is more than one item after the word giving, each receives the result of the calculation.

> [!NOTE]
> Where the GIVING phrase is not used and there is more than one OperandResult#i, Operand#il is applied to each OperandResult#i in turn, and the result of each calculation is placed in each OperandResult#i.
The maximum size of each operand is 18 digits (31 in ISO 2002 COBOL).

#### Examples of COBOL Arithmetic Statements
```
ADD Takings TO CashTotal
* Adds the value in Takings to the value in CashTotal and puts the result in CashTotal

ADD Males TO Females GIVING TotalStudents
* Adds the value in Males to the value in Females and overwrites the value in TotalStudents with the result

ADD Sales TO ShopSales, CountySales, CountrySales
* Adds the value of Sales to ShopSales and puts the result in ShopSales.
* Adds the value of Sales to CountySales and puts the result in CountySales
* Adds the value of Sales to CountrySales and puts the result in CountrySales

SUBTRACT Tax FROM GrossPay
* Subtracts the value in Tax from the value in GrossPay and puts the result in GrossPay

SUBTRACT Tax FROM GrossPay GIVING NetPay
* Subtracts the value in Tax from the value in GrossPay and puts the result in NetPay

DIVIDE Total BY Members GIVING MemberAverage ROUNDED
* Divides the value in Total by the value in Members and puts the rounded result in MemberAverage

DIVIDE Members INTO Total GIVING MemberAverage
* Divides the value in Members into the value in Total and puts the result in MemberAverage

MULTIPLY 10 BY Magnitude
* Multiplies 10 by the value in Magnitude and puts the result in Magnitude

MULTIPLY Members BY Subs GIVING TotalSubs
* Multiplies the value of Members by the value of Subs and puts the result in TotalSubs
```

> [!NOTE]
> * BY form requires the target variable to be initialized 
> * INTO form doesn't require initialization (will treat as zero if uninitialized)
>   ```
>    MOVE 5 TO Magnitude.
>    MULTIPLY 10 BY Magnitude.    *> Magnitude becomes 50 (5 * 10)
>
>    MOVE 5 TO Magnitude.
>    MULTIPLY 10 INTO Magnitude. *> Magnitude becomes 50 (10 * 5)
>
>    MULTIPLY 10 INTO Uninitialized. *> Uninitialized becomes 0 (10 * 0)
>   ```

### The ON SIZE ERROR
A size error occurs when the computed result is too large or too small to fit into the receiving field and is being truncated. When the ON SIZE ERROR phrase is used, it is followed by a block of COBOL statements that usually alert you that an error condition has occurred.
```
COMPUTE FinalResult = Num1 * Num2 * Num3 * Num4
    ON SIZE ERROR DISPLAY "Alert: FinalResult too small to hold result"
END-COMPUTE
```

If FinalResult is too small to hold the result of all these multiplications, the ON SIZE ERROR activates and the alert message is displayed.

## The COMPUTE Verb
COMPUTE assigns the result of an arithmetic expression to a data item. The arithmetic expression to the right of the equal sign is evaluated, and the result is assigned to the data item(s) on the left of the equal sign.

![compute](https://github.com/petrostrak/common-business-oriented-language/blob/main/assets/compute.png)

#### COMPUTE Examples
```
01 Result 9(4) VALUE 3333.
COMPUTE Result = 90 - 7 * 3 + 50 / 2
Before: 3333, After: 0094

01 Euro PIC 9(5)V99 VALUE 3425.15.
01 Dollar       PIC 9(5)V99 VALUE 1234.75.
01 ExchangeRate PIC 9V9(4)  VALUE 1.3017.
COMPUTE Euro ROUNDED = Dollar / ExchangeRate
Before: 3425.15, After: 0948.57
```

## The ADD Verb
The ADD verb is used for addition. You might think COMPUTE could be used for that, and of course it can, but sometimes it can be simpler to use ADD. For instance, to increment a counter, you need COMPUTE ItemCount = ItemCount + 1, whereas you could just use ADD 1 TO ItemCount.

![add](https://github.com/petrostrak/common-business-oriented-language/blob/main/assets/add.png)

> [!NOTE]
> The ADD verb mostly conforms to the common template, but note the ellipsis after the first operand. This means you could have a statement
> like ADD Num1, Num2, Num3 TO Num4 GIVING Result. What are the semantics of this version of ADD? The items before TO are all added together,
> and then the result is applied to the operand or operands after TO. Note also that in the GIVING version of the ADD verb, the word TO is 
> optional (square brackets). This means you could have a statement like ADD Num1, Num2, Num3 GIVING Result.

## The SUBTRACT Verb
The SUBTRACT verb is a specialized verb used for subtraction. It can be more convenient to use SUBTRACT to decrement acounterratherthanCOMPUTE.Forinstance,todecrementacounteryouneedCOMPUTE ItemCount = ItemCount – 1, whereas you could just use SUBTRACT 1 FROM ItemCount.

![subtract](https://github.com/petrostrak/common-business-oriented-language/blob/main/assets/subtract.png)

> [!NOTE]
> The SUBTRACT verb mostly conforms to the common template, but just as with ADD, there is an ellipsis after the first operand. This means you could have statements like these:
> SUBTRACT Num1, Num2 FROM Num3 GIVING Result.
> SUBTRACT Num1, Num2 FROM NumResult1, NumResult2.
> In the first example, all the items before the word FROM are added together, the combined result is subtracted from num3, and the result is placed in the Result data item. In the second example, all the items before the word FROM are added together. The combined result is subtracted from NumResult1, and the result is placed in NumResult1. The combined result is also subtracted from NumResult2, and the result ofthat calculation is placed in NumResult2.

## The MULTIPLY Verb
The MULTIPLY verb is one of the arithmetic verbs that fully conforms to the common template.

![multiply](https://github.com/petrostrak/common-business-oriented-language/blob/main/assets/multiply.png)

#### MULTIPLY Example
```
01 Fees       PIC 9(3)V99 VALUE 052.24
01 Members    PIC 9(4)    VALUE 1024.
01 TotalFees  PIC 9(5)V99 VALUE ZEROS.
MULTIPLY Fees BY Members GIVING TotalFees.
    DISPLAY "Alert: result to large for TotalFees"
```

## The DIVIDE Verb
The DIVIDE verb has two main formats.
![divide-1](https://github.com/petrostrak/common-business-oriented-language/blob/main/assets/divide-1.png)
![divide-2](https://github.com/petrostrak/common-business-oriented-language/blob/main/assets/divide-2.png)

#### DIVIDE Examples
```
01 Amount1 PIC 9(4) VALUE 2444.
01 Amount2 PIC 9(3) VALUE 354.
DIVIDE 15 INTO Amount1, Amount2.
Amount1: 162, Amount2: 023

01 Qty PIC 9(5) VALUE 31255.
01 Units PIC 9(3) VALUE 115.
01 Average PIC 9(4) VALUE ZEROS.
DIVIDE Qty BY Units GIVING Average ROUNDED.
Average: 0272

01 Quotient PIC 999  VALUE ZEROS.
01 Rem PIC 9 VALUE ZEROS.
DIVIDE 215 BY 10 GIVING Quotient REMAINDER Rem.
Quotient: 21, Rem: 5
```