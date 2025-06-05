# Control Structures: Selection
## IF Statement
![if](https://github.com/petrostrak/common-business-oriented-language/blob/main/assets/if.png)

The StatementBlock following the THEN executes, if the condition is true. The StatementBlock following the ELSE (if used) executes, if the condition is false. The StatementBlock(s) can include any valid COBOL statement including further IF constructs. This allows for nested IF statements. One difference from many other programming languages is that when a condition is evaluated, it evaluates to either true or false. It does not evaluate to 1 or 0.

### Condition Types
The condition that follows the IF is drawn from one of the condition types shown below.
|Condition Type|
|-|
|Relation|
|Class|
|Sign|
|Complex|
|Condition names|

If a condition is not a complex condition, then it is regarded as a simple condition. A simple condition may be negated using the NOT keyword. Bracketing a complex condition causes it to be treated as a simple condition.

#### Relation Conditions
Relation conditions are used to test whether a value is less than, equal to, or greater than another value.

![relation](https://github.com/petrostrak/common-business-oriented-language/blob/main/assets/relation.png)

There is one exception to this: unlike in many other languages, in COBOL there is no symbol for NOT. You must use the word NOT if you want to express this condition.
```
IF Num1 < 10 THEN
    DISPLAY "Num1 is less that 10"
END-IF

IF Num1 LESS THAN 10
    DISPLAY "Num1 is less that 10"
END-IF

IF Num1 GREATER THAN OR EQUAL TO Num2
    MOVE Num1 TO Num2
END-IF

IF Num1 <  (Num2 + ( Num3 / 2))
   MOVE ZEROS TO Num1
END-IF
```

#### Class Conditions
A class condition does not refer to a class in the OO sense. Instead, it refers to the broad category or class (such as numeric, alphabetic, or alphabetic lower or upper) into which a data item may fall

![class](https://github.com/petrostrak/common-business-oriented-language/blob/main/assets/class.png)

```
IF StateName IS ALPHABETIC-UPPER
   DISPLAY "All the letters in StateName are upper case"
END-IF
```

#### User-Defined Class Names
Whereas ALPHABETIC and NUMERIC are predefined class names that identify a subset of the character set, the UserDefinedClassName is a name that you can assign to a defined subset of characters. To define the subset, you must create a CLASS entry in the SPECIAL-NAMES paragraph, of the CONFIGURATION SECTION, in the ENVIRONMENT DIVISION. The CLASS clause assigns a class name to a defined subset of characters.

#### Sign Conditions
The sign condition is used to discover whether the value of an arithmetic expression is less than, greater than, or equal to zero. 

![sign](https://github.com/petrostrak/common-business-oriented-language/blob/main/assets/sign.png)

```
IF (Num2 * 10) - 10 IS NEGATIVE
    DISPLAY "Calculation result is negative"
END-IF

*> the equivalent Relation Condition is
IF (Num2 * 10 / 50) - 10 LESS THAN ZERO
    DISPLAY "Calculation result is negative"
END-IF
```

#### Complex Conditions
Complex conditions are formed by combining two or more simple conditions using the conjunction operator OR or AND. Any condition (simple, complex, condition name) may be negated by preceding it with the word NOT.

![complex](https://github.com/petrostrak/common-business-oriented-language/blob/main/assets/complex.png)

```
*> A complex condition example that detects if the cursor position located at
*> ScrnRow, ScrnCol is on screen (the text screen is 24 lines by 80 columns)
IF (ScrRow > 0 AND ScrRow < 25) AND (ScrCol > 0 AND ScrCol < 81) THEN
    DISPLAY "On Screen"
END-IF
```

#### Defining Condition Names
Condition names are sometimes called level 88s because they are created in the DATA DIVISION using the special level number 88. 

![condition-names](https://github.com/petrostrak/common-business-oriented-language/blob/main/assets/condition-names.png)

Condition names are always associated with a particular data item and are defined immediately after the definition of that data item. A condition name may be associated with a group data item and elementary data, or even the element of a table. The condition name is automatically set to true or false the moment the value of its associated data item changes.

When the VALUE clause is used with condition names, it does not assign a value. Instead, it identifies the value(s) which, if found in the associated data item, make the condition name true.

When identifying the condition values, a single value, a list of values, a range of values, or any combination of these may be specified. To specify a list of values, the entries are listed after the keyword VALUE. The list entries may be separated by commas or spaces but must terminate with a period.

#### Overlapping and Multiple-Value Condition Names
When multiple condition names are associated with a single data item, more than one condition name can be true at the same time.

The list of values that follows a condition name may be a single value, a number of values, or a range of values, or any mixture of these. When a range is specified, the word THROUGH or THRU is used to separate the minimum and maximum values in the range. 