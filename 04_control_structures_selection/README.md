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