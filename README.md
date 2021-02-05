# Simple Fortran interpreter - Functional programming 2020/2021

This project implements some of the constructs from historical FORTRAN, written purely in Haskell.  
Parser combinators inspired by [this paper](https://www.cs.nott.ac.uk/~pszgmh/monparsing.pdf).

## Compilation and usage

```
$ git clone https://github.com/hjaremko/fi.git && cd fi
$ ghc --make -o fi src/Main.hs -isrc
$ ./fi <filename>
```

## Supported statements

### Assumptions
- Every variable is typed float.
- Variables are initialized on the first use with zero.
- Whitespaces should be ignored.

### Output
```fortran
WRITE "Hello world! ", (2*4 + 2)
```
It is possible to print multiple expressions separeted by comma.

### Input
```fortran
READ A
```
Reads a float value from standard input.

### Labels
Statements can be labeled with **integer** values. Labels should be **unique**.

### Arithmetic IFs
```fortran
IF (102 .EQ. 120) 40, 50, 60
40   WRITE "Negative"
50   WRITE "False"
60   WRITE "True"
```
Three-way arithmetic conditional statement. Jumps to three different branches depending on whether the result of an expression is negative, zero, or positive.

### DO loops
```fortran
DO (label) I=(init value), (stop value) [, (step value)]
    WRITE I
(label) CONTINUE
```
Simple loop, correct labels are required. Step value can be omitted (default is 1).

### Assignments
```fortran
I = 123
I = I * (200 + 10) 
```

### Jumps
```fortran
GOTO 13
12 WRITE "This will be omitted"

13 WRITE "Hello world"
```

### Arithmetic and logic expressions
Supported arithmetic operators: `+`, `-`, `*`, `/`, `unary -`, `SQRT` and `(`, `)`.  
Supported logic operators: `.EQ.` (equals), `.NE.` (not equals), `.LT.` (less than), `.LE.` (less equals), `.GT.` (greater than), `.GE.` (greater equals).

True is indicated by value `1.0` and false by `0.0`.

## Example code

```fortran
    WRITE "Square root calculator"

    DO 300 N=0,2
        WRITE "Calculation no. ", N
        READ A
        READ B
        READ C
        D=B*B-4*A*C
        WRITE "Delta: ", D
        IF (D) 101,102,103
        101  WRITE "No roots"
        GOTO 200
    102 X=2
        WRITE "Root: ",X
        GOTO 200
    103 X1=(-B-SQRT(D))/(2*A)
        X2=(-B+SQRT(D))/(2*A)
        WRITE "Root no. 1: ",X1
        WRITE "Root no. 2: ",X2
    200 WRITE "End of calculation no. ", N
    300 CONTINUE

    DO 600 N=0, 30, 10
        WRITE "N: ", N

        DO 700 K=0, 2
            WRITE "Hello from inner loop"
        700 CONTINUE
    600 CONTINUE

    WRITE "Hello World!"

    IF (102.EQ.120) 40, 50, 60
    40   WRITE "Negative"
    50   WRITE "False"
    60   WRITE "True"


    WRITE 10-1 .EQ. -9
    WRITE (1 + 1)+1.EQ.3

    Expr = 1 + 5 * 2 .EQ. 12 - 1
    WRITE "Expr: ", Expr
    WRITE "-50:  ", -50
    WRITE Expr, " <= -50:  ", Expr .LE. -50
    WRITE Expr, " >  -50:  ", Expr .GT. -50

    IF (Expr  .LE. -50) 11, 11, 12
    11 WRITE "False"
    GOTO 13
    12 WRITE "True"

    13 WRITE "End"
```
