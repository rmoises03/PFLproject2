# PFL_TP2_T08_G07

## Instalation and Execution

To execute the program, follow these steps:

- Open ghci.
- Load `main.hs`, located on the folder `src` with `:load main.hs`.
- Test using `testAssembler [...]` and `testParser "..."` with valid commands.

## Part 1

For the inicial part of the project, we were tasked with developing an assembler. This assembler is responsible for handling machine instructions and managing the machine's state and stack accordingly.

The first decision we had to make was how to define the data types for our `Stack` and `State`. We decided on the following:

```hs
data Value = IntValue Integer | BoolValue Bool
  deriving Show

type Stack = [Value]
type State = [(String, Value)]
```

The `Stack` is capable of holding either integer or boolean constants. For this purpose, we introduced a new data type, `Value`, to represent each element within the stack. Consequently, our stack comprises elements of type `Value`.

The `State` was conceived as a collection of tuples pairing a `String` (variable name) with a corresponding `Value` (integer or boolean).

To display the `Stack` and `State`, we have defined the functions `stack2Str` and `state2str` respectively. These functions format and print the values of each type in the especified order.

The `run` function serves as the assembler, systematically handling each instruction and modifying the stack and state structures until all instructions have been executed.

```hs
run :: (Code, Stack, State) -> (Code, Stack, State)
run ([], stack, state) = ([], stack, state)
run (inst:rest, stack, state) = case inst of
  Push n -> run (rest, IntValue n : stack, state) -- Push a constant number onto the stack

  Add -> case stack of -- Add two values from the stack
    IntValue n1 : IntValue n2 : stack' -> run (rest, IntValue (n1 + n2) : stack', state)
    _ -> error "Run-time error"

  Mult -> case stack of -- Multiply two values from the stack
    IntValue n1 : IntValue n2 : stack' -> run (rest, IntValue (n1 * n2) : stack', state)
    _ -> error "Run-time error"

  Sub -> case stack of -- Subtract two values from the stack
    IntValue n1 : IntValue n2 : stack' -> run (rest, IntValue (n1 - n2) : stack', state)
    _ -> error "Run-time error"

  Tru -> run (rest, BoolValue True : stack, state) -- Push a constant boolean (True) onto the stack

  Fals -> run (rest, BoolValue False : stack, state) -- Push a constant boolean (False) onto the stack

  Equ -> case stack of -- Compare two values from the stack (equality)
    IntValue n1 : IntValue n2 : stack' -> run (rest, BoolValue (n1 == n2) : stack', state) -- Compare two integers from the stack
    BoolValue n1 : BoolValue n2 : stack' -> run (rest, BoolValue (n1 == n2) : stack', state) -- Compare two booleans from the stack
    _ -> error "Run-time error"

  Le -> case stack of -- Compare two integers from the stack (less or equal)
    IntValue n1 : IntValue n2 : stack' -> run (rest, BoolValue (n1 <= n2) : stack', state)
    _ -> error "Run-time error"

  And -> case stack of -- Logical AND of two boolean values from the stack
    BoolValue b1 : BoolValue b2 : stack' -> run (rest, BoolValue (b1 && b2) : stack', state)
    _ -> error "Run-time error"

  Neg -> case stack of -- Negate a value from the stack
    BoolValue b : stack' -> run (rest, BoolValue (not b) : stack', state) -- Negate a boolean from the stack
    IntValue n : stack' -> run (rest, IntValue (-n) : stack', state) -- Negate an integer from the stack
    _ -> error "Run-time error"

  Fetch var -> case filter (\(x, _) -> x == var) state of -- Fetch the value of a variable from the state
    (_, val) : _ -> run (rest, val : stack, state)
    _ -> error "Run-time error"

  Store var -> case stack of -- Store a value from the stack in the state
    val : stack' -> let updatedState = (var, val) : filter (\(x, _) -> x /= var) state
                    in run (rest, stack', updatedState)
    _ -> error "Run-time error"

  Noop -> run (rest, stack, state) -- Do nothing

  Branch code1 code2 -> case stack of -- Branch to one of two code blocks depending on the value on top of the stack
    BoolValue True : stack' -> run (code1 ++ rest, stack', state) -- If the value on top of the stack is True, run the first code block
    BoolValue False : stack' -> run (code2 ++ rest, stack', state) -- If the value on top of the stack is False, run the second code block
    _ -> (inst:rest, stack, state)

  Loop code1 code2 -> run (code1 ++ [Branch (code2 ++ [Loop code1 code2]) [Noop]] ++ rest, stack, state) -- Loop a code block while the value on top of the stack is True
```

## Part 2

In the second part of the project, we were tasked with developing a parser, `parse`, and a compiler, `compile`, with two auxiliary functions, `compA` and `compB`.

The data structures defined are the following:

```hs
-- Arithmetic expressions
data Aexp =
    Num Integer           -- A constant number                     (Push n)
  | VarA String           -- A variable                            (Fetch "x")
  | AddA Aexp Aexp        -- Addition of two expressions           (Add)
  | Mul Aexp Aexp         -- Multiplication of two expressions     (Mult)
  | SubA Aexp Aexp        -- Subtraction of two expressions        (Sub)
  | NegA Aexp             -- Negation of an arithmetic expression  (Neg)
  deriving Show

-- Boolean expressions
data Bexp =
    Bool Bool             -- A constant boolean                                       (Tru/Fals)
  | VarB String           -- A variable                                               (Fetch "x")
  | EqA Aexp Aexp         -- Equality check between two arithmetic expressions        (Equ)
  | EqB Bexp Bexp         -- Equality check between two boolean expressions           (Equ)
  | LeB Aexp Aexp         -- Less than comparison between two arithmetic expressions  (Le)
  | NegB Bexp             -- Negation of a boolean expression                         (Neg)
  | AndB Bexp Bexp        -- Logical AND of two boolean expressions                   (And)
  deriving Show

-- Statements
data Stm =
    Assignment String Aexp          -- Assignment: x := a      (Push a,Store "x")
  | IfThenElse Bexp Program Program -- If-then-else statement  (Branch)
  | While Bexp Program              -- While loop              (Loop)
  deriving Show
type Program = [Stm]

-- Token data type to represent lexical elements in the language
data Token
  = SemicolonTok    -- ";"
  | AssignmentTok   -- ":="
  | EqATok          -- "=="
  | LeTok           -- "<="
  | EqBTok          -- "="
  | NegTok          -- "not"
  | AndTok          -- "and"
  | MulTok          -- "*"
  | SumTok          -- "+"
  | SubTok          -- "-"
  | IfTok           -- "if"
  | ThenTok         -- "then"
  | ElseTok         -- "else"
  | WhileTok        -- "while"
  | DoTok           -- "do"
  | OpenTok         -- "("
  | CloseTok        -- ")"
  | IntTok Integer  -- number
  | BoolTok Bool    -- True or False
  | VarTok String   -- variable name
  deriving (Eq, Show)
```

As instructed, we defined `Aexp` for arithmetic expressions (and respective constants), `Bexp` for boolean expressions (and respective constants), `Stm` for statements and structures, `Program`, which is simply a list of `Stm` and `Token`, witch contail all the possible tokens in the language.

Our code structure involved multiple functions:

- The function `lexer` is responsible for translatting the inputted string into a list of tokens.
- The function `parse` is responsible for translatting the list of tokens into a program.
- The function `buildData` is responsible for building the program from a list of tokens.
- The function `buildStm` is responsible for building statments from a list of tokens.
- The function `buildAexp` is responsible for building arithmetic expressions from a list of tokens.
- The function `buildBexp` is responsible for building boolean expressions from a list of tokens.
- The function `findNotInner` is responsible for finding the first token that is not nested (A token is considered nested if it is between parentheses or inside an if statement) used in all build functions.
- The function `compile` is responsible for translatting the program into a list of instructions (code).
- The function `compA` is responsible for compiling de arithmetic expressions.
- The function `compB` is responsible for compiling de boolean expressions.
- Finally, the code will be send to the assembler.

## Conclusion

This project encompassed two main objectives: building an assembler for a low-level machine and crafting a compiler for a simple imperative language. The assembler handled machine instructions, state, and stack operations, while the parser and the compiler translated programs written in the imperative language into instructions executable by the low-level machine. This project provided valuable insights into language translation, the execution of computer instructions, and the intricacies of expressions and statements. Through this project, we learned about how languages are translated, how computers handle instructions, and the ins and outs of expressions and statements.

## Group Identification - T08 G07

|   Number   | Name                                       | Contribution |
|------------|--------------------------------------------|--------------|
| 202005083  | Nelson Teixeira Campos                     | 60%          |
| 202108679  | Rodrigo Moisés Baptista Ribeiro            | 20%          |
| 202103345  | Rui Pedro Monteiro Reis Magalhães Teixeira | 20%          |
