-- PFL 2023/24 - Haskell practical assignment

-- Turma 8 - Group 7

-- Author: Nelson Campos up202005083
-- Author: Rodrigo Ribeiro up202108679
-- Author: Rui Teixeira up202103345

import Data.List (sortOn, intercalate)
import Data.Char (isDigit, isAlpha)

-- Part 1

-- Do not modify our definition of Inst and Code
data Inst =
  Push Integer | Add | Mult | Sub | Tru | Fals | Equ | Le | And | Neg | Fetch String | Store String | Noop |
  Branch Code Code | Loop Code Code
  deriving Show
type Code = [Inst]

data Value = IntValue Integer | BoolValue Bool
  deriving Show

type Stack = [Value]
type State = [(String, Value)]

createEmptyStack :: Stack
createEmptyStack = [] -- Empty stack is represented by an empty list

stack2Str :: Stack -> String
stack2Str stack = case stack of
    [] -> ""
    [x] -> showValue x
    x:xs -> showValue x ++ "," ++ stack2Str xs -- Recursively convert the stack to a string from the top to the bottom
  where
    showValue (IntValue n) = show n
    showValue (BoolValue b) = show b

createEmptyState :: State
createEmptyState = [] -- Empty state is represented by an empty list

state2Str :: State -> String
state2Str [] = ""
state2Str state = intercalate "," [var ++ "=" ++ showValue val | (var, val) <- sortedState] -- Convert the state to a string by concatenating the variable names and values
  where
    sortedState = sortOn fst state -- Sort the state by variable name
    showValue (IntValue n) = show n
    showValue (BoolValue b) = show b

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

-- To help you test your assembler
testAssembler :: Code -> (String, String)
testAssembler code = (stack2Str stack, state2Str state)
  where (_,stack,state) = run (code, createEmptyStack, createEmptyState)

-- Examples:
-- testAssembler [Push 10,Push 4,Push 3,Sub,Mult] == ("-10","")
-- testAssembler [Fals,Push 3,Tru,Store "var",Store "a", Store "someVar"] == ("","a=3,someVar=False,var=True")
-- testAssembler [Fals,Store "var",Fetch "var"] == ("False","var=False")
-- testAssembler [Push (-20),Tru,Fals] == ("False,True,-20","")
-- testAssembler [Push (-20),Tru,Tru,Neg] == ("False,True,-20","")
-- testAssembler [Push (-20),Tru,Tru,Neg,Equ] == ("False,-20","")
-- testAssembler [Push (-20),Push (-21), Le] == ("True","")
-- testAssembler [Push 5,Store "x",Push 1,Fetch "x",Sub,Store "x"] == ("","x=4")
-- testAssembler [Push 10,Store "i",Push 1,Store "fact",Loop [Push 1,Fetch "i",Equ,Neg] [Fetch "i",Fetch "fact",Mult,Store "fact",Push 1,Fetch "i",Sub,Store "i"]] == ("","fact=3628800,i=1")
-- If you test:
-- testAssembler [Push 1,Push 2,And]
-- You should get an exception with the string: "Run-time error"
-- If you test:
-- testAssembler [Tru,Tru,Store "y", Fetch "x",Tru]
-- You should get an exception with the string: "Run-time error"


-- Part 2

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

compA :: Aexp -> Code
compA (Num n) = [Push n]                            -- Push a constant number onto the stack
compA (VarA x) = [Fetch x]                          -- Fetch the value of a variable from the state
compA (AddA a1 a2) = compA a2 ++ compA a1 ++ [Add]  -- Compile and add two arithmetic expressions
compA (Mul a1 a2) = compA a2 ++ compA a1 ++ [Mult]  -- Compile and multiply two arithmetic expressions
compA (SubA a1 a2) = compA a2 ++ compA a1 ++ [Sub]  -- Compile and subtract two arithmetic expressions
compA (NegA a) = compA a ++ [Neg]                   -- Compile and negate an arithmetic expression

compB :: Bexp -> Code
compB (Bool b) = if b then [Tru] else [Fals]        -- Push a constant boolean onto the stack
compB (VarB x) = [Fetch x]                          -- Fetch the value of a variable from the state
compB (EqA a1 a2) = compA a1 ++ compA a2 ++ [Equ]   -- Compile and compare two arithmetic expressions
compB (EqB b1 b2) = compB b1 ++ compB b2 ++ [Equ]   -- Compile and compare two boolean expressions
compB (LeB a1 a2) = compA a2 ++ compA a1 ++ [Le]    -- Compile and compare two arithmetic expressions
compB (AndB b1 b2) = compB b1 ++ compB b2 ++ [And]  -- Compile and AND two boolean expressions
compB (NegB b) = compB b ++ [Neg]                   -- Compile and negate a boolean expression

compile :: Program -> Code
compile [] = [] -- Empty program results in empty machine instructions
compile (x:xs) = case x of
  Assignment var a -> compA a ++ [Store var] ++ compile xs -- Compile assignment statement and proceed to the rest of the program
  IfThenElse cond thenStm elseStm -> compB cond ++ [Branch (compile thenStm) (compile elseStm)] ++ compile xs -- Compile if-else statement
  While cond stm -> Loop (compB cond) (compile stm) : compile xs -- Compile while loop

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

-- Lexer function to convert a string into a list of tokens
lexer :: String -> [Token]
lexer [] = []
lexer (' ':rest) = lexer rest                                 -- Ignore spaces
lexer ('\n':rest) = lexer rest                                -- Ignore newlines
lexer (';':rest) = SemicolonTok : lexer rest                  -- Semicolon is a token
lexer ('(':rest) = OpenTok : lexer rest                       -- Left parenthesis is a token
lexer (')':rest) = CloseTok : lexer rest                      -- Right parenthesis is a token
lexer (':':'=':rest) = AssignmentTok : lexer rest             -- Assignment operator is a token
lexer ('*':rest) = MulTok : lexer rest                        -- Multiplication operator is a token
lexer ('+':rest) = SumTok : lexer rest                        -- Addition operator is a token
lexer ('-':rest) = SubTok : lexer rest                        -- Subtraction operator is a token
lexer ('<':'=':rest) = LeTok : lexer rest                     -- Less or equal than operator is a token
lexer ('=':'=':rest) = EqATok : lexer rest                    -- Equality operator for integers is a token
lexer ('n':'o':'t':rest) = NegTok : lexer rest                -- Logical NOT operator is a token
lexer ('=':rest) = EqBTok : lexer rest                        -- Equality operator for Booleans is a token
lexer ('a':'n':'d':rest) = AndTok : lexer rest                -- Logical AND operator is a token
lexer ('i':'f':rest) = IfTok : lexer rest                     -- If keyword is a token
lexer ('t':'h':'e':'n':rest) = ThenTok : lexer rest           -- Then keyword is a token
lexer ('e':'l':'s':'e':rest) = ElseTok : lexer rest           -- Else keyword is a token
lexer ('w':'h':'i':'l':'e':rest) = WhileTok : lexer rest      -- While keyword is a token
lexer ('d':'o':rest) = DoTok : lexer rest                     -- Do keyword is a token
lexer ('T':'r':'u':'e':rest) = BoolTok True : lexer rest      -- True is a token
lexer ('F':'a':'l':'s':'e':rest) = BoolTok False : lexer rest -- False is a token
lexer (c : rest)
  | isDigit c = IntTok (read (c : takeWhile isDigit rest))    -- Integer is a token
                : lexer (dropWhile isDigit rest)
  | isAlpha c = VarTok (c : takeWhile isAlpha rest)           -- Variable name is a token
                : lexer (dropWhile isAlpha rest)
  | otherwise = error ("Unknown token: " ++ [c])              -- Unknown token

-- Function to build a list of statements from a list of tokens
buildData :: [Token] -> Program
buildData [] = [] -- Base case: empty list of tokens -> empty list of statements
buildData tokens = do
  case findNotInner [SemicolonTok] tokens of -- Find the index of the first semicolon token that is not within parentheses
    Just index -> do                         -- If it finds one
      let (stm, rest) = splitAt index tokens -- Split the list of tokens at that index
      if head stm == OpenTok then buildData (tail (init stm)) -- If the first token in the statement is an opening parenthesis, remove it and its closing counterpart
      else case rest of 
        [_] -> [buildStm stm] -- If there is only one token left, build the statement and add it to the result
        _ -> buildStm stm : buildData (tail rest) -- Build the statement and recurse on the remaining tokens
    Nothing -> do
      buildData (tail (init tokens)) -- If no semicolon is found, recurse on the remaining tokens

-- Function to build a statement from a list of tokens
buildStm :: [Token] -> Stm
buildStm tokens =
  case head tokens of
    IfTok -> do -- "if"
      let (bexp, stm) = break (== ThenTok) tokens
      case findNotInner [ElseTok] (tail stm) of             -- Find the index of the first 'ElseTok' that is not within parentheses
        Just index -> do                                    -- If it finds one
          let (thenStm, elseStm) = splitAt index (tail stm) -- Split the statement at that index
          case head (tail elseStm) of                                                                    -- Check if the next token after 'ElseTok' is an opening parenthesis
            OpenTok -> IfThenElse (buildBexp (tail bexp)) (buildData thenStm) (buildData (tail elseStm)) -- If it is, remove it and its closing counterpart and build the statement
            _ -> IfThenElse (buildBexp (tail bexp)) (buildData thenStm) [buildStm (tail elseStm)]        -- Otherwise, build the statement
    WhileTok -> do -- "while"
      let (bexp, stm) = break (== DoTok) tokens
      case head (tail stm) of                                           -- Check if the next token after 'DoTok' is an opening parenthesis
        OpenTok -> While (buildBexp (tail bexp)) (buildData (tail stm)) -- If it is, remove it and its closing counterpart and build the statement
        _ -> While (buildBexp (tail bexp)) [buildStm (tail stm)]        -- Otherwise, build the statement
    VarTok var -> do -- ":="
      let (_, aexp) = break (== AssignmentTok) tokens
      Assignment var (buildAexp (tail aexp))
    _ -> error "Unexpected token"

-- Function to build an arithmetic expression from a list of tokens
buildAexp :: [Token] -> Aexp
buildAexp [IntTok n] = Num n
buildAexp [VarTok x] = VarA x
buildAexp tokens =
  case findNotInner [SumTok, SubTok] (reverse tokens) of
    Just reversedIndex -> do
      let index = length tokens - reversedIndex - 1
      let (aexp1, aexp2) = splitAt index tokens
      if tokens!!index == SumTok
        then AddA (buildAexp aexp1) (buildAexp (tail aexp2)) -- "+"
        else SubA (buildAexp aexp1) (buildAexp (tail aexp2)) -- "-"
    Nothing -> do
      case findNotInner [MulTok] (reverse tokens) of
        Just reversedIndex -> do
          let index = length tokens - reversedIndex - 1
          let (aexp1, aexp2) = splitAt index tokens
          Mul (buildAexp aexp1) (buildAexp (tail aexp2)) -- "*"
        Nothing -> buildAexp (tail (init tokens)) -- Remove parentheses. This is last because parentheses has the highest priority.

-- Function to build a boolean expression from a list of tokens
buildBexp :: [Token] -> Bexp
buildBexp [BoolTok b] = Bool b
buildBexp [VarTok x] = VarB x
buildBexp tokens =
  case findNotInner [AndTok] (reverse tokens) of
    Just reversedIndex -> do
      let index = length tokens - reversedIndex - 1
      let (bexp1, bexp2) = splitAt index tokens
      AndB (buildBexp bexp1) (buildBexp (tail bexp2)) -- "and"
    Nothing -> do
      case findNotInner [EqBTok] (reverse tokens) of
        Just reversedIndex -> do
          let index = length tokens - reversedIndex - 1
          let (bexp1, bexp2) = splitAt index tokens
          EqB (buildBexp bexp1) (buildBexp (tail bexp2)) -- "="
        Nothing -> do
          case findNotInner [NegTok] (reverse tokens) of
            Just reversedIndex -> do
              let index = length tokens - reversedIndex - 1
              let bexp = drop index tokens
              NegB (buildBexp (tail bexp)) -- "not"
            Nothing -> do
              case findNotInner [EqATok] (reverse tokens) of
                Just reversedIndex -> do
                  let index = length tokens - reversedIndex - 1
                  let (aexp1, aexp2) = splitAt index tokens
                  EqA (buildAexp aexp1) (buildAexp (tail aexp2)) -- "=="
                Nothing -> do
                  case findNotInner [LeTok] (reverse tokens) of
                    Just reversedIndex -> do
                      let index = length tokens - reversedIndex - 1
                      let (aexp1, aexp2) = splitAt index tokens
                      LeB (buildAexp aexp1) (buildAexp (tail aexp2)) -- "<="
                    Nothing -> buildBexp (tail (init tokens)) -- Remove parentheses. This is last because parentheses has the highest priority.

-- Function to find the index of the first ocurrence of a not nested token.
-- A token is considered nested if it is between parentheses or inside an if statement
findNotInner :: [Token] -> [Token] -> Maybe Int
findNotInner targets = find 0 0 -- It will call the find function with depth 0, index 0 and the list of tokens
  where
    find _ _ [] = Nothing -- If it reaches the end of the token's list or given an empty list, it will return Nothing
    find depth index (x:rest) =
      case x of
        OpenTok -> find (depth + 1) (index + 1) rest              -- Finds "(", increase depth (entering nested)
        ThenTok -> find (depth + 1) (index + 1) rest              -- Finds "then", increase depth (entering if)
        CloseTok -> find (depth - 1) (index + 1) rest             -- Finds ")", decrease depth (leaving nested)
        ElseTok | depth /= 0 -> find (depth - 1) (index + 1) rest -- Finds "else", decrease depth (leaving if)
        _ -> do
          if depth == 0 && (x `elem` targets) -- If it's not nested and it finds what it's looking for
            then Just index                   -- It will return the index
            else find depth (index + 1) rest  -- Otherwise it will keep searching

-- Function to parse a string into a program
parse :: String -> Program
parse = buildData . lexer

-- To help you test your parser
testParser :: String -> (String, String)
testParser programCode = (stack2Str stack, state2Str state)
  where (_,stack,state) = run (compile (parse programCode), createEmptyStack, createEmptyState)

-- Examples:
-- testParser "x := 5; x := x - 1;" == ("","x=4")
-- testParser "x := 0 - 2;" == ("","x=-2")
-- testParser "if (not True and 2 <= 5 = 3 == 4) then x :=1; else y := 2;" == ("","y=2")
-- testParser "x := 42; if x <= 43 then x := 1; else (x := 33; x := x+1;);" == ("","x=1")
-- testParser "x := 42; if x <= 43 then x := 1; else x := 33; x := x+1;" == ("","x=2")
-- testParser "x := 42; if x <= 43 then x := 1; else x := 33; x := x+1; z := x+x;" == ("","x=2,z=4")
-- testParser "x := 44; if x <= 43 then x := 1; else (x := 33; x := x+1;); y := x*2;" == ("","x=34,y=68")
-- testParser "x := 42; if x <= 43 then (x := 33; x := x+1;) else x := 1;" == ("","x=34")
-- testParser "if (1 == 0+1 = 2+1 == 3) then x := 1; else x := 2;" == ("","x=1")
-- testParser "if (1 == 0+1 = (2+1 == 4)) then x := 1; else x := 2;" == ("","x=2")
-- testParser "x := 2; y := (x - 3)*(4 + 2*3); z := x +x*(2);" == ("","x=2,y=-10,z=6")
-- testParser "i := 10; fact := 1; while (not(i == 1)) do (fact := fact * i; i := i - 1;);" == ("","fact=3628800,i=1")

-- testParser "x:=3-1;" == ("","x=2")
-- testParser "x:=3-1-1; y:=x+1;" == ("","x=1,y=2")
-- testParser "x:=3-1*3+(2-2*1+1-1);" == ("","x=0")
-- testParser "aZb:=3-1*3+(2-2*1+1-1);" == ("","aZb=0")
-- testParser "if (True) then () else ();" == ("","")
-- testParser  "x := 1; (y := 2; z := y+x;);" == ("","x=1,y=2,z=3")
-- testParser "a := 1071; b := 462; while (not(a == 0)) do (temp := a; while (a <= b) do (b := b - a;); a := b; b := temp;); gcd := b;" == ("","a=0,b=21,gcd=21,temp=21")
-- testParser "x := 5; if x <= 5 then y := 1; else if x <= 10 then y := 2; else if x <= 15 then y := 3; else y := 4;" == ("","x=5,y=1")
-- testParser "x := 10; if x <= 5 then y := 1; else if x <= 10 then y := 2; else if x <= 15 then y := 3; else y := 4;" == ("","x=10,y=2")
-- testParser "x := 15; if x <= 5 then y := 1; else if x <= 10 then y := 2; else if x <= 15 then y := 3; else y := 4;" == ("","x=15,y=3")
-- testParser "x := 20; if x <= 5 then y := 1; else if x <= 10 then y := 2; else if x <= 15 then y := 3; else y := 4;" == ("","x=20,y=4")
-- testParser "x := 1; if x <= 1 then x := 1; else x := 2; if x <= 1 then x := 3; else x := 4;" == ("","x=1")
