module VM.Processing where

import           VM.Types                      as VM
import           Assembly.Types                as ASM
import           Common.Types
import Polysemy
import Polysemy.Writer
import Polysemy.Reader
import Polysemy.State

data VMCompiler m a where
  NewDynRetLoc ::String -> VMCompiler m String
  GetFilename ::VMCompiler m String
  Emit :: Source 'AST 'Unresolved ASM.Command -> VMCompiler m ()
  
makeSem ''VMCompiler

compile :: String -> [Source 'AST 'Unresolved VM.Command] -> [Source 'AST 'Unresolved ASM.Command]
compile filename ast = fst . run $ effectsStack program
  where program = sequence_ (compileCommand . unSource <$> ast) >> halt >> subroutines
        effectsStack = evalState 0 . runReader filename . runWriter . vmCompilerToState

vmCompilerToState
  :: Sem (VMCompiler ': effs) a -> Sem (Writer [Source 'AST 'Unresolved ASM.Command] ': Reader String ': State Int ': effs) a
vmCompilerToState = reinterpret3 $ \case
  NewDynRetLoc prefix -> do
    cnt :: Int <- get
    put (succ cnt)
    pure $ prefix ++ show cnt
  GetFilename -> ask
  Emit com    -> tell [com]

halt :: Member VMCompiler effs => Sem effs ()
halt = loc "_HALT" >> jmpLbl "_HALT"

-- TODO: dedup segment push\pop
compileCommand :: Member VMCompiler effs => VM.Command -> Sem effs ()
compileCommand (Push Constant value) = do
  loadI value
  push
compileCommand (Pop Constant _) = error "Pop constant"

compileCommand (Push Argument value) = pushSegment atARG value
compileCommand (Pop Argument index) = popSegment atARG index

compileCommand (Push VM.Local value) = pushSegment atLCL value
compileCommand (Pop VM.Local index) = popSegment atLCL index

compileCommand (Push This value) = pushSegment atTHIS value
compileCommand (Pop This index) = popSegment atTHIS index

compileCommand (Push That value) = pushSegment atTHAT value
compileCommand (Pop That index) = popSegment atTHAT index

compileCommand (Push Temp value) = pushSegment (command $ CInstruction [A] (Plus D (Val 5)) Nothing) value
compileCommand (Pop Temp index) = popSegment (command $ CInstruction [A] (Plus D (Val 5)) Nothing) index

compileCommand (Push Pointer value) = pushSegment (command $ CInstruction [A] (Plus D (Val 3)) Nothing) value
compileCommand (Pop Pointer index)  = popSegment  (command $ CInstruction [A] (Plus D (Val 3)) Nothing) index

compileCommand (Push Static value) = do
  f <- getFilename
  atLbl (f <> "." <> show value)
  load
  push
compileCommand (Pop Static index) = do
  pop
  f <- getFilename
  atLbl (f <> "." <> show index)
  store
compileCommand Add = do
  pop
  derefSP
  command $ CInstruction [M] (Plus D (Reg M)) Nothing
  atSP
  increment
compileCommand Sub = do
  pop
  derefSP
  command $ CInstruction [M] (Minus M (Reg D)) Nothing
  atSP
  increment
compileCommand Neg = do
  derefSP
  command $ CInstruction [M] (Negate (Reg M)) Nothing
  atSP
  increment
compileCommand Eq = do
  popArgs 2
  eqRet <- newDynRetLoc "_EQ_DYN_RET_"
  atLbl eqRet
  command $ CInstruction [D] (Identity (Reg A)) Nothing
  storeR 15
  jmpLbl "_EQ"
  loc eqRet
  loadR 1
  push
compileCommand Lt = do
  popArgs 2
  ltRet <- newDynRetLoc "_LT_DYN_RET_"
  atLbl ltRet
  command $ CInstruction [D] (Identity (Reg A)) Nothing
  storeR 15
  jmpLbl "_LT"
  loc ltRet
  loadR 1
  push
compileCommand Gt = do
  pop
  storeR 2
  pop
  storeR 1
  gtRet <- newDynRetLoc "_GT_DYN_RET_"
  atLbl gtRet
  command $ CInstruction [D] (Identity (Reg A)) Nothing
  storeR 15
  jmpLbl "_GT"
  loc gtRet
  loadR 1
  push
compileCommand VM.And = do
  pop
  derefSP
  command $ CInstruction [M] (ASM.And D (Reg M)) Nothing
  atSP
  increment
compileCommand VM.Or = do
  pop
  derefSP
  command $ CInstruction [M] (ASM.Or D (Reg M)) Nothing
  atSP
  increment
compileCommand VM.Not = do
  derefSP
  command $ CInstruction [M] (ASM.Not (Reg M)) Nothing
  atSP
  increment

pushSegment :: Member VMCompiler effs => Sem effs a -> Address -> Sem effs ()
pushSegment segment i = do
  loadI i
  _ <- segment
  command $ CInstruction [A] (Plus M (Reg D)) Nothing
  load
  push

popSegment :: Member VMCompiler effs => Sem effs a -> Address -> Sem effs ()
popSegment segment i = do
  loadI i
  _ <- segment
  command $ CInstruction [D] (Plus M (Reg D)) Nothing
  atR 13 -- TODO: find suitable temp register
  store  -- Save segment[index]
  pop
  atR 13
  deref
  store

-- subroutines
-- TODO: Extract common parts and try to unify to single subroutine
-- TODO: Compare-subroutine?
subroutines :: Member VMCompiler effs => Sem effs ()
subroutines = eqSubroutine >> ltSubroutine >> gtSubroutine

-- | Subroutine to check if values are equal
-- 1. Store x and y to R1 and R2
-- 2. Store return address to R15
-- 3. Jump to _EQ
-- 4. R1 will contain true or false value after return
eqSubroutine :: Member VMCompiler effs => Sem effs ()
eqSubroutine = do
  loc "_EQ"
  loadR 1
  atR 2
  command $ CInstruction [D] (Minus D (Reg M)) Nothing
  atLbl "_EQ_TRUE"
  command $ CInstruction [] (Identity (Reg D)) (Just JEQ)
  atR 1
  storeI false
  jmpLbl "_EQ_EXIT"
  loc "_EQ_TRUE"
  atR 1
  storeI true
  loc "_EQ_EXIT"
  atR 15
  command $ CInstruction [A] (Identity (Reg M)) Nothing
  jmp

-- | Subroutine to check if one value is less than other
-- 1. Store x and y to R1 and R2
-- 2. Store return address to R15
-- 3. Jump to _EQ
-- 4. R1 will contain true or false value after return
ltSubroutine :: Member VMCompiler effs => Sem effs ()
ltSubroutine = do
  loc "_LT"
  loadR 1
  atR 2
  command $ CInstruction [D] (Minus D (Reg M)) Nothing
  atLbl "_LT_TRUE"
  command $ CInstruction [] (Identity (Reg D)) (Just JLT)
  atR 1
  storeI false
  jmpLbl "_LT_EXIT"
  loc "_LT_TRUE"
  atR 1
  storeI true
  loc "_LT_EXIT"
  atR 15
  command $ CInstruction [A] (Identity (Reg M)) Nothing
  jmp

-- | Subroutine to check if one value is greater than other
-- 1. Store x and y to R1 and R2
-- 2. Store return address to R15
-- 3. Jump to _EQ
-- 4. R1 will contain true or false value after return
gtSubroutine :: Member VMCompiler effs => Sem effs ()
gtSubroutine = do
  loc "_GT"
  loadR 1
  atR 2
  command $ CInstruction [D] (Minus D (Reg M)) Nothing
  atLbl "_GT_TRUE"
  command $ CInstruction [] (Identity (Reg D)) (Just JGT)
  atR 1
  storeI false
  jmpLbl "_GT_EXIT"
  loc "_GT_TRUE"
  atR 1
  storeI true
  loc "_GT_EXIT"
  atR 15
  command $ CInstruction [A] (Identity (Reg M)) Nothing
  jmp

command :: Member VMCompiler effs => ASM.Command -> Sem effs ()
command c = emit $ Source c

-- Jumps, labels, locations
-- | Sets A to address of @lbl@
atLbl :: Member VMCompiler effs => String -> Sem effs ()
atLbl lbl = command $ AInstruction (Label lbl)

-- | Sets A to register @r@
atR :: Member VMCompiler effs => Address -> Sem effs ()
atR r = command $ AInstruction (Address r)

-- | Sets A to corresponding VM segment base
atSP, atLCL, atARG, atTHIS, atTHAT, atTEMP
  :: Member VMCompiler effs => Sem effs ()
atSP = atLbl "SP"
atLCL = atLbl "LCL"
atARG = atLbl "ARG"
atTHIS = atLbl "THIS"
atTHAT = atLbl "THAT"
atTEMP = atLbl "TEMP"

-- | Pops value from stack and sets A to it.
derefSP :: Member VMCompiler effs => Sem effs ()
derefSP = do
  atSP
  decrement
  deref

loc :: Member VMCompiler effs => String -> Sem effs ()
loc l = command $ Location l

-- | Jumps to @l@ label unconditionally
jmpLbl :: Member VMCompiler effs => String -> Sem effs ()
jmpLbl l = atLbl l >> jmp

-- | Jumps to current A unconditionally
jmp :: Member VMCompiler effs => Sem effs ()
jmp = command $ CInstruction [] (Identity (Val 0)) (Just JEQ)

-- | Bool values
true, false :: Value
true = 0xFFFF
false = 0x0000

-- Stack ops
-- | Pop stack value to D
pop :: Member VMCompiler effs => Sem effs ()
pop = atSP >> decrement >> deref >> load

-- | Pop @n@ values from stack and store them in registers from @Rn@ to @R0@ in reverse order
popArgs :: Member VMCompiler effs => Value -> Sem effs ()
popArgs 0 = pure ()
popArgs n = do
  pop
  storeR n
  popArgs (n - 1)

-- | Push D to stack
push :: Member VMCompiler effs => Sem effs ()
push = atSP >> deref >> store >> atSP >> increment

-- Memory ops
-- | Load M to D
load :: Member VMCompiler effs => Sem effs ()
load = command $ CInstruction [D] (Identity (Reg M)) Nothing

-- | Load @Rr@ to D
loadR :: Member VMCompiler effs => Address -> Sem effs ()
loadR r = atR r >> load

-- | Load @v@ to D
loadI :: Member VMCompiler effs => Value -> Sem effs ()
loadI v = do
  command $ AInstruction (Address v)
  command $ CInstruction [D] (Identity (Reg A)) Nothing

-- | Store D to M
store :: Member VMCompiler effs => Sem effs ()
store = command $ CInstruction [M] (Identity (Reg D)) Nothing

-- | Store @Rr@ to M
storeR :: Member VMCompiler effs => Address -> Sem effs ()
storeR r = atR r >> store

-- | Store @v@ to M
storeI :: Member VMCompiler effs => Value -> Sem effs ()
storeI v = do
  command $ AInstruction (Address v)
  command $ CInstruction [M] (Identity (Reg A)) Nothing

-- | Load M to A
deref :: Member VMCompiler effs => Sem effs ()
deref = command $ CInstruction [A] (Identity (Reg M)) Nothing

-- | Decrement value in M
decrement :: Member VMCompiler effs => Sem effs ()
decrement = command $ CInstruction [M] (Minus M (Val 1)) Nothing

-- | Increment value in M
increment :: Member VMCompiler effs => Sem effs ()
increment = command $ CInstruction [M] (Plus M (Val 1)) Nothing
