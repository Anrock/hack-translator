module Assembly.Linker ( SymbolTable
                       , predefinedSymbols
                       , stdResolver
                       , resolve
                       ) where

import Polysemy
import Data.Map.Strict ((!))
import qualified Data.Map.Strict as M

import Assembly.Types
import Common.Types
import Polysemy.State

type Symbol   = String
type SymbolTable = M.Map String (Maybe Address)
data SymResolverState = SymResolverState {
       symtab :: SymbolTable,
       line :: Address,
       nextDynAddr :: Address
     }

stdResolver :: SymResolverState
stdResolver = SymResolverState predefinedSymbols 0 16

predefinedSymbols :: SymbolTable
predefinedSymbols = M.fromList $ [("SP",     Just 0),
                                  ("LCL",    Just 1),
                                  ("ARG",    Just 2),
                                  ("THIS",   Just 3),
                                  ("THAT",   Just 4),
                                  ("SCREEN", Just 16384),
                                  ("KBD",    Just 24576)] ++
                                    [('R':show r, Just r) | r <- [0..15]]

runSymResolver :: SymResolverState -> Sem (SymResolver' ': r) a -> Sem r SymResolverState
runSymResolver symres = execState symres . symresToState where
  symresToState :: Sem (SymResolver' ': eff) a -> Sem (State SymResolverState ': eff) a
  symresToState = reinterpret $ \case
    GetLineNum    -> gets line
    GetDynAddress -> do 
      addr <- gets nextDynAddr
      modify (\s@SymResolverState {nextDynAddr} -> s {nextDynAddr = succ nextDynAddr})
      pure addr
    BumpLine      -> modify (\s@SymResolverState {line} -> s {line = succ line})
    (ResolveTo l a) -> modify
      (\s@SymResolverState {symtab} -> s {symtab = M.insert l (Just a) symtab})
    (AddSymbol sym) -> modify (\s@SymResolverState{symtab} ->
      s {symtab = M.insertWith (\_ old -> old) sym Nothing symtab})
    GetUnresolved -> M.keys . M.filter (\case Nothing -> True
                                              _          -> False) <$> gets symtab

-- TODO: Remove excessive effects
data SymResolver' m a where
  GetLineNum     :: SymResolver' m Address
  GetDynAddress  :: SymResolver' m Address
  BumpLine       :: SymResolver' m ()
  GetUnresolved  :: SymResolver' m [Symbol]
  ResolveTo      :: Symbol -> Address -> SymResolver' m ()
  AddSymbol      :: Symbol -> SymResolver' m ()

makeSem ''SymResolver'

resolveLocation :: Member SymResolver' effs => String -> Sem effs ()
resolveLocation sym = do
  line <- getLineNum
  resolveTo sym line

resolveDynamic :: Member SymResolver' effs => Symbol -> Sem effs ()
resolveDynamic s = do
  loc <- getDynAddress
  resolveTo s loc

resolveDynamics :: Member SymResolver' effs => Sem effs ()
resolveDynamics = do
  unresolved <- getUnresolved
  mapM_ resolveDynamic unresolved

gather :: Member SymResolver' effs => [Source 'AST 'Unresolved Command] -> Sem effs ()
gather src = mapM_ gather' src >> resolveDynamics
  where gather' :: Member SymResolver' effs => Source 'AST 'Unresolved Command -> Sem effs ()
        gather' (Source (AInstruction (Label l))) = addSymbol l >> bumpLine
        gather' (Source (Location l)) = resolveLocation l
        gather' _ = bumpLine

-- | Resolve labels\locations against given 'SymbolTable'
resolveWith :: [Source 'AST 'Unresolved Command] -> SymbolTable -> [Source 'AST 'Resolved Command]
resolveWith src st = foldMap resolveWith' src
  where resolveWith' :: Source 'AST 'Unresolved Command -> [Source 'AST 'Resolved Command]
        resolveWith' (Source (AInstruction (Label l))) = [Source . AInstruction $ Address $ addr l]
        resolveWith' (Source (Location _)) = []
        resolveWith' (Source l) = [Source l]
        addr l = let (Just a) = st ! l in a

-- | Resolve labels\locations in assembly AST
resolve :: [Source 'AST 'Unresolved Command] -> [Source 'AST 'Resolved Command]
resolve src = src `resolveWith` st
  where st = symtab . run $ runSymResolver stdResolver $ gather src
