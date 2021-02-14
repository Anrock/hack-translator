-- | Common types and functions
module Common where

-- | Enum type of hack-related source types
data SourceType = Vm | Asm | Hack deriving (Show, Eq)

-- | Maps file extension to 'SourceType'
extensionToSourceType :: String -> Maybe SourceType
extensionToSourceType "hack" = Just Hack
extensionToSourceType "asm"  = Just Asm
extensionToSourceType "vm"   = Just Vm
extensionToSourceType _      = Nothing

