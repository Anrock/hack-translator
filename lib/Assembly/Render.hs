-- | Rendering assembly AST to source strings
-- | See 'Render' typeclass
module Assembly.Render where

import Assembly.Types

class Render a where
  render :: a -> String

instance (Render a) => Render [a] where
  render = unlines . fmap render

instance (Render a) => Render (Maybe a) where
  render Nothing = ""
  render (Just a) = render a

instance Render Command where
  render (AInstruction a) = "@" <> render a
  render (Location l) = "(" <> l <> ")"
  render (CInstruction dest comp jmp) = mconcat [dest', comp', jmp']
    where
      dest' = case dest of
        [] -> mempty
        rs -> concat (show <$> rs) <> "="
      jmp' = render jmp
      comp' = render comp

-- TODO: reduce code duplication
instance Render Computation where
  render (Negate op) = "-" <> render op
  render (Not op) = "!" <> render op
  render (Minus lhs rhs) = render lhs <> "-" <> render rhs
  render (Plus lhs rhs) = render lhs <> "+" <> render rhs
  render (And lhs rhs) = render lhs <> "&" <> render rhs
  render (Or lhs rhs) = render lhs <> "|" <> render rhs
  render (Identity (Reg r)) = render r
  render (Identity (Val v)) = show v

instance Render Operand where
  render (Reg r) = render r
  render (Val i) = show i

instance Render Register where
  render = show

instance Render Addressable where
  render (Address a) = show a
  render (Label l) = l

instance Render Jump where
  render j = ";" <> show j
