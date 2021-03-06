module VMSpec (spec) where

import qualified Assembly.Parser as Asm
import Common.Types
import Data.Either
import Test.Hspec
import VM.Parser
import VM.Processing
import VM.Types

spec :: SpecWith ()
spec = describe "VM" $ do
  describe "Parser" $
    it "command" $ do
      parse "eq" `shouldBe` Right [Source Eq]
      parse "lt" `shouldBe` Right [Source Lt]
      parse "gt" `shouldBe` Right [Source Gt]
      parse "add" `shouldBe` Right [Source Add]
      parse "sub" `shouldBe` Right [Source Sub]
      parse "neg" `shouldBe` Right [Source Neg]
      parse "and" `shouldBe` Right [Source And]
      parse "or" `shouldBe` Right [Source Or]
      parse "not" `shouldBe` Right [Source Not]
      parse "push constant 82" `shouldBe` Right [Source (Push Constant 82)]
