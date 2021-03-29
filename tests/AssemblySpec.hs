module AssemblySpec (parsing, processing, rendering) where

import Assembly.Linker
import Assembly.Parser
import Assembly.Render
import Assembly.Types
import Common.Types
import Test.Hspec

parsing :: SpecWith ()
parsing = describe "Assembly" $
  describe "Parser" $ do
    it "comments" $ do
      parse "//First comment\n//Second comment"
        `shouldBe` Right
          []
      parse "@123 //A comment"
        `shouldBe` Right
          [Source $ AInstruction (Address 123)]
    describe "A instructions" $ do
      it "raw address" $
        parse "@123"
          `shouldBe` Right
            [Source $ AInstruction (Address 123)]
      it "labels" $ do
        parse "@some_label"
          `shouldBe` Right
            [Source $ AInstruction (Label "some_label")]
        parse "@R3"
          `shouldBe` Right
            [Source $ AInstruction (Label "R3")]
    describe "C instructions" $ do
      it "destination part" $ do
        parse "A=1"
          `shouldBe` Right
            [Source $ CInstruction [A] (Identity (Val 1)) Nothing]
        parse "M=1"
          `shouldBe` Right
            [Source $ CInstruction [M] (Identity (Val 1)) Nothing]
        parse "D=1"
          `shouldBe` Right
            [Source $ CInstruction [D] (Identity (Val 1)) Nothing]
        parse "AMD=1"
          `shouldBe` Right
            [Source $ CInstruction [A, M, D] (Identity (Val 1)) Nothing]
        parse "AD=1"
          `shouldBe` Right
            [Source $ CInstruction [A, D] (Identity (Val 1)) Nothing]
      it "computation parts" $ do
        parse "0"
          `shouldBe` Right
            [Source $ CInstruction [] (Identity (Val 0)) Nothing]
        parse "1"
          `shouldBe` Right
            [Source $ CInstruction [] (Identity (Val 1)) Nothing]
        parse "-1"
          `shouldBe` Right
            [Source $ CInstruction [] (Negate (Val 1)) Nothing]
        parse "D"
          `shouldBe` Right
            [Source $ CInstruction [] (Identity (Reg D)) Nothing]
        parse "A"
          `shouldBe` Right
            [Source $ CInstruction [] (Identity (Reg A)) Nothing]
        parse "M"
          `shouldBe` Right
            [Source $ CInstruction [] (Identity (Reg M)) Nothing]
        parse "!D"
          `shouldBe` Right
            [Source $ CInstruction [] (Not (Reg D)) Nothing]
        parse "!A"
          `shouldBe` Right
            [Source $ CInstruction [] (Not (Reg A)) Nothing]
        parse "!M"
          `shouldBe` Right
            [Source $ CInstruction [] (Not (Reg M)) Nothing]
        parse "-D"
          `shouldBe` Right
            [Source $ CInstruction [] (Negate (Reg D)) Nothing]
        parse "-A"
          `shouldBe` Right
            [Source $ CInstruction [] (Negate (Reg A)) Nothing]
        parse "-M"
          `shouldBe` Right
            [Source $ CInstruction [] (Negate (Reg M)) Nothing]
        parse "D+1"
          `shouldBe` Right
            [Source $ CInstruction [] (Plus D (Val 1)) Nothing]
        parse "A+1"
          `shouldBe` Right
            [Source $ CInstruction [] (Plus A (Val 1)) Nothing]
        parse "M+1"
          `shouldBe` Right
            [Source $ CInstruction [] (Plus M (Val 1)) Nothing]
        parse "D-1"
          `shouldBe` Right
            [Source $ CInstruction [] (Minus D (Val 1)) Nothing]
        parse "A-1"
          `shouldBe` Right
            [Source $ CInstruction [] (Minus A (Val 1)) Nothing]
        parse "M-1"
          `shouldBe` Right
            [Source $ CInstruction [] (Minus M (Val 1)) Nothing]
        parse "D+A"
          `shouldBe` Right
            [Source $ CInstruction [] (Plus D (Reg A)) Nothing]
        parse "D+M"
          `shouldBe` Right
            [Source $ CInstruction [] (Plus D (Reg M)) Nothing]
        parse "D-A"
          `shouldBe` Right
            [Source $ CInstruction [] (Minus D (Reg A)) Nothing]
        parse "D-M"
          `shouldBe` Right
            [Source $ CInstruction [] (Minus D (Reg M)) Nothing]
        parse "A-D"
          `shouldBe` Right
            [Source $ CInstruction [] (Minus A (Reg D)) Nothing]
        parse "M-D"
          `shouldBe` Right
            [Source $ CInstruction [] (Minus M (Reg D)) Nothing]
        parse "D&A"
          `shouldBe` Right
            [Source $ CInstruction [] (And D (Reg A)) Nothing]
        parse "D&M"
          `shouldBe` Right
            [Source $ CInstruction [] (And D (Reg M)) Nothing]
        parse "D|A"
          `shouldBe` Right
            [Source $ CInstruction [] (Or D (Reg A)) Nothing]
        parse "D|M"
          `shouldBe` Right
            [Source $ CInstruction [] (Or D (Reg M)) Nothing]
      it "jump part" $ do
        parse "1;JLT"
          `shouldBe` Right
            [Source $ CInstruction [] (Identity (Val 1)) (Just JLT)]
        parse "1;JLE"
          `shouldBe` Right
            [Source $ CInstruction [] (Identity (Val 1)) (Just JLE)]
        parse "1;JEQ"
          `shouldBe` Right
            [Source $ CInstruction [] (Identity (Val 1)) (Just JEQ)]
        parse "1;JNE"
          `shouldBe` Right
            [Source $ CInstruction [] (Identity (Val 1)) (Just JNE)]
        parse "1;JGT"
          `shouldBe` Right
            [Source $ CInstruction [] (Identity (Val 1)) (Just JGT)]
        parse "1;JGE"
          `shouldBe` Right
            [Source $ CInstruction [] (Identity (Val 1)) (Just JGE)]
        parse "1;JMP"
          `shouldBe` Right
            [Source $ CInstruction [] (Identity (Val 1)) (Just JMP)]
    it "multiline" $
      parse "@2\nD=A\n@R12\nD=D+A\n@lbl\nM=D;JMP"
        `shouldBe` Right
          ( Source
              <$> [ AInstruction (Address 2)
                  , CInstruction [D] (Identity (Reg A)) Nothing
                  , AInstruction (Label "R12")
                  , CInstruction [D] (Plus D (Reg A)) Nothing
                  , AInstruction (Label "lbl")
                  , CInstruction [M] (Identity (Reg D)) (Just JMP)
                  ]
          )

processing :: SpecWith ()
processing =
  describe "Processing" $
    it "Symbol resolution" $
      resolve
        ( Source
            <$> [ AInstruction (Label "Loc") -- 0
                , AInstruction (Label "Dyn1") -- 1
                , AInstruction (Label "Loc2") -- 2
                , Location "Loc"
                , AInstruction (Label "Dyn2") -- 3
                , Location "Loc2"
                , AInstruction (Label "Dyn3") -- 4
                ]
        )
        `shouldBe` Source
          <$> [ AInstruction (Address 3)
              , AInstruction (Address 16)
              , AInstruction (Address 4)
              , AInstruction (Address 17)
              , AInstruction (Address 18)
              ]

rendering :: SpecWith ()
rendering = describe "Rendering" $
  it "Renders A instructions" $ do
    render (AInstruction (Address 123)) `shouldBe` "@123"
    render (AInstruction (Label "R12")) `shouldBe` "@R12"
    render (AInstruction (Label "HI")) `shouldBe` "@HI"
