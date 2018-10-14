module Sec10.ParserSpec (spec) where

import Sec10.SimplyTyped
import Sec10.Parser hiding (context)
import Test.Hspec
  
spec :: Spec
spec =
  describe "judgement" $ do
    context "in simple cases" $ do
      it "should parse without context" $
        fromString "⊢true:Bool" `shouldBe` 
          Just ([], NTmTrue Info, TyBool)
      it "should parse with context" $
        fromString "f:Bool⊢true:Bool" `shouldBe` 
          Just ([("f", VarBind TyBool)], NTmTrue Info, TyBool)
      it "should parse with arr context" $
        fromString "f:Bool→Bool⊢true:Bool" `shouldBe` 
          Just ([("f", VarBind (TyArr TyBool TyBool))], NTmTrue Info, TyBool) 
      it "should parse if-clause" $
        fromString "⊢if false then true else false:Bool" `shouldBe` 
          Just ([],  NTmIf Info (NTmFalse Info) (NTmTrue Info) (NTmFalse Info), 
            TyBool)
      it "should parse var" $
        fromString "⊢x:Bool" `shouldBe` 
          Just ([],  NTmVar Info "x", TyBool)
      it "should parse abs" $
        fromString "⊢\\x:Bool.true:Bool" `shouldBe` 
          Just ([], NTmAbs Info "x" TyBool (NTmTrue Info), TyBool)
      it "should parse app" $
        fromString "⊢(\\x:Bool. if x then true else x)true:Bool" `shouldBe` 
          Just ([], NTmApp Info (NTmAbs Info "x" TyBool (NTmIf Info 
            (NTmVar Info "x") (NTmTrue Info) (NTmVar Info "x"))) 
            (NTmTrue Info),TyBool)
    context "in 9.2.2. cases" $
      it "should parse (1)" $
        fromString "f:Bool→Bool⊢f(if false then true else false):Bool" `shouldBe`
          Just ([("f", VarBind (TyArr TyBool TyBool))], 
            NTmApp Info (NTmVar Info "f") 
              (NTmIf Info (NTmFalse Info) (NTmTrue Info) (NTmFalse Info)),
            TyBool)