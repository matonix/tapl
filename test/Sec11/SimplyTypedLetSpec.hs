module Sec11.SimplyTypedLetSpec (spec) where

import Sec11.SimplyTypedLet
import Test.Hspec

spec :: Spec
spec = do
  describe "SimplyTypedLet.typeof" $ do
    context "with 9.2.2. cases" $ do
      it "should return (1) Bool" $ 
        let
          ctx = [("f", VarBind (TyArr TyBool TyBool))]
          term = TmApp Info
            (TmVar Info 0 1) 
            (TmIf Info (TmFalse Info) (TmTrue Info) (TmFalse Info))
          ty = TyBool
        in typeof ctx term `shouldBe` ty
      it "should return (2) Bool -> Bool" $ 
        let
          ctx = [("f", VarBind (TyArr TyBool TyBool))]
          term = TmAbs Info "x" TyBool
            (TmApp Info
              (TmVar Info 1 2) 
              (TmIf Info (TmVar Info 0 2) (TmFalse Info) (TmVar Info 0 2)))
          ty = TyArr TyBool TyBool
        in typeof ctx term `shouldBe` ty
    context "with x:Bool⊢let x=true in if x then false else true" $
      it "should have type Bool" $
        let
          ctx = [("x", VarBind TyBool)]
          term = TmLet Info "x" (TmTrue Info) (TmIf Info 
            (TmVar Info 0 1) (TmFalse Info) (TmTrue Info))
          ty = TyBool
        in typeof ctx term `shouldBe` ty
  describe "SimplyTypedLet.eval" $ do
    context "with (λx:Bool. if x then false else x) true" $
      it "should return false" $ 
        let
          ctx = []
          term = 
            TmApp Info
              (TmAbs Info "x" TyBool 
                (TmIf Info (TmVar Info 0 1) (TmFalse Info) (TmVar Info 0 1)))
              (TmTrue Info)
          val = TmFalse Info
        in eval ctx term `shouldReturn` val
    context "with x:Bool⊢let x=true in if x then false else true" $
      it "should return false" $
        let
          ctx = [("x", VarBind TyBool)]
          term = TmLet Info "x" (TmTrue Info) (TmIf Info 
            (TmVar Info 0 1) (TmFalse Info) (TmTrue Info))
          val = TmFalse Info
        in eval ctx term `shouldReturn` val