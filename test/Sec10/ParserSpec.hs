module Sec10.ParserSpec (spec) where

import Sec10.SimplyTyped
import Sec10.Parser hiding (context)
import Test.Hspec
  
spec :: Spec
spec = do
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
    context "in 9.2.2. cases" $ do
      it "should parse (1)" $
        fromString "f:Bool→Bool⊢f(if false then true else false):Bool" `shouldBe`
          Just ([("f", VarBind (TyArr TyBool TyBool))], 
            NTmApp Info (NTmVar Info "f") 
              (NTmIf Info (NTmFalse Info) (NTmTrue Info) (NTmFalse Info)),
            TyBool)
      it "should parse (2)" $
        fromString "f:Bool→Bool⊢\\x:Bool.f(if x then false else x):Bool→Bool" `shouldBe`
        Just ([("f",VarBind (TyArr TyBool TyBool))],
          NTmAbs Info "x" TyBool (NTmApp Info (NTmVar Info "f") 
            (NTmIf Info (NTmVar Info "x") (NTmFalse Info) (NTmVar Info "x"))),
          TyArr TyBool TyBool)
  describe "removenames" $
    context "in 9.2.2. cases" $ do
      it "should parse (1)" $
        removenames ([("f", VarBind (TyArr TyBool TyBool))],
          NTmApp Info (NTmVar Info "f") 
            (NTmIf Info (NTmFalse Info) (NTmTrue Info) (NTmFalse Info)),
          TyBool)
          `shouldBe`
          TmApp Info
            (TmVar Info 0 1) 
            (TmIf Info (TmFalse Info) (TmTrue Info) (TmFalse Info))
      it "should parse (2)" $
        removenames ([("f", VarBind (TyArr TyBool TyBool))],
          NTmAbs Info "x" TyBool (NTmApp Info (NTmVar Info "f") 
            (NTmIf Info (NTmVar Info "x") (NTmFalse Info) (NTmVar Info "x"))),
          TyArr TyBool TyBool)
          `shouldBe`
          TmAbs Info "x" TyBool
            (TmApp Info
              (TmVar Info 1 2) 
              (TmIf Info (TmVar Info 0 2) (TmFalse Info) (TmVar Info 0 2)))
  describe "integration with SimplyTyped.typeof" $
    context "in 9.2.2. cases" $ do
      it "should return (1) Bool" $
        typecheck "f:Bool→Bool⊢f(if false then true else false):Bool"
          `shouldBe` Just True
      it "should return (2) Bool→Bool" $
        typecheck "f:Bool→Bool⊢\\x:Bool.f(if false then true else false):Bool→Bool"
          `shouldBe` Just True


typecheck :: String -> Maybe Bool
typecheck str = do
  judge@(ctx, _ntm, typ) <- fromString str
  let tm = removenames judge
  return $ typeof ctx tm == typ