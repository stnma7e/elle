import Test.Hspec
import Test.QuickCheck
import Text.Parsec

import Eval
import Parser
import Syntax

import Control.Monad.State.Lazy

skkIdentity = "(\\x. (\\y. (\\z. x z (y z)))) (\\x. (\\y. x)) (\\x. (\\y . x))"

main :: IO ()
main = hspec $ do
    describe "parser" $ do
        it "can parse the SKK identity" $ do
            property $ case parseExpr skkIdentity of
                Right _ -> True
                Left _ -> False

    describe "evaluator" $ do
        it "reduces the SKK identity" $ do
            let (Right expr) = parseExpr skkIdentity
            let reduced = evalState (reduce expr) defaultStack
            reduced `shouldBe` (Lam "z" (Var "z"))
