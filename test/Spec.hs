import Test.Hspec
import Test.QuickCheck
import Text.Parsec

import Eval
import Parser
import Syntax

import Control.Monad.State.Lazy

main :: IO ()
main = hspec $ do
    describe "parser" $ do
        it "can parse the SKK identity" $ do
            let src = "(\\x. (\\y. (\\z. x z (y z)))) (\\x. (\\y. x)) (\\x. (\\y . x))"
            property $ case parseExpr src of
                Right _ -> True
                Left _ -> False

    describe "evaluator" $ do
        it "reduces the SKK identity" $ do
            let src = "(\\x. (\\y. (\\z. x z (y z)))) (\\x. (\\y. x)) (\\x. (\\y . x))"
            let (Right expr) = parseExpr src
            let reduced = evalState (reduce expr) defaultStack
            reduced `shouldBe` (Lam "z" (Var "z"))
