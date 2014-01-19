import Test.Hspec

main :: IO ()
main = hspec $ do
        describe "Example Spec" $ do
          it "should pass" $ do
            (0 :: Int) `shouldBe` (0 :: Int)
