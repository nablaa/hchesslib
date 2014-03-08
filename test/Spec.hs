import Test.Hspec
import GameTests
import NotationTests
import BoardTests

main :: IO ()
main = do gameSpec
          notationSpec
          boardSpec
