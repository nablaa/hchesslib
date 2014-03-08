import Test.Hspec
import GameTests
import NotationTests
import BoardTests
import FENTests

main :: IO ()
main = do gameSpec
          notationSpec
          boardSpec
          fenSpec
