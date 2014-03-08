import Test.Hspec
import GameTests
import NotationTests
import BoardTests
import FENTests
import MoveTests
import PieceTests

main :: IO ()
main = do gameSpec
          notationSpec
          boardSpec
          fenSpec
          moveSpec
          pieceSpec
