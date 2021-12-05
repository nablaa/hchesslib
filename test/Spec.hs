import GameTests
import NotationTests
import BoardTests
import FENTests
import MoveTests
import PieceTests
import UITests

main :: IO ()
main = do gameSpec
          notationSpec
          boardSpec
          fenSpec
          moveSpec
          pieceSpec
          uiSpec
