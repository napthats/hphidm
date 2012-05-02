import System.Time (getClockTime, ClockTime)
import qualified Utils.MyDelay as UM
import qualified ClientManager as CM
import qualified Network.SimpleTCPServer as NS
import qualified PlayerCharacterDB as PCD


main :: IO ()
main = do
  server <- NS.runTCPServer 20017
  let world = CM.makePhiWorld
  let phimap = case world of (x,_,_) -> x
  pcdb <- PCD.makePcDB phimap
  cur <- getClockTime
  mainLoop server world pcdb cur

mainLoop :: NS.SimpleTCPServer -> CM.PhiWorld -> PCD.PlayerCharacterDB -> ClockTime -> IO ()
mainLoop server world pcdb pre = do
  (new_world, new_pcdb) <- CM.resolveClientMessages server world pcdb
  cur <- getClockTime
  UM.delayForFramerate 100 cur pre
  mainLoop server new_world new_pcdb cur
