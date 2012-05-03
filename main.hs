import System.Time (getClockTime, ClockTime)
import qualified Utils.MyDelay as UM
import qualified ClientManager as CM
import qualified Network.SimpleTCPServer as NS
import qualified PlayerCharacterDB as PCD
import qualified PhiWorld as PW


main :: IO ()
main = do
  server <- NS.runTCPServer 20017
  let world = PW.makePhiWorld
  let phimap = case world of (x,_,_) -> x
  pcdb <- PCD.makePcDB phimap
  cur <- getClockTime
  mainLoop server world pcdb cur

mainLoop :: NS.SimpleTCPServer -> PW.PhiWorld -> PCD.PlayerCharacterDB -> ClockTime -> IO ()
mainLoop server world pcdb pre = do
  pc_action_result_list <- CM.resolveClientMessages server world pcdb
  (new_world, new_pcdb) <- PW.resolveActionResult server pc_action_result_list world pcdb
  cur <- getClockTime
  UM.delayForFramerate 100 cur pre
  mainLoop server new_world new_pcdb cur
