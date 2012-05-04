import System.Time (getClockTime, ClockTime)
import System.Random (RandomGen, getStdGen)
import qualified Utils.MyDelay as UM
import qualified ClientManager as CM
import qualified Network.SimpleTCPServer as NS
import qualified PlayerCharacterDB as PCD
import qualified PhiWorld as PW
import qualified NonPlayerCharacterManager as NPCM


main :: IO ()
main = do
  server <- NS.runTCPServer 20017
  let world = PW.makePhiWorld
  let phimap = PW.getPhiMap world
  pcdb <- PCD.makePcDB phimap
  cur <- getClockTime
  gen <- getStdGen
  mainLoop server world pcdb cur gen

mainLoop ::
  RandomGen g => NS.SimpleTCPServer -> PW.PhiWorld -> PCD.PlayerCharacterDB -> ClockTime -> g -> IO ()
mainLoop server world pcdb pre gen = do
  pc_action_result_list <- CM.resolveClientMessages server world pcdb
  let (npc_action_result_list, next_gen) = NPCM.resolveNpcActions world millisecondsPerFrame gen 
  (new_world, new_pcdb) <-
    PW.resolveActionResult server (pc_action_result_list ++ npc_action_result_list) world pcdb
  cur <- getClockTime
  UM.delayForFramerate millisecondsPerFrame cur pre
  next_cur <- getClockTime
  mainLoop server new_world new_pcdb next_cur next_gen

millisecondsPerFrame :: Int
millisecondsPerFrame = 100
