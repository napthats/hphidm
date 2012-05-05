import System.Time (getClockTime, ClockTime)
import System.Random (RandomGen, getStdGen)
import qualified Utils.MyDelay as UM
import qualified ClientManager as CM
import qualified Network.SimpleTCPServer as NS
import qualified PlayerCharacterDB as PCD
import qualified PhiWorld as PW
import qualified NonPlayerCharacterManager as NPCM
import qualified Event as EV

main :: IO ()
main = do
  server <- NS.runTCPServer 20017
  let world = PW.makePhiWorld
  let phimap = PW.getPhiMap world
  let swdb = EV.makeSwitchDB
  pcdb <- PCD.makePcDB phimap
  cur <- getClockTime
  gen <- getStdGen
  mainLoop server world pcdb swdb cur gen []

mainLoop ::
  RandomGen g =>
  NS.SimpleTCPServer -> PW.PhiWorld -> PCD.PlayerCharacterDB -> EV.SwitchDB ->
  ClockTime -> g -> [EV.TriggeredEvent] ->
  IO ()
mainLoop server world pcdb swdb pre_time gen event_list = do
  (next_world, next_pcdb, new_event_list_chained) <-
    PW.resolveActionResult server event_list world pcdb swdb
  
  pc_action_result_list <- CM.resolveClientMessages server next_world
  let (npc_action_result_list, next_gen) = NPCM.resolveNpcActions next_world millisecondsPerFrame gen 
  (pre_final_world, final_pcdb, new_event_list) <-
    PW.resolveActionResult server (pc_action_result_list++npc_action_result_list)
    next_world next_pcdb swdb
  
  let final_world = PW.addLivetimeAllNpc millisecondsPerFrame pre_final_world
  
  cur_time <- getClockTime
  UM.delayForFramerate millisecondsPerFrame cur_time pre_time
  next_cur_time <- getClockTime
  
  mainLoop server final_world final_pcdb swdb next_cur_time next_gen $
    new_event_list_chained ++ new_event_list


millisecondsPerFrame :: Int
millisecondsPerFrame = 100
