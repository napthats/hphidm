module PhiWorld
       (
         ActionResult(..),
         ClientIDSet(..),
         PcSet(..),
         PhiWorld,
         Phirc,
         resolveActionResult,
         makePhiWorld,
       ) where

import Data.List (find)
import Data.List.Utils (addToAL, delFromAL)
import qualified Network.SimpleTCPServer as NS
import qualified PlayerCharacter as PC hiding (makePlayerChara)
import qualified PlayerCharacterDB as PCD
import qualified Chara as CH
import qualified PhiMap as PM
import qualified DmMessages as DM


type Phirc = String
data ActionResult = NewPc NS.ClientID Phirc PC.PlayerCharacter
                  | PcStatusChange Phirc PC.PlayerCharacter
                  | MessageFromDm NS.ClientID String
                  | MessageFromPc PC.PlayerCharacter PC.PlayerCharacter String
--                          | NormalMessage PC.PlayerChara String
--                          | BroadcastMessage PC.PlayerChara String
                  | LogoutPc NS.ClientID
                  | ForceDisconnect NS.ClientID
                  deriving (Show)

type PhiWorld = (PM.PhiMap, ClientIDSet, PcSet)

-- for debug
makePhiWorld :: PhiWorld
makePhiWorld = (PM.makePhiMap, ClientIDSet [], PcSet [])

newtype ClientIDSet = ClientIDSet [(NS.ClientID, Phirc)] deriving (Show)
newtype PcSet = PcSet [(Phirc, PC.PlayerCharacter)] deriving (Show)

reverseLookUp :: Eq b => b -> [(a, b)] -> Maybe a
reverseLookUp value al = case find ((== value) . snd) al of
  Nothing -> Nothing
  Just (a, _) -> Just a


resolveActionResult ::
  NS.SimpleTCPServer -> [ActionResult] -> PhiWorld -> PCD.PlayerCharacterDB ->
  IO (PhiWorld, PCD.PlayerCharacterDB)
resolveActionResult server result_list phiworld first_pcdb = do
  let (final_world, final_pcdb, final_io_list) =
        foldl (\((phimap, ClientIDSet cidset, PcSet pcset), pcdb, io_list) result ->
                case result of
                  NewPc cid phirc pc ->
                    let new_cidset = addToAL cidset cid phirc in
                    let new_pcset = addToAL pcset phirc pc in
                    ((phimap, ClientIDSet new_cidset, PcSet new_pcset), pcdb, io_list)
                  PcStatusChange phirc pc ->
                    let new_pcset = addToAL pcset phirc pc in
                    ((phimap, ClientIDSet cidset, PcSet new_pcset), pcdb, io_list)
                  MessageFromDm cid msg ->
                    -- ignore disconnected client
                    let io = NS.sendMessageTo server cid msg in
                    ((phimap, ClientIDSet cidset, PcSet pcset), pcdb, io:io_list)
                  MessageFromPc opc dpc msg ->
                    case reverseLookUp (PC.getPhirc dpc) cidset of
                      Nothing -> error "Assertion error: pc doesn't have client."
                      Just dcid ->
                        let io = NS.sendMessageTo server dcid $
                                 DM.makeDmMessage (DM.PcMessage (CH.getName opc) msg)
                        in ((phimap, ClientIDSet cidset, PcSet pcset), pcdb, io:io_list)
                  LogoutPc cid ->
                    case lookup cid cidset of
                      -- already disconnected
                      Nothing -> ((phimap, ClientIDSet cidset, PcSet pcset), pcdb, io_list)
                      Just phirc ->
                        let io = NS.disconnectClient server cid in
                        let maybe_pc = lookup phirc pcset in
                        case maybe_pc of
                          Nothing -> error "Assertion error: client don't have pc."
                          Just pc -> 
                            let new_pcdb = PCD.savePc pcdb phirc pc in
                            let new_cidset = delFromAL cidset cid in
                            let new_pcset = delFromAL pcset phirc in
                            ((phimap, ClientIDSet new_cidset, PcSet new_pcset), 
                             new_pcdb, io:io_list)
                  ForceDisconnect cid ->
                    let io = NS.disconnectClient server cid in
                    ((phimap, ClientIDSet cidset, PcSet pcset), pcdb, io:io_list)
          ) (phiworld, first_pcdb, []) result_list
  mapM_ (\x -> do _ <- x; return ()) $ reverse final_io_list
  return (final_world, final_pcdb)
