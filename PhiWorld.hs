module PhiWorld
       (
         ActionResult(..),
         ClientIDSet,
         PcSet,
         PhiWorld(),
         Phirc,
         getPhiMap,
         getClientIDSet,
         getPcSet,
         resolveActionResult,
         makePhiWorld,
       ) where

import Data.List (find)
import Data.List.Utils (addToAL, delFromAL)
import qualified Network.SimpleTCPServer as NS
import qualified PlayerCharacter as PC hiding (makePlayerCharacter)
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

newtype PhiWorld = PhiWorld (PM.PhiMap, ClientIDSet, PcSet)

getPhiMap :: PhiWorld -> PM.PhiMap
getPhiMap (PhiWorld (phimap, _, _)) = phimap

getClientIDSet :: PhiWorld -> ClientIDSet
getClientIDSet (PhiWorld (_, cidset, _)) = cidset

getPcSet :: PhiWorld -> PcSet
getPcSet (PhiWorld (_, _, pcset)) = pcset


-- for debug
makePhiWorld :: PhiWorld
makePhiWorld = PhiWorld (PM.makePhiMap, [], [])

type ClientIDSet = [(NS.ClientID, Phirc)]
type PcSet = [(Phirc, PC.PlayerCharacter)]

reverseLookUp :: Eq b => b -> [(a, b)] -> Maybe a
reverseLookUp value al = case find ((== value) . snd) al of
  Nothing -> Nothing
  Just (a, _) -> Just a


resolveActionResult ::
  NS.SimpleTCPServer -> [ActionResult] -> PhiWorld -> PCD.PlayerCharacterDB ->
  IO (PhiWorld, PCD.PlayerCharacterDB)
resolveActionResult server result_list (PhiWorld phiworld) first_pcdb = do
  let (final_world, final_pcdb, final_io_list) =
        foldl (\((phimap, cidset, pcset), pcdb, io_list) result ->
                case result of
                  NewPc cid phirc pc ->
                    let new_cidset = addToAL cidset cid phirc in
                    let new_pcset = addToAL pcset phirc pc in
                    ((phimap, new_cidset, new_pcset), pcdb, io_list)
                  PcStatusChange phirc pc ->
                    let new_pcset = addToAL pcset phirc pc in
                    ((phimap, cidset, new_pcset), pcdb, io_list)
                  MessageFromDm cid msg ->
                    -- ignore disconnected client
                    let io = NS.sendMessageTo server cid msg in
                    ((phimap, cidset, pcset), pcdb, io:io_list)
                  MessageFromPc opc dpc msg ->
                    case reverseLookUp (PC.getPhirc dpc) cidset of
                      Nothing -> error "Assertion error: pc doesn't have client."
                      Just dcid ->
                        let io = NS.sendMessageTo server dcid $
                                 DM.makeDmMessage (DM.PcMessage (CH.getName opc) msg)
                        in ((phimap, cidset, pcset), pcdb, io:io_list)
                  LogoutPc cid ->
                    case lookup cid cidset of
                      -- already disconnected
                      Nothing -> ((phimap, cidset, pcset), pcdb, io_list)
                      Just phirc ->
                        let io = NS.disconnectClient server cid in
                        let maybe_pc = lookup phirc pcset in
                        case maybe_pc of
                          Nothing -> error "Assertion error: client don't have pc."
                          Just pc -> 
                            let new_pcdb = PCD.savePc pcdb phirc pc in
                            let new_cidset = delFromAL cidset cid in
                            let new_pcset = delFromAL pcset phirc in
                            ((phimap, new_cidset, new_pcset), 
                             new_pcdb, io:io_list)
                  ForceDisconnect cid ->
                    let io = NS.disconnectClient server cid in
                    ((phimap, cidset, pcset), pcdb, io:io_list)
          ) (phiworld, first_pcdb, []) result_list
  mapM_ (\x -> do _ <- x; return ()) $ reverse final_io_list
  return (PhiWorld final_world, final_pcdb)
