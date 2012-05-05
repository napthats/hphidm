module ClientManager
       (
         resolveClientMessages,
       ) where

import qualified Data.Map as Map
import qualified Network.SimpleTCPServer as NS
--import qualified PlayerCharacter as PC hiding (makePlayerCharacter)
import qualified PlayerCharacterDB as PCD
import qualified Chara as CH
import qualified PhiMap as PM
import qualified ProtocolDecoder as PD
import qualified ProtocolEncoder as PE
import qualified DmMessages as DM
import qualified PhiWorld as PW


resolveClientMessages ::
  NS.SimpleTCPServer -> PW.PhiWorld -> PCD.PlayerCharacterDB -> IO [PW.ActionResult]
resolveClientMessages server phiworld pcdb = do
  msg_list <- NS.getEachClientMessages server
  let protocol_list = map (\(cid, msg) -> (cid, PD.decodeClientMessage msg)) msg_list
  return $ concat $ map (\(cid, p) -> executeClientProtocol phiworld pcdb cid p) protocol_list

-- returned results have to be excuted in an order of the list
executeClientProtocol ::
  PW.PhiWorld -> PCD.PlayerCharacterDB -> NS.ClientID -> PD.ClientProtocol -> [PW.ActionResult]
executeClientProtocol world pcdb cid protocol =
  let phimap = PW.getPhiMap world in
  let cidset = PW.getClientIDSet world in
  let pcset = PW.getPcSet world in
  let maybe_pc = case lookup cid cidset of 
        Nothing -> Nothing
        Just phirc -> case Map.lookup phirc pcset of
          Nothing -> error "PlayerCharacter is unregistered in PcSet"
          Just pc -> Just pc
  in case maybe_pc of
    Nothing -> case protocol of
      PD.Open phirc -> case Map.lookup phirc pcset of
        Just _ -> [PW.MessageFromDm cid $ DM.makeDmMessage DM.AccessAlready,
                   PW.MessageFromDm cid $ DM.makeDmMessage DM.ChangeClientFail,
                   PW.MessageFromDm cid $ PE.encodeProtocol PE.X,
                   PW.ForceDisconnect cid]
        Nothing -> case PCD.loadPc pcdb phirc of
          Nothing -> [PW.MessageFromDm cid $ DM.makeDmMessage DM.NoCharacter,
                      PW.MessageFromDm cid $ PE.encodeProtocol PE.X,
                      PW.ForceDisconnect cid]
          Just new_pc -> [PW.NewPc cid phirc new_pc]
      -- ignore world trans
      _ -> []
    Just pc -> case protocol of
      PD.Go dir -> let maybe_modified_pc = CH.walk phimap dir pc in
                   case maybe_modified_pc of
                     Nothing -> [PW.MessageFromDm cid $ DM.makeDmMessage DM.GoNo]
                     Just modified_pc ->
                       [PW.PcStatusChange PW.PSCPosition modified_pc]
      PD.Turn maybe_dir -> case maybe_dir of
        Just dir -> let modified_pc = CH.turn dir pc in
                    [PW.PcStatusChange PW.PSCDirection modified_pc]
        Nothing -> [PW.MessageFromDm cid $ DM.makeDmMessage DM.TurnBad]
      PD.RawMessage msg ->
        let visible_pos_list = PM.getVisiblePositions PM.All phimap
                               (CH.getPosition pc) (CH.getDirection pc) sightWidth sightHeight
        in let visible_chara_list = CH.getCharaInRegion visible_pos_list (Map.elems pcset) in
        map (\(_, _, vchara) -> PW.MessageFromPc pc vchara msg) visible_chara_list
      PD.Exit -> [PW.MessageFromDm cid $ DM.makeDmMessage DM.Savedata,
                  PW.MessageFromDm cid $ DM.makeDmMessage DM.Seeyou,
                  PW.MessageFromDm cid $ PE.encodeProtocol PE.Close,
                  PW.LogoutPc cid]
      PD.Hit -> [PW.PcHit pc]
      PD.Open _ -> []
      PD.UnknownProtocol -> []

-- tentative
sightWidth :: Int
sightWidth = 7
sightHeight :: Int
sightHeight = 7
