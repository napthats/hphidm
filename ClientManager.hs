module ClientManager
       (
         resolveClientMessages,
       ) where

import qualified Data.Map as Map
import qualified Network.SimpleTCPServer as NS
--import qualified PlayerCharacter as PC hiding (makePlayerCharacter)
import qualified PlayerCharacter as PC
import qualified Chara as CH
import qualified PhiMap as PM
import qualified ProtocolDecoder as PD
import qualified ProtocolEncoder as PE
import qualified DmMessages as DM
import qualified PhiWorld as PW
--
import Data.Maybe (fromJust)
--


resolveClientMessages ::
  NS.SimpleTCPServer -> PW.PhiWorld -> IO [PW.ActionResult]
resolveClientMessages server phiworld = do
  msg_list <- NS.getEachClientMessages server
  let protocol_list = map (\(cid, msg) -> (cid, PD.decodeClientMessage msg)) msg_list
  return $ concat $ map (\(cid, p) -> executeClientProtocol phiworld cid p) protocol_list

-- returned results have to be excuted in an order of the list
executeClientProtocol ::
  PW.PhiWorld -> NS.ClientID -> PD.ClientProtocol -> [PW.ActionResult]
executeClientProtocol world cid protocol =
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
      PD.SharpProtocol (PD.Open phirc) -> [PW.NewPc cid phirc]
      -- ignore world trans
      _ -> []
    Just pc ->
      case protocol of
        PD.SharpProtocol sprotocol ->
              case sprotocol of
                PD.Open _ -> []
                PD.UnknownProtocol -> []          
        PD.RawProtocol _ ->
          case PC.getState pc of
            PC.SelectList slaction ->
              case protocol of
                PD.SharpProtocol _ ->
                  undefined
                PD.RawProtocol rprotocol ->
                  case slaction of
                    PC.SLGet ->
                      case rprotocol of
                        PD.RawMessage msg -> [PW.GetItem (PW.Pc pc cid) (Just msg),
                                              PW.PcStatusChange PW.PSCState (PC.getPhirc pc)
                                              $ PC.setState PC.Command]
                        _ -> [PW.GetItem (PW.Pc pc cid) (Just ""),
                              PW.PcStatusChange PW.PSCState (PC.getPhirc pc) $ PC.setState PC.Command]
                    PC.SLPut ->
                      case rprotocol of
                        PD.RawMessage msg -> [PW.PutItem (PW.Pc pc cid) (Just msg),
                                              PW.PcStatusChange PW.PSCState (PC.getPhirc pc) 
                                              $ PC.setState PC.Command]
                        _ -> [PW.PutItem (PW.Pc pc cid) (Just ""),
                              PW.PcStatusChange PW.PSCState (PC.getPhirc pc) $ PC.setState PC.Command]
            PC.Command ->
              case protocol of
                PD.SharpProtocol _ ->
                  undefined
                PD.RawProtocol rprotocol ->
                  case rprotocol of
                    PD.Go dir ->
                      [PW.PcStatusChange PW.PSCPosition (PC.getPhirc pc) (CH.walk phimap dir)]
                    PD.Turn maybe_dir -> case maybe_dir of
                      Just dir -> [PW.PcStatusChange PW.PSCDirection (PC.getPhirc pc) (CH.turn dir)]
                      Nothing -> [PW.MessageFromDm cid $ DM.makeDmMessage DM.TurnBad]
                    PD.RawMessage msg ->
                      let visible_pos_list =PM.getVisiblePositions PM.All phimap
                                            (CH.getPosition pc) (CH.getDirection pc)
                                            sightWidth sightHeight
                      in let visible_chara_list =
                               CH.getCharaInRegion visible_pos_list (Map.elems pcset) in
                      map (\(_, _, vchara) -> PW.MessageFromPc pc vchara msg) visible_chara_list
                    PD.Exit -> [PW.MessageFromDm cid $ DM.makeDmMessage DM.Savedata,
                                PW.MessageFromDm cid $ DM.makeDmMessage DM.Seeyou,
                                PW.MessageFromDm cid $ PE.encodeProtocol PE.Close,
                                PW.LogoutPc cid]
                    PD.Hit -> [PW.PcHit pc]
                    PD.Get maybe_item_name -> [PW.GetItem (PW.Pc pc cid) maybe_item_name]
                    PD.Put maybe_item_name -> [PW.PutItem (PW.Pc pc cid) maybe_item_name]

-- tentative
sightWidth :: Int
sightWidth = 7
sightHeight :: Int
sightHeight = 7
