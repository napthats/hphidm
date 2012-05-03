module ClientManager
       (
         resolveClientMessages,
       ) where

import Data.List.Utils (delFromAL)
import qualified Network.SimpleTCPServer as NS
import qualified PlayerCharacter as PC hiding (makePlayerChara)
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
executeClientProtocol (phimap, PW.ClientIDSet cidset, PW.PcSet pcset) pcdb cid protocol =
  let maybe_pc = case lookup cid cidset of 
        Nothing -> Nothing
        Just phirc -> case lookup phirc pcset of
          Nothing -> error "PlayerCharacter is unregistered in PcSet"
          Just pc -> Just pc
  in case maybe_pc of
    Nothing -> case protocol of
      PD.Open phirc -> case lookup phirc pcset of
        Just _ -> [PW.MessageFromDm cid $ DM.makeDmMessage DM.AccessAlready,
                   PW.MessageFromDm cid $ DM.makeDmMessage DM.ChangeClientFail,
                   PW.MessageFromDm cid $ PE.encodeProtocol PE.X,
                   PW.ForceDisconnect cid]
        Nothing -> case PCD.loadPc pcdb phirc of
          Nothing -> [PW.MessageFromDm cid $ DM.makeDmMessage DM.NoCharacter,
                      PW.MessageFromDm cid $ PE.encodeProtocol PE.X,
                      PW.ForceDisconnect cid]
          Just new_pc -> [PW.NewPc cid phirc new_pc] ++ makeLookResult cid phimap new_pc pcset cidset
      _ -> []
    Just pc -> case protocol of
      PD.Go dir -> let maybe_modified_pc = CH.walk phimap dir pc in
                   case maybe_modified_pc of
                     Nothing -> [PW.MessageFromDm cid $ DM.makeDmMessage DM.GoNo]
                     Just modified_pc ->
                       let phirc = case lookup cid cidset of
                             Nothing -> error "Assertion error"
                             Just x -> x
                       in [PW.PcStatusChange phirc modified_pc] ++
                          makeLookResult cid phimap modified_pc pcset cidset
      PD.Turn maybe_dir -> case maybe_dir of
        Just dir -> let modified_pc = CH.turn dir pc in
                    let phirc = case lookup cid cidset of
                          Nothing -> error "Assertion error"
                          Just x -> x
                    in [PW.PcStatusChange phirc modified_pc] ++
                       makeLookResult cid phimap modified_pc pcset cidset
        Nothing -> [PW.MessageFromDm cid $ DM.makeDmMessage DM.TurnBad]
      PD.RawMessage msg ->
        let visible_pos_list = PM.getVisiblePositions PM.All phimap
                               (CH.getPosition pc) (CH.getDirection pc) sightWidth sightHeight
        in let visible_chara_list = CH.getCharaInRegion visible_pos_list (map snd pcset) in
        map (\(_, _, vchara) -> PW.MessageFromPc pc vchara msg) visible_chara_list
      PD.Exit -> [PW.MessageFromDm cid $ DM.makeDmMessage DM.Savedata,
                  PW.MessageFromDm cid $ DM.makeDmMessage DM.Seeyou,
                  PW.MessageFromDm cid $ PE.encodeProtocol PE.Close,
                  PW.LogoutPc cid]
      PD.Open _ -> []
      PD.UnknownProtocol -> []

makeLookResult :: 
  NS.ClientID -> PM.PhiMap -> PC.PlayerCharacter ->
  [(PW.Phirc, PC.PlayerCharacter)] -> [(NS.ClientID, PW.Phirc)] -> [PW.ActionResult]
makeLookResult cid phimap pc pcset cidset =
  case lookup cid cidset of
    Nothing -> 
      let charaset = pc : map snd pcset in
      _makeLookResult cid phimap pc charaset
    Just phirc -> 
      let charaset = pc : map snd (delFromAL pcset phirc) in
      _makeLookResult cid phimap pc charaset
      
_makeLookResult ::
  (CH.Chara a) => NS.ClientID -> PM.PhiMap -> PC.PlayerCharacter -> [a] -> [PW.ActionResult]
_makeLookResult cid phimap pc charaset =
  [PW.MessageFromDm cid $ makeAroundView phimap pc] ++
  map (PW.MessageFromDm cid) (makeAroundCharaView phimap pc charaset) ++
  [PW.MessageFromDm cid $ PE.encodeProtocol PE.M57End]

makeAroundCharaView :: (CH.Chara a) => PM.PhiMap -> PC.PlayerCharacter -> [a] -> [String]
makeAroundCharaView phimap pc charaset =
  map (\(CH.CharaView x y rdir name) -> PE.encodeProtocol $ PE.M57Obj PE.CObj x y rdir name)
  (map (CH.getCharaView $ CH.getDirection pc) (CH.getCharaInRegion (CH.getSight phimap pc) charaset))

makeAroundView :: PM.PhiMap -> PC.PlayerCharacter -> String
makeAroundView phimap pc =
  PE.encodeProtocol $ PE.M57Map (CH.getDirection pc) $
  PM.getMapView PM.All phimap (CH.getPosition pc) (CH.getDirection pc) sightWidth sightHeight

-- tentative
sightWidth :: Int
sightWidth = 7
sightHeight :: Int
sightHeight = 7
