module NonPlayerCharacterManager
       (
         resolveNpcActions,
       ) where

import System.Random (RandomGen, randomR)
import qualified Data.Map as Map
import qualified PhiWorld as PW
import qualified PhiMap as PM
import qualified NonPlayerCharacter as NPC
import qualified Chara as CH


resolveNpcActions :: RandomGen g => PW.PhiWorld -> Int -> g -> ([PW.ActionResult], g)
resolveNpcActions world dtime gen =
  let npcset = PW.getNpcSet world in
  let npc_action_set = map (\npc -> (npc, NPC.chooseAction npc)) $ Map.elems (snd npcset) in
  foldr (\(npc, action) (list, g) ->
          let (result, new_g) = executeNpcAction world npc action dtime g in
          (result ++ list, new_g)) ([], gen) npc_action_set

executeNpcAction ::
  RandomGen g => PW.PhiWorld -> NPC.NonPlayerCharacter -> NPC.NpcAction -> Int-> g ->
  ([PW.ActionResult], g)
executeNpcAction world npc action dtime gen =
  let phimap = PW.getPhiMap world in
  if NPC.canActNext dtime npc
  then case action of
    NPC.RandomMove ->
      let (dir_ord, next_gen) = randomR (0, 3) gen in
      let change_npc =
            CH.walk phimap
            (PM.AbsoluteDirection $ [PM.North, PM.East, PM.West, PM.South] !! dir_ord) in
      ([PW.NpcStatusChange PW.NPSCPosition (NPC.getNpcId npc) change_npc], next_gen)
  else ([], gen)
