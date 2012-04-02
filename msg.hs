module Msg (
  Msg,
  ClientId,
  getBroadcastId,
  getNewId,
  getNextId,
  getServerId,
  getIdNum
  )where

import ClientId

type Msg = (ClientId, ClientId, String)
