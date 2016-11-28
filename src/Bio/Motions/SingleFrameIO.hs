{- |
Module      : Bio.Motions.SingleFrameIO
Description : Tools for reading and writing chromatine state from IO
License     : Apache
Stability   : experimental
Portability : unportable
-}

{-# LANGUAGE RecordWildCards #-}

module Bio.Motions.SingleFrameIO where

import Control.Monad
import Text.ProtocolBuffers
import Data.ByteString.Lazy as BS

import Bio.Motions.Callback.Class
import Bio.Motions.Format.Proto.SimulationState
import Bio.Motions.Format.DumpDeserialisation as D
import Bio.Motions.Format.DumpSerialisation
import Bio.Motions.Representation.Dump
import Bio.Motions.Types

type SingleFrame = SimulationState

getDump :: SingleFrame -> Maybe Dump
getDump SimulationState{..} = join $ deserialiseDump <$> header <*> state

getChainNames :: SingleFrame -> Maybe [String]
getChainNames SimulationState{..} = D.getChainNames  =<< header

getBinderTypesNames :: SingleFrame -> Maybe [String]
getBinderTypesNames SimulationState{..} = D.getBinderTypesNames <$> header

getSimulationName :: SingleFrame -> Maybe String
getSimulationName SimulationState{..} = D.getSimulationName =<< header

getSimulationDescription :: SingleFrame -> Maybe String
getSimulationDescription SimulationState{..} = D.getSimulationDescription =<< header

makeSingleFrame ::
    String
    -- ^Simulation name
    -> String
    -- ^Simulation description
    -> [String]
    -- ^Binder types names
    -> [String]
    -- ^Chain names
    -> Callbacks
    -> StepCounter
    -> Dump
    -> SingleFrame
makeSingleFrame name desc binderNames chainNames callbacks steps dump = SimulationState{..}
  where
    header = Just $ getHeader name desc binderNames chainNames dump
    state = Just $ getKeyframe dump callbacks steps

serialiseSingleFrame :: SingleFrame -> ByteString
serialiseSingleFrame = messagePut

deserialiseSingleFrame :: ByteString  -> Either String SingleFrame
deserialiseSingleFrame str = fst <$> messageGet str

readSingleFrameFromIO :: IO (Either String SingleFrame)
readSingleFrameFromIO = deserialiseSingleFrame <$> BS.getContents

writeSingleFrameToIO :: ByteString -> IO ()
writeSingleFrameToIO = BS.putStr
