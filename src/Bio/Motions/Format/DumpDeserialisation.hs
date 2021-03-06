{- |
Module      : Bio.Motions.Format.DumpDeserialisation
Description : Deserialisation of Dump from a proto message
License     : Apache
Stability   : experimental
Portability : unportable
-}
{-# LANGUAGE RecordWildCards #-}
module Bio.Motions.Format.DumpDeserialisation where

import Control.Monad
import qualified Data.Map as M
import qualified Data.Vector.Unboxed as U
import Data.Foldable
import Linear
import Text.ProtocolBuffers.Basic

import qualified Bio.Motions.Format.Proto.Header as ProtoHeader
import qualified Bio.Motions.Format.Proto.Keyframe as ProtoKeyframe
import qualified Bio.Motions.Format.Proto.Keyframe.Binder as ProtoBinder
import Bio.Motions.Format.Proto.Delta
import Bio.Motions.Format.Proto.Keyframe.Chain
import Bio.Motions.Format.Proto.Point
import Bio.Motions.Format.Proto.Header.ChainDescription.BeadDescription.Binding
import Bio.Motions.Format.Proto.Header.ChainDescription.BeadDescription
import Bio.Motions.Format.Proto.Header.ChainDescription
import Bio.Motions.Representation.Dump
import Bio.Motions.Types

deserialiseMove :: Delta -> Maybe Move
deserialiseMove Delta{..} = do
    moveFrom <- from >>= readPosition
    moveDiff <- disp >>= readPosition
    return Move{..}

deserialiseDump :: ProtoHeader.Header -> ProtoKeyframe.Keyframe -> Maybe Dump
deserialiseDump header keyframe = do
    binderTypesCount <- fromIntegral <$> ProtoHeader.binders_types_count header
    dumpBinders <- mapM readBinder $ toList $ ProtoKeyframe.binders keyframe
    let chainPositions = toList $ ProtoKeyframe.chains keyframe
    let chainDescriptions = toList $ ProtoHeader.chains header
    dumpChains <- zipWithM (readChainBeads binderTypesCount) chainDescriptions chainPositions
    return Dump{..}

getChainNames :: ProtoHeader.Header -> Maybe [String]
getChainNames = mapM readChainName . toList . ProtoHeader.chains

getBinderTypesNames :: ProtoHeader.Header -> [String]
getBinderTypesNames = map uToString . toList . ProtoHeader.binder_types_names

readBinder :: ProtoBinder.Binder -> Maybe BinderInfo
readBinder ProtoBinder.Binder{..} = do
    binderType' <- BinderType . fromIntegral <$> binder_type
    position' <- position >>= readPosition
    return $ BinderInfo position' binderType'

readChainName :: ChainDescription -> Maybe String
readChainName ChainDescription{..} = uToString <$> chain_name

readChainBeads :: Int -> ChainDescription -> Chain -> Maybe [DumpBeadInfo]
readChainBeads binderTypesCount ChainDescription{..} Chain{..} =
    zipWithM (readBeadDescription binderTypesCount) (toList beads) (toList bead_positions)

readBeadDescription :: Int -> BeadDescription -> Point -> Maybe DumpBeadInfo
readBeadDescription binderTypesCount beadDescription position = do
    dumpBeadPosition <- readPosition position
    dumpBeadEV <- readEnergyVector binderTypesCount $ toList $ energy_vector beadDescription
    return DumpBeadInfo{..}

readEnergyVector :: Int -> [Binding] -> Maybe EnergyVector
readEnergyVector binderTypesCount bindings = do
    bindingsMap <- M.fromList <$> sequence [liftM2 (,) binder_type force | Binding{..} <- bindings]
    return $ EnergyVector $ U.fromList [fromIntegral $ M.findWithDefault 0 (fromIntegral i) bindingsMap
      | i <- [0..binderTypesCount - 1]]

readPosition :: Point -> Maybe Vec3
readPosition Point{..} = do
    [x', y', z'] <- mapM (fmap fromIntegral) [x, y, z]
    return $ V3 x' y' z'
