{-# LANGUAGE OverloadedLists #-}
module SingleFrameIOSpec where

import Test.Hspec
import Linear
import Data.Either
import qualified Data.ByteString.Lazy as BS

import Bio.Motions.SingleFrameIO

import Bio.Motions.Representation.Dump
import Bio.Motions.Types

dump :: Dump
dump = Dump
    { dumpBinders =
          [   BinderInfo (V3 0 1 2) bi0
          ,   BinderInfo (V3 0 1 3) bi0
          ,   BinderInfo (V3 5 5 5) bi1
          ]
    , dumpChains =
          [ [ DumpBeadInfo (V3 0 1 1) ev0
            , DumpBeadInfo (V3 5 6 6) ev1
            , DumpBeadInfo (V3 5 5 6) ev0
            ]
          , [ DumpBeadInfo (V3 0 0 2) ev0
            , DumpBeadInfo (V3 5 4 5) ev1
            ]
          , [ DumpBeadInfo (V3 7 7 7) ev0
            , DumpBeadInfo (V3 7 8 8) ev0
            ]
          ]
    }
  where
    [bi0, bi1] = map BinderType [0, 1]
    (ev0, ev1) = ([1, 0], [0, 1000])

singleFrame :: SingleFrame
singleFrame = makeSingleFrame
    "name" -- Simulation name
    "description" -- Simulation description
    ["binder_x", "binder_y"] -- Binder types names
    ["chain1", "chain2", "chain3"] -- Chains names
    ([], []) -- Callbacks
    123 -- Step Counter
    dump -- Dump

spec :: Spec
spec = context "when serialising and deserialising single frame" $ do
    let deserialisedOrError = deserialiseSingleFrame $ serialiseSingleFrame singleFrame
    runIO $ BS.writeFile "/tmp/singleFrame.bytes" $ serialiseSingleFrame singleFrame
    it "doesn't result in an error" $
       deserialisedOrError `shouldSatisfy` isRight
    let (Right deserialised) = deserialisedOrError
    it "doesn't change the frame's dump" $
        getDump deserialised `shouldBe` Just dump
    it "doesn't change the frames's name" $
        getSimulationName deserialised `shouldBe` Just "name"
    it "doesn't change the frame's description" $
        getSimulationDescription deserialised `shouldBe` Just "description"
    it "doesn't change binder types names" $
        getBinderTypesNames deserialised `shouldBe` Just ["binder_x", "binder_y"]
    it "doesn't change chain names" $
        getChainNames deserialised `shouldBe` Just ["chain1", "chain2", "chain3"]
