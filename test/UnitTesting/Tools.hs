module  UnitTesting.Tools   ( inputScript
                            , inputPubKey
                            , makeAssetClass
                            , samplePubKeyHash
                            , outputLovelace
                            , outputNFT
                            , outputValue
                            , makeTxId
                            , outputScript
                            , TestElement (..)
                            , Arguments
                            , applyArguments
                            , applyAllArgs
                            , testElement
                            ) where

import PlutusTx.Builtins
import PlutusLedgerApi.V2
import Plutarch.Context (Builder, SpendingBuilder, output, address, withValue, input, script, withRefTxId, withRefIndex, withRedeemer, withInlineDatum)
import PlutusLedgerApi.V1.Value (AssetClass, assetClass, assetClassValue)
import PlutusTx.Builtins.Class (stringToBuiltinByteString)
import PlutusTx.Prelude (dropByteString)
import Plutarch.Prelude
import Plutarch.Api.V2
import Plutarch.Test.Precompiled
import Test.Tasty

--------------------------------------------------- Testing Data Types

data TestElement    = Condition     {number :: Int, title :: String}
                    | Vulnerability {number :: Int, title :: String}

instance Show TestElement where
    show :: TestElement -> String
    show (Condition     n title) = "Test Condition "     <> show n <> " : " <> title
    show (Vulnerability n title) = "Test Vulnerability " <> show n <> " : " <> title

--------------------------------------------------- Arguments to TestTree - Helper functions

type Arguments = Either [Data] [Data]

testElement :: ClosedTerm PValidator -> (Int -> Int -> Maybe Arguments) -> TestElement -> TestTree
testElement validator getArguments testElem = tryFromPTerm (show testElem) validator . sequence_ $ applyAllArgs arguments
    where
        arguments = getArguments testElem.number <$> [1..]

applyAllArgs :: [Maybe Arguments] -> [TestCompiled ()]
applyAllArgs ((Just args) : restOfArgs) = applyArguments args : applyAllArgs restOfArgs
applyAllArgs _ = []

applyArguments :: Arguments -> TestCompiled ()
applyArguments (Right arguments) = testEvalCase "Pass " Success arguments
applyArguments (Left  arguments) = testEvalCase "Fail " Failure arguments

--------------------------------------------------- Inputs

inputScript :: ToData redeemer => ScriptHash -> redeemer -> Integer -> [AssetClass] -> Integer -> SpendingBuilder
inputScript scriptHash red lovelace nfts i = input $ mconcat $  [ script scriptHash
                                                                , withValue (singleton adaSymbol adaToken lovelace)
                                                                , withRefTxId . makeTxId $ show nfts <> show i
                                                                , withRefIndex i
                                                                , withRedeemer $ toData red
                                                                ] <>
                                                                [withValue (assetClassValue nft 1) | nft <- nfts]

inputPubKey :: (Builder a) => Address -> Integer -> [AssetClass] -> Integer -> a
inputPubKey addr lovelace nfts i = input $ mconcat $    [ address addr
                                                        , withValue (singleton adaSymbol adaToken lovelace)
                                                        , withRefTxId . makeTxId $ show addr <> show lovelace <> show nfts <> show i
                                                        , withRefIndex i
                                                        ] <>
                                                        [withValue (assetClassValue nft 1) | nft <- nfts]

--------------------------------------------------- Outputs

outputScript :: ToData datum => Address -> Integer -> [AssetClass] -> datum -> SpendingBuilder
outputScript addr lovelace nfts dat = output $ mconcat $    [ address addr
                                                            , withValue (singleton adaSymbol adaToken lovelace)
                                                            , withInlineDatum $ toData dat
                                                            ] <>
                                                            [withValue (assetClassValue nft 1) | nft <- nfts]

-------------------------------------------------- | Utility Functions | ---------------------------------------------------

makeAssetClass :: BuiltinByteString -> BuiltinByteString -> AssetClass
makeAssetClass symbol name = assetClass (CurrencySymbol symbol) (TokenName name)

samplePubKeyHash :: String -> PubKeyHash
samplePubKeyHash = PubKeyHash . dropByteString 4 . blake2b_256 . stringToBuiltinByteString

outputLovelace :: (Builder a) => Address -> Integer -> a
outputLovelace addr amount = output $ mconcat [address addr, withValue (singleton adaSymbol adaToken amount)]

outputNFT :: (Builder a) => Address -> AssetClass -> a
outputNFT addr nft = output $ mconcat [address addr, withValue (assetClassValue nft 1)]

outputValue :: Address -> Integer -> [AssetClass] -> SpendingBuilder
outputValue addr amount nfts = mconcat $ (outputLovelace addr amount) : [ outputNFT addr nft | nft <- nfts]

makeTxId :: String -> TxId
makeTxId = TxId . blake2b_256 . stringToBuiltinByteString

------------------------------------------------------  End of Code  -------------------------------------------------------