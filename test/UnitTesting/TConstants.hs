module  UnitTesting.TConstants  ( alicePubKeyCredential
                                , bobPubKeyCredential
                                , aliceStakingCredential
                                , bobStakingCredential
                                , alicePaymentAddress
                                , aliceFullAddress
                                , bobPaymentAddress
                                , bobFullAddress
                                , compromisedAddress1
                                , compromisedAddress2
                                , compromisedRunGameSCAddress
                                , clayNFT
                                , bondNFT
                                , ticketNFT
                                , identifierNFT_A
                                , identifierNFT_B
                                , identifierNFT_C
                                , refNFT_A
                                , refNFT_B
                                , playerA
                                , playerB
                                , playerC
                                , playerX
                                , playerY
                                , turnDuration1ms
                                , turnDuration1h
                                , turnDuration7h
                                , currentTime2024Jan1
                                ) where

import PlutusLedgerApi.V2
import PlutusLedgerApi.V1.Value (AssetClass)
import UnitTesting.Tools
import DataTypes (Player (BluePlayer, RedPlayer))
import Plutarch.Prelude (plift)
import RunGameSC qualified
import UtilityFxs (bytesFromHex)

--------------------------------------------------- Addresses

alicePubKeyCredential, bobPubKeyCredential :: Credential
alicePubKeyCredential   = PubKeyCredential $ samplePubKeyHash "Alice"
bobPubKeyCredential     = PubKeyCredential $ samplePubKeyHash "Bob"

aliceStakingCredential, bobStakingCredential :: StakingCredential
aliceStakingCredential  = StakingHash . PubKeyCredential $ samplePubKeyHash "Alice_Stake"
bobStakingCredential    = StakingHash . PubKeyCredential $ samplePubKeyHash "Bob_Stake"

alicePaymentAddress, aliceFullAddress, bobPaymentAddress, bobFullAddress :: Address
alicePaymentAddress = Address alicePubKeyCredential Nothing
aliceFullAddress    = Address alicePubKeyCredential (Just aliceStakingCredential)
bobPaymentAddress   = Address bobPubKeyCredential   Nothing
bobFullAddress      = Address bobPubKeyCredential   (Just bobStakingCredential)

compromisedAddress1, compromisedAddress2 :: Address
compromisedAddress1 = Address alicePubKeyCredential (Just bobStakingCredential)
compromisedAddress2 = Address bobPubKeyCredential   (Just aliceStakingCredential)

compromisedRunGameSCAddress :: Address
compromisedRunGameSCAddress = Address (plift RunGameSC.scriptCredential) (Just bobStakingCredential)

--------------------------------------------------- NFTs
clayNFT,bondNFT, ticketNFT :: AssetClass

clayNFT     = makeAssetClass "currency-symbol-one" "clayNFT"
bondNFT     = makeAssetClass "currency-symbol-two" "bondNFT"
ticketNFT   = makeAssetClass "currency-symbol-two" "ticketNFT"

userNFTLabel, refNFTLabel :: BuiltinByteString
userNFTLabel = toBuiltin $ bytesFromHex "000de140"
refNFTLabel  = toBuiltin $ bytesFromHex "000643b0"

identifierNFT_A, identifierNFT_B, identifierNFT_C, refNFT_A, refNFT_B :: AssetClass

identifierNFT_A = makeAssetClass "currency-symbol-identifier-A" $ userNFTLabel <> "identifierNFT-A"
identifierNFT_B = makeAssetClass "currency-symbol-identifier-B" $ userNFTLabel <> "identifierNFT-B"
identifierNFT_C = makeAssetClass "currency-symbol-identifier-C" $ userNFTLabel <> "identifierNFT-C"
refNFT_A        = makeAssetClass "currency-symbol-identifier-A" $ refNFTLabel  <> "identifierNFT-A"
refNFT_B        = makeAssetClass "currency-symbol-identifier-B" $ refNFTLabel  <> "identifierNFT-B"

--------------------------------------------------- Players
playerA, playerB, playerC, playerX, playerY :: Player

playerA = BluePlayer (CurrencySymbol "currency-symbol-identifier-A") (TokenName $ userNFTLabel <> "identifierNFT-A")
playerB = RedPlayer  (CurrencySymbol "currency-symbol-identifier-B") (TokenName $ userNFTLabel <> "identifierNFT-B")
playerC = RedPlayer  (CurrencySymbol "currency-symbol-identifier-C") (TokenName $ userNFTLabel <> "identifierNFT-C")
playerX = RedPlayer  (CurrencySymbol "currency-symbol-identifier-A") (TokenName $ userNFTLabel <> "identifierNFT-A")
playerY = BluePlayer (CurrencySymbol "currency-symbol-identifier-B") (TokenName $ userNFTLabel <> "identifierNFT-B")

--------------------------------------------------- POSIXTime

turnDuration7h, turnDuration1h, turnDuration1ms :: POSIXTime
turnDuration7h = POSIXTime 25200000
turnDuration1h = POSIXTime 3600000
turnDuration1ms = POSIXTime 1

currentTime2024Jan1 :: POSIXTime
currentTime2024Jan1 = POSIXTime 1704067200000