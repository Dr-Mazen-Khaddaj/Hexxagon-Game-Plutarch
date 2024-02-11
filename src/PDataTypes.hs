{-# OPTIONS_GHC -Wno-missing-export-lists #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module PDataTypes where

import Plutarch.DataRepr (PDataRecord, PLabeledType ((:=)), PlutusTypeData, DerivePConstantViaData (DerivePConstantViaData), PDataFields)
import Plutarch.Prelude (Term, S, Generic, PlutusType, PIsData, PEq, PShow, DerivePlutusType,DPTStrat, PInteger, PlutusTypeNewtype, (#==), pto, PBuiltinList)
import Plutarch.Api.V2 (PCurrencySymbol, PTokenName, PMap, KeyGuarantees (Unsorted), PPOSIXTime)
import Plutarch.Lift (PUnsafeLiftDecl (..), PConstantDecl, DerivePConstantViaNewtype (DerivePConstantViaNewtype))
import DataTypes (Player, Hexagon, Position, Board (..), Move, GameSettings, GameState, GameInfo, Initialization, RunGame)
import Instances ()

{- PPlayer -}
data PPlayer (s :: S)   = PRedPlayer    (Term s ( PDataRecord   [ "symbol"  ':= PCurrencySymbol
                                                                , "name"    ':= PTokenName
                                                                ] ))
                        | PBluePlayer   (Term s ( PDataRecord   [ "symbol"  ':= PCurrencySymbol
                                                                , "name"    ':= PTokenName
                                                                ] ))
                        deriving stock (Generic)
                        deriving anyclass (PlutusType, PIsData, PEq, PShow)

instance DerivePlutusType PPlayer where type DPTStrat _         = PlutusTypeData
instance PUnsafeLiftDecl  PPlayer where type PLifted PPlayer    = Player

deriving via (DerivePConstantViaData Player PPlayer)
    instance (PConstantDecl Player)

{- PHexagon -}
data PHexagon (s :: S)  = PEmpty    (Term s (PDataRecord '[]))
                        | PRed      (Term s (PDataRecord '[]))
                        | PBlue     (Term s (PDataRecord '[]))
                        deriving stock (Generic)
                        deriving anyclass (PlutusType, PIsData, PEq, PShow)

instance DerivePlutusType PHexagon where type DPTStrat _        = PlutusTypeData
instance PUnsafeLiftDecl  PHexagon where type PLifted PHexagon  = Hexagon

deriving via (DerivePConstantViaData Hexagon PHexagon)
    instance (PConstantDecl Hexagon)

{- PPosition -}
data PPosition (s :: S) = PPosition (Term s ( PDataRecord   [ "getX" ':= PInteger
                                                            , "getY" ':= PInteger
                                                            ] ))
                        deriving stock (Generic)
                        deriving anyclass (PlutusType, PIsData, PEq, PShow)

instance DerivePlutusType PPosition where type DPTStrat _        = PlutusTypeData
instance PUnsafeLiftDecl  PPosition where type PLifted PPosition = Position

deriving via (DerivePConstantViaData Position PPosition)
    instance (PConstantDecl Position)

{- PBoard -}
newtype PBoard (s :: S) = PBoard (Term s (PMap 'Unsorted PPosition PHexagon))
                        deriving stock (Generic)
                        deriving anyclass (PlutusType, PIsData, PShow)

instance DerivePlutusType PBoard where type DPTStrat _      = PlutusTypeNewtype
instance PUnsafeLiftDecl  PBoard where type PLifted PBoard  = Board

deriving via (DerivePConstantViaNewtype Board PBoard (PMap 'Unsorted PPosition PHexagon))
    instance (PConstantDecl Board)

instance (PEq (PMap 'Unsorted PPosition PHexagon))
instance PEq PBoard where a #== b = pto a #== pto b

{- PMove -}
data PMove (s :: S) = PMove (Term s ( PDataRecord   [ "initialPosition" ':= PPosition
                                                    , "finalPosition"   ':= PPosition
                                                    ] ))
                        deriving stock (Generic)
                        deriving anyclass (PlutusType, PIsData, PEq, PShow)

instance DerivePlutusType PMove where type DPTStrat _       = PlutusTypeData
instance PUnsafeLiftDecl  PMove where type PLifted PMove    = Move

deriving via (DerivePConstantViaData Move PMove)
    instance (PConstantDecl Move)

{- PGameSettings -}
data PGameSettings (s :: S) = PSettings (Term s ( PDataRecord   [ "player1"         ':= PPlayer
                                                                , "turnDuration"    ':= PPOSIXTime
                                                                , "boardS0"         ':= PBoard
                                                                ] ))
                        deriving stock (Generic)
                        deriving anyclass (PlutusType, PDataFields, PIsData, PEq, PShow)

instance DerivePlutusType PGameSettings where type DPTStrat _            = PlutusTypeData
instance PUnsafeLiftDecl  PGameSettings where type PLifted PGameSettings = GameSettings

deriving via (DerivePConstantViaData GameSettings PGameSettings)
    instance (PConstantDecl GameSettings)

{- PGameState -}
data PGameState (s :: S) = PGame (Term s ( PDataRecord  [ "player'sTurn"    ':= PPlayer
                                                        , "deadline"        ':= PPOSIXTime
                                                        , "board"           ':= PBoard
                                                        ] ))
                        deriving stock (Generic)
                        deriving anyclass (PlutusType, PIsData, PEq, PShow)

instance DerivePlutusType PGameState where type DPTStrat _          = PlutusTypeData
instance PUnsafeLiftDecl  PGameState where type PLifted PGameState  = GameState

deriving via (DerivePConstantViaData GameState PGameState)
    instance (PConstantDecl GameState)

{- PGameInfo -}
data PGameInfo (s :: S) = PGameInfo (Term s ( PDataRecord   [ "players"         ':= PBuiltinList PPlayer
                                                            , "turnDuration"    ':= PPOSIXTime
                                                            , "gameState"       ':= PGameState
                                                            ] ))
                        deriving stock (Generic)
                        deriving anyclass (PlutusType, PIsData, PEq, PShow)

instance DerivePlutusType PGameInfo where type DPTStrat _           = PlutusTypeData
instance PUnsafeLiftDecl  PGameInfo where type PLifted PGameInfo    = GameInfo

deriving via (DerivePConstantViaData GameInfo PGameInfo)
    instance (PConstantDecl GameInfo)

{- PInitialization -}
data PInitialization (s :: S)   = PAdd      (Term s (PDataRecord '["player" ':= PPlayer]))
                                | PWithdraw (Term s (PDataRecord '[]))
                        deriving stock (Generic)
                        deriving anyclass (PlutusType, PIsData, PEq, PShow)

instance DerivePlutusType PInitialization where type DPTStrat _                 = PlutusTypeData
instance PUnsafeLiftDecl  PInitialization where type PLifted PInitialization    = Initialization

deriving via (DerivePConstantViaData Initialization PInitialization)
    instance (PConstantDecl Initialization)

{- PRunGame -}
data PRunGame (s :: S)  = PPlayTurn (Term s (PDataRecord '[]))
                        | PGameOver (Term s (PDataRecord '["player" ':= PPlayer]))
                        | PTimeOut  (Term s (PDataRecord '[]))
                        deriving stock (Generic)
                        deriving anyclass (PlutusType, PIsData, PEq, PShow)

instance DerivePlutusType PRunGame where type DPTStrat _        = PlutusTypeData
instance PUnsafeLiftDecl  PRunGame where type PLifted PRunGame  = RunGame

deriving via (DerivePConstantViaData RunGame PRunGame)
    instance (PConstantDecl RunGame)

----------------------------------------------------------------------------------------------------------------------------