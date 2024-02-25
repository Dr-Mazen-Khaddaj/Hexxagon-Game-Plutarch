module  RunGameSC   ( typedValidator
                    , validator
                    , script
                    , scriptHash
                    , scriptCredential
                    , address
                    ) where

import Plutarch.Prelude
import Plutarch.Api.V2.Contexts
import PDataTypes (PGameInfo(..), PRunGame (..), PMove, PBoard (PBoard), PHexagon (..), PPosition (..), PPlayer (..), PGameState (..))
import Plutarch.Api.V2 ( PStakingCredential, PAddress(..), PMaybeData(PDNothing), PValidator, PScriptHash, PMap (PMap), PExtended (PFinite) )
import Plutarch.Api.V1.Address (PCredential)
import PUtilities
import Plutarch.Script (Script)
import Plutarch.Extra.Field (pletAll)
import Plutarch.Monadic qualified as P
import Plutarch.Num ((#-), PNum (pabs), (#+))
import DataTypes (Hexagon(..))
import Plutarch.Builtin (ppairDataBuiltin)
import Plutarch.Maybe (pfromJust)

----------------------------------------------------------------------------------------------------------------------------
{-  Conditions (Play Turn) :
        1- Only 1 UTxO consumed from RunGameSC.
        2- Continuing Output: Own UTxO is sent to own smart contract.
        3- Value Preserved
        4- correct Datum:
            a- same players
            b- same turn duration
            c- new game state
        5- it's not deadline
        6- Player's registered NFT is found in the input UTxOs.
    Conditions (GameOver) : undefined
    Conditions (TimeOut) : undefined
-}
------------------------------------------------------ | Validator | -------------------------------------------------------

typedValidator :: Term s (PGameInfo :--> PRunGame :--> PScriptContext :--> PBool)
typedValidator = phoistAcyclic $ plam $ \ gameInfo runGame scriptContext ->
    pmatch runGame $ \case
        PPlayTurn ((pfield @"move" #) -> move) -> P.do
            scriptContext <- pletAll scriptContext
            txInfo <- pletFields @'["inputs", "outputs", "validRange"] scriptContext.txInfo
            let ownTxOutRef = pmatch scriptContext.purpose
                            $ \case PSpending txOutRef  -> pfield @"_0" # txOutRef
                                    _                   -> ptraceError "ScriptPurpose is not PSpending! @validator"
            ownAddress <- plet $ getOwnAddress # ownTxOutRef # txInfo.inputs

            let ownCredential = pfield @"credential" #$ ownAddress
                ownValue = getInputValue # ownCredential # txInfo.inputs                                                -- C1
                currentTime = pfromData $ pmatch (pfield @"_0" #$ pfield @"from" # txInfo.validRange)
                            $ \case PFinite a   -> pfield @"_0" # a
                                    _           -> ptraceError "Invalid LowerBound POSIXTime! @validator"

            PGameInfo gameInfo <- pmatch gameInfo
            gameInfo <- pletAll gameInfo
            PGame gameState <- pmatch gameInfo.gameState
            gameState <- pletAll gameState

            let player = gameState.player'sTurn
                board = gameState.board

            registeredNFT <- pletAll $ pmatch gameState.player'sTurn (\case PBluePlayer nft -> nft
                                                                            PRedPlayer  nft -> nft )

        -- Calculated variables
            let iHex = pmatch player (\case PRedPlayer _ -> pconstant Red ; PBluePlayer _ -> pconstant Blue)
            let cBoard = makeMove # iHex # move # board
                cNextPlayer = pfromJust #$ pfind # (plam $ (pnot #) . (#== player)) # gameInfo.players
                cNewDeadline = gameState.deadline #+ gameInfo.turnDuration
                cGameState = pcon . PGame    $ pdcons @"player'sTurn"   # pdata cNextPlayer
                                            #$ pdcons @"deadline"       # pdata cNewDeadline
                                            #$ pdcons @"board"          # pdata cBoard
                                            #  pdnil
                cGameInfo = pcon . PGameInfo $ pdcons @"players"        # gameInfo.players          -- C4.a
                                            #$ pdcons @"turnDuration"   # gameInfo.turnDuration     -- C4.b
                                            #$ pdcons @"gameState"      # pdata cGameState          -- C4.c
                                            #  pdnil
                cDatum = toPOutputDatum cGameInfo

        -- Proposed variables
            outputUTxO <- pletFields @'["value", "datum"] $ getOutputUTxO # ownAddress # txInfo.outputs                 -- C2

            pAnd'   [ outputUTxO.value #== ownValue                                                                     -- C3
                    , outputUTxO.datum #== cDatum                                                                       -- C4
                    , currentTime #<= gameState.deadline                                                                -- C5
                    , elemNFT # registeredNFT.symbol # registeredNFT.name # txInfo.inputs                               -- C6
                    ]

        PGameOver _player -> ptraceError "Undefined"
        PTimeOut _ -> ptraceError "Undefined"

--------------------------------------------------- | Helper Functions | ---------------------------------------------------

makeMove :: Term s (PHexagon :--> PMove :--> PBoard :--> PBoard)
makeMove = phoistAcyclic $ plam $ \ iHex move board -> P.do
    let d = distance # move
    move <- pletAll move
    iPos <- plet move.initialPosition
    fPos <- plet move.finalPosition
    nearbyPositions <- plet $ calculateNearbyPositions # pfromData fPos # 1

    let flipInitialPosition boardList = precList
            (\onBoard block rest -> P.do
                let pos = pfstBuiltin # block
                let hex = pfromData $ psndBuiltin # block
                let fHex = pif (d #== 1) (iHex) -- Distance of the Move is either 1 or 2, and calculate what the Hexagon should be replaced with.
                                         (pif (d #== 2) (pconstant Empty)
                                                        (ptraceError "Invalid Distance! @makeMove") )
                pif (pos #== iPos) -- Initial position is found.
                    (pif (hex #== iHex) -- Hexagon belong to right player.
                            (pcons  # (ppairDataBuiltin # pos # pdata fHex) # rest )
                            (ptraceError "Initial Hexagon does not belong to player! @makeMove") )
                    (pcons  # block #$ onBoard # rest)
            )
            (\_-> ptraceError "Can't find initial position! @makeMove")
            # boardList

    let flipNearbyPositions boardList = precList
            (\onBoard block rest -> P.do
                let pos = pfstBuiltin # block
                let hex = pfromData $ psndBuiltin # block
                pif (pelem # pos # nearbyPositions)
                    (pcons  # (ppairDataBuiltin # pos # (pdata $ flipHex # hex # iHex))
                            #$ onBoard # rest )
                    (pcons  # block #$ onBoard # rest)
            )
            (\_-> pnil)
            # boardList

    let flipFinalPosition boardList = precList
            (\onBoard block rest -> P.do
                let pos = pfstBuiltin # block
                let hex = pfromData $ psndBuiltin # block
                pif (pos #== fPos #&& hex #== pconstant Empty) -- This will check that the final position is present and Empty.
                    (pcons  # (ppairDataBuiltin # pos # pdata iHex)
                            # rest )
                    (pcons  # block #$ onBoard # rest)
            )
            (\_-> ptraceError "Final position is either not Empty or not available! @makeMove")
            # boardList

    pcon . PBoard . pcon . PMap . flipFinalPosition . flipNearbyPositions . flipInitialPosition $ pto . pto $ board

distance :: Term s (PMove :--> PInteger)
distance = phoistAcyclic $ plam $ \ move -> P.do
    move <- pletAll move
    iPos <- pletAll move.initialPosition
    fPos <- pletAll move.finalPosition
    let deltaX = (pfromData fPos.getX) #- (pfromData iPos.getX)
        deltaY = (pfromData fPos.getY) #- (pfromData iPos.getY)
    pdiv # (((pabs # deltaX) #+ (pabs # deltaY)) #+ (pabs # (deltaX #- deltaY)))
         # 2

flipHex :: Term s (PHexagon :--> PHexagon :--> PHexagon)
flipHex = phoistAcyclic $ plam $ \ oldHex newHex ->
    pmatch oldHex $ \ case
        PEmpty _ -> pconstant Empty
        _ -> newHex

calculateNearbyPositions :: Term s (PPosition :--> PInteger :--> PBuiltinList (PAsData PPosition))
calculateNearbyPositions = phoistAcyclic $ plam $ \ position distance -> P.do
    let deltas = pif (distance #== 1) (pconstant $ calculateDeltas 1)
                        (pif (distance #== 2) (pconstant $ calculateDeltas 2)
                                (ptraceError "Unacceptable Distance! @calculateNearbyPositions"))
    position <- pletAll position
    iPosX <- plet position.getX
    iPosY <- plet position.getY
    precList    (\self bltnPair restOfPairs -> P.do
                    let deltaX = pfstBuiltin # bltnPair
                        deltaY = psndBuiltin # bltnPair
                    let fPosX = pdata $ iPosX #+ deltaX
                        fPosY = pdata $ iPosY #+ deltaY
                    pcons # (pdata . pcon . PPosition $ pdcons @"getX" # fPosX #$ pdcons @"getY" # fPosY # pdnil)
                          # (self # restOfPairs)
                )
                (\_ -> pnil)
                # deltas
    where
        calculateDeltas :: Integer -> [(Integer,Integer)]
        calculateDeltas n = concat [s1,s2,s3,s4,s5,s6]
            where
                s1 = (n,) <$> [0..n]
                s2 = (,n) <$> [0..n-1]
                s3 = (,-n) <$> [0,-1..(-n)]
                s4 = (-n,) <$> [0,-1..(-n+1)]
                s5 = [ (i,i-n) | i <- [1..n-1]]
                s6 = [ (i-n,i) | i <- [1..n-1]]

---------------------------------------------------- | Serializations | ----------------------------------------------------

validator :: Term s PValidator
validator = pwrapValidator # typedValidator

script :: Script
script = validatorToScript validator

scriptHash :: Term s PScriptHash
scriptHash = hashScript script

scriptCredential :: Term s PCredential
scriptCredential = mkScriptCredential scriptHash

address :: Term s PAddress
address = mkAddress scriptCredential stakingCredential
    where
        stakingCredential :: Term s (PMaybeData PStakingCredential)
        stakingCredential = pcon $ PDNothing pdnil

------------------------------------------------------- End Of Code --------------------------------------------------------