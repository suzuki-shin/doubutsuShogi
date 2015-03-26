module DoubutsuShogi.Port where

import Array as A
import Array (Array)
import List as L
import DoubutsuShogi.Type (..)
import Debug

type alias ExPos = {typ : String, label : String, x : Int, y : Int, player : ExPlayer, n : Int}
type alias ExPlayer = {typ : String, label : String}
type alias ExKomaType = {typ : String, label : String}
type alias ExStateAt = Maybe {typ : String, komaType : ExKomaType, player : ExPlayer}
type alias ExEffect = {typ : String, label : String}
type alias ExCel = {typ : String, pos : ExPos, stateAt : ExStateAt, effect : ExEffect}
type alias ExBoard = List ExCel
type alias ExKomaDai = Array ExKomaType
type alias ExGameResult = {typ : String, label : String, player : ExPlayer}
type alias ExPlayState = {typ : String, label : String}
type alias ExGameState = {
    typ : String
  , board : ExBoard
  , turn : ExPlayer
  , playState : ExPlayState
  , result : ExGameResult
  , clickedStateAt : ExStateAt
  , clickedPosition : Maybe ExPos
  , movablePositions : List ExPos
  , mochiGoma1 : ExKomaDai
  , mochiGoma2 : ExKomaDai
  }

toExPos : Pos -> ExPos
toExPos pos = case pos of
  OnBoard (x,y) -> {typ = "Pos", label = "OnBoard", x = x, y = y, player = {typ = "Player", label = "Nothing"}, n = -1}
  InHand pl n -> {typ = "Pos", label = "InHand", x = -1, y = -1, player = toExPlayer pl, n = n}

fromExPos : ExPos -> Pos
fromExPos ex = case ex.label of
  "OnBoard" -> OnBoard (ex.x, ex.y)
  "InHand"  -> InHand (fromExPlayer ex.player) ex.n

toExPlayer : Player -> ExPlayer
toExPlayer pl = case pl of
   P1 -> {typ = "Player", label = "P1"}
   P2 -> {typ = "Player", label = "P2"}

fromExPlayer : ExPlayer -> Player
fromExPlayer ex = if
    | ex.label == "P1" -> P1
    | ex.label == "P2" -> P2
    | otherwise -> Debug.crash "fromExPlayer error"

toExKomaType : KomaType -> ExKomaType
toExKomaType kt = case kt of
  Lion -> {typ = "KomaType", label = "Lion"}
  Elephant -> {typ = "KomaType", label = "Elephant"}
  Giraffe -> {typ = "KomaType", label = "Giraffe"}
  Chick -> {typ = "KomaType", label = "Chick"}
  Chicken -> {typ = "KomaType", label = "Chicken"}

fromExKomaType : ExKomaType -> KomaType
fromExKomaType ex =
    let a = ex -- Debug.log "fromExKomaType ex:" ex
    in if
    | ex.label == "Lion" -> Lion
    | ex.label == "Elephant" -> Elephant
    | ex.label == "Giraffe" -> Giraffe
    | ex.label == "Chick" -> Chick
    | ex.label == "Chicken" -> Chicken

toExStateAt : StateAt -> ExStateAt
toExStateAt st = case st of
  Just (kt, pl) -> Just {typ = "StateAt", komaType = toExKomaType kt, player =  toExPlayer pl}
  Nothing       -> Nothing

fromExStateAt : ExStateAt -> StateAt
fromExStateAt ex = case ex of
  Just st -> Just ((fromExKomaType st.komaType), (fromExPlayer st.player))
  Nothing -> Nothing

toExEffect : Effect -> ExEffect
toExEffect ef = case ef of
  NoEffect -> {typ = "Effect", label = "NoEffect"}
  Transparent -> {typ = "Effect", label = "Transparent"}

fromExEffect : ExEffect -> Effect
fromExEffect ex = if
    | ex.label == "NoEffect" -> NoEffect
    | ex.label == "Transparent" -> Transparent
    | otherwise -> Debug.crash "fromExEffect error"

toExCel : Cel -> ExCel
toExCel (p,s,e) = {typ = "Cel", pos = toExPos p, stateAt = toExStateAt s, effect = toExEffect e}

fromExCel : ExCel -> Cel
fromExCel ex = ((fromExPos ex.pos), (fromExStateAt ex.stateAt), (fromExEffect ex.effect))

toExBoard : Board -> ExBoard
toExBoard = L.map toExCel

fromExBoard : ExBoard -> Board
fromExBoard = L.map fromExCel

toExKomaDai : KomaDai -> ExKomaDai
-- toExKomaDai = A.map toExKomaType -- これだとなぜか Runtime error
toExKomaDai kd = if
    | kd == A.empty -> A.fromList []
    | otherwise -> A.map toExKomaType kd

fromExKomaDai : ExKomaDai -> KomaDai
fromExKomaDai = A.map fromExKomaType

toExGameResult : GameResult -> ExGameResult
toExGameResult gr = case gr of
  Unfinished -> {typ = "GameResult", label = "Unfinished", player = {typ = "Player", label = "Nothing"}}
  Win pl -> {typ = "GameResult", label = "Win", player = toExPlayer pl}
  Draw -> {typ = "GameResult", label = "Draw", player = {typ = "Player", label = "Nothing"}}

fromExGameResult : ExGameResult -> GameResult
fromExGameResult ex = if
    | ex.label == "Unfinished" -> Unfinished
    | ex.label == "Win" -> Win (fromExPlayer ex.player)
    | ex.label == "Draw" -> Draw

toExPlayState : PlayState -> ExPlayState
toExPlayState ps = case ps of
  Neutral -> {typ = "PlayState", label = "Neutral"}
  Selected -> {typ = "PlayState", label = "Selected"}

fromExPlayState : ExPlayState -> PlayState
fromExPlayState ex = if
    | ex.label == "Neutral" -> Neutral
    | ex.label == "Selected" -> Selected

toExGameState : GameState -> ExGameState
toExGameState gs = {
    typ = "GameState"
  , board = toExBoard gs.board
  , turn = toExPlayer gs.turn
  , playState = toExPlayState gs.playState
  , result = toExGameResult gs.result
  , clickedStateAt = toExStateAt gs.clickedStateAt
  , clickedPosition = case gs.clickedPosition of
                        Just pos -> Just (toExPos pos)
                        Nothing -> Nothing
  , movablePositions = L.map toExPos gs.movablePositions
  , mochiGoma1 = toExKomaDai gs.mochiGoma1
  , mochiGoma2 = toExKomaDai gs.mochiGoma2
  }

fromExGameState : ExGameState -> GameState
fromExGameState ex = {
    board = fromExBoard ex.board
  , turn = fromExPlayer ex.turn
  , playState = fromExPlayState ex.playState
  , result = fromExGameResult ex.result
  , clickedStateAt = fromExStateAt ex.clickedStateAt
  , clickedPosition = case ex.clickedPosition of
                        Just exPos -> Just (fromExPos exPos)
                        Nothing -> Nothing
  , movablePositions = L.map fromExPos ex.movablePositions
  , mochiGoma1 = fromExKomaDai ex.mochiGoma1
  , mochiGoma2 = fromExKomaDai ex.mochiGoma2
  }

