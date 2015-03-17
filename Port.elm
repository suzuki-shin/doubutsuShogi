module Port where

import Array as A
import Array (Array)
import List as L
import Type (..)

type alias ExPos = {typ : String, label : String, x : Maybe Int, y : Maybe Int, player : Maybe ExPlayer, n : Maybe Int}
type alias ExPlayer = {typ : String, label : String}
type alias ExKomaType = {typ : String, label : String}
type alias ExStateAt = {typ : String, komaType : Maybe ExKomaType, player : Maybe ExPlayer}
type alias ExEffect = {typ : String, label : String}
type alias ExCel = {typ : String, pos : ExPos, stateAt : ExStateAt, effect : ExEffect}
type alias ExBoard = List ExCel
type alias ExKomaDai = Array ExKomaType
type alias ExGameResult = {typ : String, label : String, player : Maybe ExPlayer}
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

-- type Pos = OnBoard (Int,Int) | InHand Player Int
toExPos : Pos -> ExPos
toExPos pos = case pos of
  OnBoard (x,y) -> {typ = "Pos", label = "OnBoard", x = Just x, y = Just y, player = Nothing, n = Nothing}
  InHand pl n -> {typ = "Pos", label = "InHand", x = Nothing, y = Nothing, player = Just (toExPlayer pl), n = Just n}

-- type Player = P1 | P2
toExPlayer : Player -> ExPlayer
toExPlayer pl = case pl of
   P1 -> {typ = "Player", label = "P1"}
   P2 -> {typ = "Player", label = "P2"}

-- type KomaType = Lion | Elephant | Giraffe | Chick | Chicken
toExKomaType : KomaType -> ExKomaType
toExKomaType kt = case kt of
  Lion -> {typ = "KomaType", label = "Lion"}
  Elephant -> {typ = "KomaType", label = "Elephant"}
  Giraffe -> {typ = "KomaType", label = "Giraffe"}
  Chick -> {typ = "KomaType", label = "Chick"}
  Chicken -> {typ = "KomaType", label = "Chicken"}

-- type alias StateAt = Maybe (KomaType, Player)
toExStateAt : StateAt -> ExStateAt
toExStateAt st = case st of
  Just (kt, pl) -> {typ = "StateAt", komaType = Just (toExKomaType kt), player = Just (toExPlayer pl)}
  Nothing       -> {typ = "StateAt" , komaType = Nothing, player = Nothing}

-- type Effect = NoEffect | Transparent
toExEffect : Effect -> ExEffect
toExEffect ef = case ef of
  NoEffect -> {typ = "Effect", label = "NoEffect"}
  Transparent -> {typ = "Effect", label = "Transparent"}

-- type alias Cel = (Pos, StateAt, Effect)
toExCel : Cel -> ExCel
toExCel (p,s,e) = {typ = "Cel", pos = toExPos p, stateAt = toExStateAt s, effect = toExEffect e}

-- type alias Board = List Cel
toExBoard : Board -> ExBoard
toExBoard = L.map toExCel

-- type alias KomaDai = Array KomaType
toExKomaDai : KomaDai -> ExKomaDai
toExKomaDai = A.map toExKomaType

-- type GameResult = Unfinished | Win Player | Draw
toExGameResult : GameResult -> ExGameResult
toExGameResult gr = case gr of
  Unfinished -> {typ = "GameResult", label = "Unfinished", player = Nothing}
  Win pl -> {typ = "GameResult", label = "Unfinished", player = Just (toExPlayer pl)}
  Draw -> {typ = "GameResult", label = "Unfinished", player = Nothing}

-- type PlayState = Neutral | Selected
toExPlayState : PlayState -> ExPlayState
toExPlayState ps = case ps of
  Neutral -> {typ = "PlayState", label = "Neutral"}
  Selected -> {typ = "PlayState", label = "Selected"}

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
-- type alias ExGameState = {
--     typ : String
--   , board : ExBoard
--   , turn : ExPlayer
--   , playState : ExPlayState
--   , result : ExGameResult
--   , clickedStateAt : ExStateAt
--   , clickedPosition : Maybe ExPos
--   , movablePositions : List ExPos
--   , mochiGoma1 : ExKomaDai
--   , mochiGoma2 : ExKomaDai
--   }
