module DoubutsuShogi.Type where

import Array (Array)

type Pos = OnBoard (Int,Int) | InHand Player Int
type Player = P1 | P2
type KomaType = Lion | Elephant | Giraffe | Chick | Chicken
type alias StateAt = Maybe (KomaType, Player)
type Effect = NoEffect | Transparent
type alias Cel = (Pos, StateAt, Effect)
type alias Board = List Cel
type alias KomaDai = Array KomaType
type GameResult = Unfinished | Win Player | Draw
type PlayState = Neutral | Selected
type alias GameState = {
    board : Board
  , turn : Player
  , playState : PlayState
  , result : GameResult
  , clickedStateAt : StateAt
  , clickedPosition : Maybe Pos
  , movablePositions : List Pos
  , mochiGoma1 : KomaDai
  , mochiGoma2 : KomaDai
  }
