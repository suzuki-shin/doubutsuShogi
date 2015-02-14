import List as L
import List ((::))
import Dict as D
import Array as A
import Text as T
import Graphics.Element (..)
import Graphics.Collage as GC
import Graphics.Input (clickable)
import Color (..)
import Mouse
import Signal (Signal, Channel, send, channel, subscribe, (<~), (~), foldp)

type alias Pos = (Int,Int) -- (x,y)
type Player = P1 | P2
type KomaType = Lion | Elephant | Giraffe | Chick
type alias StateAt = Maybe (KomaType, Player)
type Effect = NoEffect | Transparent
type alias Board = List (Pos, StateAt, Effect)
type GameResult = Unfinished | Win Player | Draw
type PlayState = Neutral | Selected
type alias GameState = {
    board : Board
  , turn : Player
  , playState : PlayState
  , result : GameResult
  , clickedStateAt : StateAt
  , clickedPosition : Pos
  , movablePositions : List Pos
  }

boardSize = {x = 3, y = 4}
komaSize = {x = 100, y = 100}

initBoard : Board
initBoard = [
    ((0,0), Just (Elephant, P2), NoEffect)
  , ((1,0), Just (Lion, P2), NoEffect)
  , ((2,0), Just (Giraffe, P2), NoEffect)
  , ((0,1), Nothing, NoEffect)
  , ((1,1), Just (Chick, P2), NoEffect)
  , ((2,1), Nothing, NoEffect)
  , ((0,2), Nothing, NoEffect)
  , ((1,2), Just (Chick, P1), NoEffect)
  , ((2,2), Nothing, NoEffect)
  , ((0,3), Just (Elephant, P1), NoEffect)
  , ((1,3), Just (Lion, P1), NoEffect)
  , ((2,3), Just (Giraffe, P1), NoEffect)
  ]

posMessage : Channel Pos
posMessage = channel (0,0)

show : (Pos, StateAt, Effect) -> Element
show (p, s, e) =
  let effect : Element -> Element
      effect = if | e == Transparent -> opacity 0.5
                  | otherwise -> identity

      showKoma : KomaType -> Player -> Element
      showKoma kt player = komaImg kt player |> fittedImage komaSize.x komaSize.y

      komaImg : KomaType -> Player -> String
      komaImg kt player =
          let ktImg = case kt of
                        Lion     -> "lion"
                        Elephant -> "zou"
                        Giraffe  -> "kirin"
                        Chick    -> "hiyoko"
              plImg = case player of
                        P1 -> "A"
                        P2 -> "B"
          in "img/" ++ ktImg ++ plImg ++ ".gif"

      emptyImg : Element
      emptyImg = fittedImage komaSize.x komaSize.y "img/y5.png"

  in clickable (send posMessage p) <| effect <| case s of
       Nothing -> emptyImg
       Just (kt, player) -> showKoma kt player


-- [((0,0), 'A'), ((1,0), 'B'), ((0,1), 'C'), ((1,1), 'D')] みたいな "Posが1要素目の2要素タプルのリスト" データを
-- [ ['A','B']
--  ,['C','D']] みたいな "二次元リスト" データに変換する
posKeyListTo2DList : Board -> List (List (Pos, StateAt, Effect))
posKeyListTo2DList tuples =
  let yMax : Int
      yMax = boardSize.y - 1
      yiList : Int -> List (Pos, StateAt, Effect) -> List (Pos, StateAt, Effect)
      yiList i = L.filter (\((_,y),_,_) -> y == i)
      tpl2nd3rd (_,b,c) = (b,c)
  in L.map (\yi -> (yiList yi) tuples ) [0..yMax]

fromListListStateAtToElement : List (List (Pos, StateAt, Effect)) -> Element
fromListListStateAtToElement = L.map (\cel -> flow right (L.map show cel)) >> flow down

-- 指定したマスの情報を返す
getAt : Board -> Pos -> (Pos, StateAt, Effect)
getAt b p = case L.filter (\(p',_,_) -> p' == p) b of
              [] -> (p, Nothing, NoEffect)
              [a] -> a
              otherwise -> (p, Nothing, NoEffect)

getStateAt : Board -> Pos -> StateAt
getStateAt b p = getAt b p |> (\(_,s,_) -> s)

-- 指定したPosの駒が動かせる場合、その移動可能範囲にエフェクトをつける
selected : Pos -> Board -> Board
selected p' b =
  let a : (Pos, StateAt, Effect)
      a = getAt b p'
      pos = (\(p,_,_) -> p) a
      st = (\(_,s,_) -> s) a
      movablePos' : List Pos
      movablePos' = movablePos b (p', st)
      effectedPoss : Pos -> Board -> List Pos
      effectedPoss p' b = pos :: movablePos'
  in if | (isOwn P1 b p') && (not (L.isEmpty movablePos'))
            -> L.map (\(p,s,e) -> if L.member p (effectedPoss p' b) then (p,s,Transparent) else (p,s,e)) b
        | otherwise -> b

-- 選択状態を解除する
cancelSelect : Board -> Board
cancelSelect = L.map (\(p,s,e) -> (p,s, NoEffect))

click : GameState -> Pos -> GameState
click gs p = if | gs.playState == Neutral -> { gs | board <- selected p gs.board
                                                   , playState <- Selected }
                | gs.playState == Selected -> { gs | turn <- opponent gs.turn
                                                   , playState <- Neutral }

initGameState : GameState
initGameState = { board = initBoard
                 , turn = P1
                 , playState = Neutral
                 , result = Unfinished
                 , clickedStateAt = Nothing
                 , clickedPosition = (0,4)
                 , movablePositions = []}

gameState : Signal GameState
gameState = foldp updateGameState initGameState (subscribe posMessage)

main =
  let
      a : Board -> Element
      a b = b |> posKeyListTo2DList |> fromListListStateAtToElement
      view : GameState -> Element
      view gs = flow down [
           a gs.board
--          , T.asText (selected p board)
--        , T.asText (possesOf P1 board)
--        , T.asText (movablePos board ((1,2), Just Giraffe P1))
--          , T.asText p
         , T.asText gs
         ]
  in view <~ gameState

-- そのPosが盤上かどうかを返す
isOnBoard : Board -> Pos -> Bool
isOnBoard b (x,y) =   (0 <= x) && (x <= boardSize.x - 1)
                    && (0 <= y) && (y <= boardSize.y - 1)


-- 指定した駒の動けるマスのPosのリストを返す
movablePos : Board -> (Pos, StateAt) -> List Pos
movablePos b ((x,y), s) =
  let filterFunc : Player -> Pos -> Bool
      filterFunc pl pos = (isOnBoard b pos) && not (L.member pos (possesOf pl b))
  in case s of
       Nothing -> [(x,y)]
       Just (kt, pl) -> case (kt, pl) of
         (Lion, p) -> [(x-1,y),(x,y-1),(x+1,y),(x,y+1),(x+1,y+1),(x+1,y-1),(x-1,y+1),(x-1,y-1)] |> L.filter (filterFunc p)
         (Elephant, p) -> [(x+1,y+1),(x-1,y+1),(x-1,y-1),(x+1,y-1)] |> L.filter (filterFunc p)
         (Giraffe, p) -> [(x,y+1),(x,y-1),(x+1,y),(x-1,y)] |> L.filter (filterFunc p)
         (Chick, p) -> [(x,if p == P1 then y-1 else y+1)] |> L.filter (filterFunc p)

-- 指定したプレイヤーの駒が占めているPosのリストを返す
possesOf : Player -> Board -> List Pos
possesOf player = L.filter (\(_, stateAt, _) -> case stateAt of
                                                  Just (_, p) -> p == player
                                                  otherwise -> False) >> L.map (\(a,_,_) -> a)

-- 指定したPosに自分の駒があるかどうかを返す
isOwn : Player -> Board -> Pos -> Bool
isOwn player board pos = L.member pos (possesOf player board)


updateGameState : Pos -> GameState -> GameState
updateGameState pos gs =
    let stAt : StateAt
        stAt = getAt (gs.board) pos |> (\(_,s,_) -> s)
        mPoss : List Pos
        mPoss = movablePos gs.board (pos, stAt)
        isSelect : Bool
        isSelect = (gs.playState == Neutral) && (isOwn gs.turn gs.board pos)
        isMove : Bool
        isMove = (gs.playState == Selected) && (L.member pos gs.movablePositions)
    in if | isMove -> { gs | playState <- Neutral
                        , result <- if getStateAt gs.board pos == Just (Lion ,opponent gs.turn) then Win gs.turn else Unfinished
                        , board <- updateBoard gs.board [
                                     (gs.clickedPosition, Nothing, NoEffect)
                                   , (pos, gs.clickedStateAt, NoEffect)]
                        , turn <- (opponent gs.turn)
                        , clickedPosition <- pos
                        , movablePositions <- [] }
          | isSelect -> { gs | board <- selected pos gs.board
                      , playState <- Selected
                      , clickedStateAt <- getStateAt gs.board pos
                      , clickedPosition <- pos
                      , movablePositions <- mPoss }
          | otherwise -> { gs | board <- cancelSelect gs.board
                         , playState <- Neutral
                         , clickedStateAt <- Nothing
                         , clickedPosition <- pos
                         , movablePositions <- [] }

-- 対戦相手
opponent : Player -> Player
opponent p = if p == P1 then P2 else P1

boardToDict : Board -> D.Dict (Int, Int) (StateAt, Effect)
boardToDict b = L.map (\(p,s,e) -> (p,(s,e))) b |> D.fromList

boardFromDict : D.Dict (Int, Int) (StateAt, Effect) -> Board
boardFromDict d = D.toList d |> L.map (\(p, (s,e)) -> (p,s,e))

updateBoard : Board -> List (Pos, StateAt, Effect) -> Board
updateBoard b pses =
  let updateOnePos : Board -> (Pos, StateAt, Effect) -> Board
      updateOnePos b (p,s,e) = boardToDict b |> D.update p (\_ -> Just (s, e)) |> boardFromDict
  in L.foldl (\(p,s,e) b' -> updateOnePos b' (p,s,e)) b pses
