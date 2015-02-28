import List as L
import List ((::))
import Dict as D
import Array as A
import Text as T
import Maybe
import Debug
import Graphics.Element (..)
import Graphics.Collage as GC
import Graphics.Input (clickable)
import Color (..)
import Signal (Signal, Channel, send, channel, subscribe, (<~), (~), foldp)

type Pos = OnBoard (Int,Int) | InHand Int
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
  , clickedPosition : Maybe Pos
  , movablePositions : List Pos
  , mochiGoma : List (KomaType, Player)
  }

boardSize = {x = 3, y = 4}
komaSize = {x = 100, y = 100}

initBoard : Board
initBoard = [
    (OnBoard (0,0), Just (Elephant, P2), NoEffect)
  , (OnBoard (1,0), Just (Lion, P2), NoEffect)
  , (OnBoard (2,0), Just (Giraffe, P2), NoEffect)
  , (OnBoard (0,1), Nothing, NoEffect)
  , (OnBoard (1,1), Just (Chick, P2), NoEffect)
  , (OnBoard (2,1), Nothing, NoEffect)
  , (OnBoard (0,2), Nothing, NoEffect)
  , (OnBoard (1,2), Just (Chick, P1), NoEffect)
  , (OnBoard (2,2), Nothing, NoEffect)
  , (OnBoard (0,3), Just (Elephant, P1), NoEffect)
  , (OnBoard (1,3), Just (Lion, P1), NoEffect)
  , (OnBoard (2,3), Just (Giraffe, P1), NoEffect)
  ]

clickMessage : Channel Pos
clickMessage = channel <| OnBoard (0,0)



show : (Pos, StateAt, Effect) -> Element
show (p, s, e) =
  let effect : Element -> Element
      effect = if | e == Transparent -> opacity 0.5
                  | otherwise -> opacity 1.0

  in clickable (send clickMessage p) <| effect <| case s of
       Nothing -> emptyImg
       Just (kt, player) -> showKoma kt player


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
emptyImg = spacer komaSize.x komaSize.y |> color white

-- [((0,0), 'A'), ((1,0), 'B'), ((0,1), 'C'), ((1,1), 'D')] みたいな "Posが1要素目の2要素タプルのリスト" データを
-- [ ['A','B']
--  ,['C','D']] みたいな "二次元リスト" データに変換する
posKeyListTo2DList : Board -> List (List (Pos, StateAt, Effect))
posKeyListTo2DList tuples =
  let yMax : Int
      yMax = boardSize.y - 1
      yiList : Int -> List (Pos, StateAt, Effect) -> List (Pos, StateAt, Effect)
      yiList i = L.filter (\(p,_,_) -> case p of
                                         OnBoard (_,y) -> y == i
                                         InHand n -> False)
      tpl2nd3rd (_,b,c) = (b,c)
  in L.map (\yi -> (yiList yi) tuples ) [0..yMax]

fromListListStateAtToElement : List (List (Pos, StateAt, Effect)) -> Element
fromListListStateAtToElement = L.map (\cel -> flow right (L.map show cel)) >> flow down

-- 指定したマスの情報を返す
getAt : Board -> List (KomaType, Player) -> Pos -> (Pos, StateAt, Effect)
getAt b mochiG p =
    let getAtOnBoard : Board -> Pos -> (Pos, StateAt, Effect)
        getAtOnBoard b p = case L.filter (\(p',_,_) -> p' == p) b of
              [a] -> a
              otherwise -> (p, Nothing, NoEffect)
        getAtInHand : List (KomaType, Player) -> Int -> StateAt
        getAtInHand mochiG n = A.get n (A.fromList mochiG)
    in case p of
         OnBoard _ -> getAtOnBoard b p
         InHand n -> getAtInHand mochiG n |> (\stAt -> (p, stAt, NoEffect))

getStateAt : Board -> List (KomaType, Player) -> Pos -> StateAt
getStateAt b mochiG p = getAt b mochiG p |> (\(_,s,_) -> s)

-- 指定したPosの駒が動かせる場合、その移動可能範囲にエフェクトをつける
-- selected : Pos -> Board -> List (KomaType, Player) -> Board
selected : Pos -> GameState -> Board
selected p' gs =
  let (pos, st, _) = getAt gs.board gs.mochiGoma p'
      player = justOrCrash "yyy" st |> snd
      movablePos' : List Pos
      movablePos' = movablePos gs.board (p', st)
      effectedPoss : Pos -> Board -> List Pos
      effectedPoss p' b = pos :: movablePos'
  in if | (isOwn player gs.board gs.mochiGoma p') && (not (L.isEmpty movablePos'))
            -> L.map (\(p,s,e) -> if L.member p (effectedPoss p' gs.board) then (p,s,Transparent) else (p,s,e)) gs.board
        | otherwise -> gs.board

-- 選択状態を解除する
cancelSelect : Board -> Board
cancelSelect = L.map (\(p,s,e) -> (p,s, NoEffect))

initGameState : GameState
initGameState = { board = initBoard
                 , turn = P1
                 , playState = Neutral
                 , result = Unfinished
                 , clickedStateAt = Nothing
                 , clickedPosition = Nothing
                 , movablePositions = []
                 , mochiGoma = [] }

gameState : Signal GameState
gameState = foldp updateGameState initGameState (subscribe clickMessage)

main =
  let
      a : Board -> Element
      a b = b |> posKeyListTo2DList |> fromListListStateAtToElement
      komaDai : List (KomaType, Player) -> Element
      komaDai mochiG =
          let p1KomaDai = L.filter (\(i, (_, p)) -> p == P1) (L.map2 (,) [0..7] mochiG)
              p2KomaDai = L.filter (\(i, (_, p)) -> p == P2) (L.map2 (,) [0..7] mochiG)
          in flow down [
                    flow right <| L.map (\(i, (kt, p)) -> showKoma kt p |> clickable (send clickMessage (InHand i))) p2KomaDai
                  , flow right <| L.map (\(i, (kt, p)) -> showKoma kt p |> clickable (send clickMessage (InHand i))) p1KomaDai
                 ]
      view : GameState -> Element
      view gs = flow right [
                 flow down [
                   case gs.result of
                     Win p -> flow right [T.asText p,  T.plainText "の勝ちです"]
                     otherwise -> flow right [T.asText gs.turn, T.plainText "の手番です"]
                 , a gs.board |> color green
                 , T.asText gs
                      ] |> width (boardSize.x * komaSize.x)
                , komaDai gs.mochiGoma
                ]
  in view <~ gameState

-- その座標が盤上かどうかを返す
isOnBoard : Board -> (Int,Int) -> Bool
isOnBoard b (x,y) = (0 <= x) && (x <= boardSize.x - 1) && (0 <= y) && (y <= boardSize.y - 1)


-- 指定した駒の動けるマスのPosのリストを返す
movablePos : Board -> (Pos, StateAt) -> List Pos
movablePos b (p,s) = case p of
                       OnBoard p' -> movablePosOnBoard b (p', s)
                       InHand n -> movablePosInHand b

movablePosInHand : Board -> List Pos
movablePosInHand b = emptyPoss b

movablePosOnBoard : Board -> ((Int,Int), StateAt) -> List Pos
movablePosOnBoard b ((x,y), s) =
  let filterFunc : Player -> (Int,Int) -> Bool
      filterFunc pl xy = (isOnBoard b xy) && not (L.member (OnBoard xy) (possesOf pl b))
  in case s of
       Nothing -> []
       Just (kt, pl) -> case (kt, pl) of
         (Lion, p) -> [(x-1,y),(x,y-1),(x+1,y),(x,y+1),(x+1,y+1),(x+1,y-1),(x-1,y+1),(x-1,y-1)] |> L.filter (filterFunc p) |> L.map OnBoard
         (Elephant, p) -> [(x+1,y+1),(x-1,y+1),(x-1,y-1),(x+1,y-1)] |> L.filter (filterFunc p) |> L.map OnBoard
         (Giraffe, p) -> [(x,y+1),(x,y-1),(x+1,y),(x-1,y)] |> L.filter (filterFunc p) |> L.map OnBoard
         (Chick, p) -> [(x,if p == P1 then y-1 else y+1)] |> L.filter (filterFunc p) |> L.map OnBoard

-- 指定したプレイヤーの駒が占めているボード上のPosのリストを返す
possesOf : Player -> Board -> List Pos
possesOf player = L.filter (\(_, stateAt, _) -> case stateAt of
                                                  Just (_, p) -> p == player
                                                  otherwise -> False) >> L.map (\(a,_,_) -> a)

-- 指定したプレイヤーの駒が占めている持ち駒のPosのリストを返す
possesOfInHand : Player -> List (KomaType, Player) -> List Int
possesOfInHand player mochiGoma = L.map2 (,) [0..7] mochiGoma |> L.filter (\(n, (_,pl)) -> pl == player) |> L.map fst

-- 盤上でコマが置かれていないPosのリストを返す
emptyPoss : Board -> List Pos
emptyPoss = L.filter (\(p,s,_) -> s == Nothing && case p of
                                                      OnBoard _ -> True
                                                      otherwise -> False) >> L.map (\(p,_,_) -> p)

-- 指定したPosに自分の駒があるかどうかを返す
isOwn : Player -> Board -> List (KomaType, Player) -> Pos -> Bool
isOwn player board mochiGoma pos = case pos of
  OnBoard xy -> L.member pos (possesOf player board)
--   InHand n -> L.member n (possesOfInHand player mochiGoma) |> Debug.log "isOwn InHand"
  InHand n ->
      let a = Debug.log "InHand n" n
          b = Debug.log "InHand player" player
      in L.member n (possesOfInHand player mochiGoma) -- |> Debug.log "isOwn InHand"


updateGameState : Pos -> GameState -> GameState
updateGameState pos gs =
    let stAt : StateAt
        stAt = getAt (gs.board) (gs.mochiGoma) pos |> (\(_,s,_) -> s)
        mPoss : List Pos
        mPoss = movablePos gs.board (pos, stAt)
        isSelect : Bool
--         isSelect = (gs.playState == Neutral) && (isOwn gs.turn gs.board gs.mochiGoma (Debug.log "pos" pos))
        isSelect = (gs.playState == Neutral) && (isOwn gs.turn gs.board gs.mochiGoma pos)
        isMove : Bool
        isMove = (gs.playState == Selected) && (L.member pos gs.movablePositions)
        opponent_ : Player
        opponent_ = opponent gs.turn
        mochiGoma_ : StateAt -> Maybe Pos -> List (KomaType, Player)
        mochiGoma_ stAt clickedP = case (Debug.log "mochiGoma_ pos" pos) of
                       OnBoard _ -> case getStateAt gs.board gs.mochiGoma pos of
                         Just (kt, opponent_) -> (kt, gs.turn) :: gs.mochiGoma
                         otherwise -> case clickedP of
                           Just (InHand _) -> gs.mochiGoma |> L.filter (\mG -> (Debug.log "clickedStateAt" stAt) /= Just mG)
                           otherwise -> gs.mochiGoma
    in if | isMove -> { gs | playState <- Neutral
                        , result <- if getStateAt gs.board gs.mochiGoma pos == Just (Lion ,opponent_) then Win gs.turn else Unfinished
                        , board <- resetEffect <| updateBoard gs.board [
                                     (justOrCrash "xxx" gs.clickedPosition, Nothing, NoEffect)
                                   , (pos, gs.clickedStateAt, NoEffect)]
                        , turn <- (opponent gs.turn)
                        , clickedPosition <- Just pos
                        , movablePositions <- []
                        , mochiGoma <- mochiGoma_ gs.clickedStateAt gs.clickedPosition
                      }
          | isSelect -> { gs | board <- selected pos gs
                      , playState <- Selected
                      , clickedStateAt <- getStateAt gs.board gs.mochiGoma pos -- |> Debug.log "isSelect"
                      , clickedPosition <- Just pos
                      , movablePositions <- mPoss }
          | otherwise -> { gs | board <- cancelSelect gs.board
                         , playState <- Neutral
                         , clickedStateAt <- Nothing -- |> Debug.log "otherwise"
                         , clickedPosition <- Just pos
                         , movablePositions <- [] }

-- 対戦相手
opponent : Player -> Player
opponent p = if p == P1 then P2 else P1

updateBoard : Board -> List (Pos, StateAt, Effect) -> Board
updateBoard b pses =
  let onBoardFilter : Board -> Board
      onBoardFilter = L.filter (\(p,_,_) -> case p of
                                           OnBoard _ -> True
                                           InHand _ -> False)
      xy : Pos -> (Int,Int)
      xy p = case p of
               OnBoard xy -> xy
      updateOnePos : Board -> (Pos, StateAt, Effect) -> Board
      updateOnePos b (p,s,e) = case p of
        OnBoard xy_ -> boardToDict b |> D.update xy_ (\_ -> Just (s, e)) |> boardFromDict
        otherwise -> b
--       updateOnePos b (p,s,e) = onBoardFilter b |> boardToDict |> D.update (xy p) (\_ -> Just (s, e)) |> boardFromDict

      boardToDict : Board -> D.Dict (Int, Int) (StateAt, Effect)
      boardToDict b = onBoardFilter b |> L.map (\(p,s,e) -> ((xy p),(s,e))) |> D.fromList

      boardFromDict : D.Dict (Int, Int) (StateAt, Effect) -> Board
      boardFromDict d = D.toList d |> L.map (\(p, (s,e)) -> (OnBoard p,s,e))

  in L.foldl (\(p,s,e) b' -> updateOnePos b' (p,s,e)) b pses

resetEffect : Board -> Board
resetEffect = L.map (\(p,s,e) -> (p,s,NoEffect))

justOrCrash : String -> Maybe a -> a
justOrCrash errStr m = case m of
  Just a -> a
  Nothing -> Debug.crash errStr
