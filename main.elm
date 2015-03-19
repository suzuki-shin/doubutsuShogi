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
import Type
import Type (..)
import Port (..)

show a = case a of
           P1 -> "先手"
           P2 -> "後手"

boardSize = {x = 3, y = 4}
komaSize = {x = 100, y = 100}

initBoard : Board
initBoard = [
    (OnBoard (0,0), Just (Giraffe, P2), NoEffect)
  , (OnBoard (1,0), Just (Lion, P2), NoEffect)
  , (OnBoard (2,0), Just (Elephant, P2), NoEffect)
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

clickMessage : Channel Type.Pos
clickMessage = channel <| OnBoard (0,0)

-- 指定したマスの情報を返す
getAt : Board -> KomaDai -> KomaDai -> Type.Pos -> Cel
getAt b mochiG1 mochiG2 p =
    let getAtOnBoard : Board -> Type.Pos -> Cel
        getAtOnBoard b p = case L.filter (\(p',_,_) -> p' == p) b of
              [a] -> a
              otherwise -> (p, Nothing, NoEffect)
        getAtInHand : KomaDai -> Player -> Int -> StateAt
        getAtInHand mochiG player n = case A.get n mochiG of
                                        Just mG -> Just (mG, player)
                                        Nothing -> Nothing
    in case p of
         OnBoard _ -> getAtOnBoard b p
         InHand player n -> getAtInHand (if player == P1 then mochiG1 else mochiG2) player n |> (\stAt -> (p, stAt, NoEffect))

initGameState : GameState
initGameState = { board = initBoard
                 , turn = P1
                 , playState = Neutral
                 , result = Unfinished
                 , clickedStateAt = Nothing
                 , clickedPosition = Nothing
                 , movablePositions = []
                 , mochiGoma1 = A.empty
                 , mochiGoma2 = A.empty }

gameState : Signal GameState
gameState = foldp updateGameState initGameState (subscribe clickMessage)

main =
  let
      komaImg : KomaType -> Player -> String
      komaImg kt player =
          let ktImg = case kt of
                        Lion     -> "lion"
                        Elephant -> "zou"
                        Giraffe  -> "kirin"
                        Chick    -> "hiyoko"
                        Chicken  -> "niwatori"
                        otherwise -> Debug.log "ktImg" <| toString kt
              plImg = case player of
                        P1 -> "A"
                        P2 -> "B"
          in "img/" ++ ktImg ++ plImg ++ ".png"

      emptyImg : Element
      emptyImg = spacer komaSize.x komaSize.y |> color white

      komaElement : KomaType -> Player -> Element
      komaElement kt player = komaImg kt player |> fittedImage komaSize.x komaSize.y

      -- [((0,0), 'A'), ((1,0), 'B'), ((0,1), 'C'), ((1,1), 'D')] みたいな "Posが1要素目の2要素タプルのリスト" データを
      -- [ ['A','B']
      --  ,['C','D']] みたいな "二次元リスト" データに変換する
      posKeyListTo2DList : Board -> List (List Cel)
      posKeyListTo2DList tuples =
        let yMax : Int
            yMax = boardSize.y - 1
            yiList : Int -> List Cel -> List Cel
            yiList i = L.filter (\(p,_,_) -> case p of
                                               OnBoard (_,y) -> y == i
                                               InHand _ _ -> False)
            tpl2nd3rd (_,b,c) = (b,c)
        in L.map (\yi -> (yiList yi) tuples ) [0..yMax]

      celToElement : Cel -> Element
      celToElement (p, s, e) =
        let effect : Element -> Element
            effect = if | e == Transparent -> opacity 0.5
                        | otherwise -> opacity 1.0
        in clickable (send clickMessage p) <| effect <| case s of
             Nothing -> emptyImg
             Just (kt, player) -> komaElement kt player

      -- Cel の二次元リストをElementに変換する
      fromListListStateAtToElement : List (List Cel) -> Element
      fromListListStateAtToElement = L.map (\cel -> flow right (L.map celToElement cel)) >> flow down

      boardToElement : Board -> Element
      boardToElement = posKeyListTo2DList >> fromListListStateAtToElement >> color green

      toClickable : Type.Pos -> Element -> Element
      toClickable pos = clickable (send clickMessage pos)

      komaDaiToElement : Player -> KomaDai -> Element
      komaDaiToElement pl = A.toIndexedList >> L.map (\(i, kt) -> toClickable (InHand pl i) (komaElement kt pl)) >> flow right

      turnMessage : GameState -> Element
      turnMessage gs = case gs.result of
                             Win p -> flow right [p |> show |> T.plainText,  T.plainText "の勝ちです"]
                             otherwise -> flow right [gs.turn |> show |> T.plainText, T.plainText "の手番です"]

      -- GameStateの更新を受け取って描画する
      view : GameState -> Element
--       view gs = flow right [
      view gs =
          let a = Debug.log "view gs" gs
          in flow right [
                  flow down [
                      turnMessage gs
                    , boardToElement gs.board
--                     , T.asText gs
                  ] |> width (boardSize.x * komaSize.x)
                , flow down [
                      komaDaiToElement P2 gs.mochiGoma2
                    , komaDaiToElement P1 gs.mochiGoma1
                  ]
                ]

      view2 : ExGameState -> Element
      view2 = fromExGameState >> view
  in view2 <~ inGameState
--   in view <~ gameState

-- その座標が盤上かどうかを返す
isOnBoard : Board -> (Int,Int) -> Bool
isOnBoard b (x,y) = (0 <= x) && (x <= boardSize.x - 1) && (0 <= y) && (y <= boardSize.y - 1)


-- 指定した駒の動けるマスのPosのリストを返す
movablePos : Board -> (Type.Pos, StateAt) -> List Type.Pos
movablePos b (p,s) = case p of
                       OnBoard p' -> movablePosOnBoard b (p', s)
                       InHand _ n -> movablePosInHand b

movablePosInHand : Board -> List Type.Pos
movablePosInHand b = emptyPoss b

movablePosOnBoard : Board -> ((Int,Int), StateAt) -> List Type.Pos
movablePosOnBoard b ((x,y), s) =
  let filterFunc : Player -> (Int,Int) -> Bool
      filterFunc pl xy = (isOnBoard b xy) && not (L.member (OnBoard xy) (possesOf pl b))
      movableArea : KomaType -> Player -> List (Int, Int)
      movableArea kt pl = case kt of
         Lion     -> [(x-1,y),(x,y-1),(x+1,y),(x,y+1),(x+1,y+1),(x+1,y-1),(x-1,y+1),(x-1,y-1)]
         Elephant -> [(x+1,y+1),(x-1,y+1),(x-1,y-1),(x+1,y-1)]
         Giraffe  -> [(x,y+1),(x,y-1),(x+1,y),(x-1,y)]
         Chick    -> [(x,if pl == P1 then y-1 else y+1)]
         Chicken  -> [(x,y+1),(x,y-1),(x+1,y),(x-1,y)] ++ if pl == P1 then [(x-1,y-1),(x+1,y-1)] else [(x-1,y+1),(x+1,y+1)]
  in case s of
       Nothing -> []
       Just (kt, pl) -> movableArea kt pl |> L.filter (filterFunc pl) |> L.map OnBoard

-- 指定したプレイヤーの駒が占めているボード上のPosのリストを返す
possesOf : Player -> Board -> List Type.Pos
possesOf player = L.filter (\(_, stateAt, _) -> case stateAt of
                                                  Just (_, p) -> p == player
                                                  otherwise -> False) >> L.map (\(a,_,_) -> a)

-- 盤上でコマが置かれていないPosのリストを返す
emptyPoss : Board -> List Type.Pos
emptyPoss = L.filter (\(p,s,_) -> s == Nothing && case p of
                                                      OnBoard _ -> True
                                                      otherwise -> False) >> L.map (\(p,_,_) -> p)

-- 指定したPosに自分の駒があるかどうかを返す
isOwn : Player -> Board -> Type.Pos -> Bool
isOwn player board pos = case pos of
  OnBoard _ -> L.member pos (possesOf player board)
  InHand mochiGPlayer _ -> player == mochiGPlayer

updateGameState : Type.Pos -> GameState -> GameState
updateGameState pos gs =
    let stAt : StateAt
        stAt = getAt (gs.board) (gs.mochiGoma1) (gs.mochiGoma2) pos |> (\(_,s,_) -> s)
        getStateAt : Board -> KomaDai -> KomaDai -> Type.Pos -> StateAt
        getStateAt b mochiG1 mochiG2 p = getAt b mochiG1 mochiG2 p |> (\(_,s,_) -> s)
        mPoss : List Type.Pos
        mPoss = movablePos gs.board (pos, stAt)
        isSelect : Bool
        isSelect = (gs.playState == Neutral) && (isOwn gs.turn gs.board pos)
        isMove : Bool
        isMove = (gs.playState == Selected) && (L.member pos gs.movablePositions)
        isFinished : Bool
        isFinished = gs.result /= Unfinished
        opponent_ : Player
        opponent_ = opponent gs.turn
        mochiGoma_ : Player -> StateAt -> Maybe Type.Pos -> KomaDai
        mochiGoma_ pl stAt clickedP = case (Debug.log "mochiGoma_ pos" pos) of
                       OnBoard _ -> case getStateAt gs.board gs.mochiGoma1 gs.mochiGoma2 pos of
                         Just (Chicken, opponent_) -> (Debug.log "mochiGoma_ OnBoard" (A.push Chick (if pl == P1 then gs.mochiGoma1 else gs.mochiGoma2)))
                         Just (kt, opponent_) -> (Debug.log "mochiGoma_ OnBoard" (A.push kt (if pl == P1 then gs.mochiGoma1 else gs.mochiGoma2)))
                         otherwise -> case clickedP of
                           Just (InHand player _) -> (if player == P1 then gs.mochiGoma1 else gs.mochiGoma2)
                                                  |> A.filter (\mG -> (Debug.log "clickedStateAt" stAt) /= Just (mG,player))
                           otherwise -> if pl == P1 then gs.mochiGoma1 else gs.mochiGoma2
        -- 指定したPosの駒が動かせる場合、その移動可能範囲にエフェクトをつける
        selected : Type.Pos -> GameState -> Board
        selected p' gs =
          let (pos, st, _) = getAt gs.board gs.mochiGoma1 gs.mochiGoma2 p'
              player = justOrCrash "yyy" st |> snd
              movablePos' : List Type.Pos
              movablePos' = movablePos gs.board (p', st)
              effectedPoss : Type.Pos -> Board -> List Type.Pos
              effectedPoss p' b = pos :: movablePos'
          in if | (isOwn player gs.board p') && (not (L.isEmpty movablePos'))
                    -> L.map (\(p,s,e) -> if L.member p (effectedPoss p' gs.board) then (p,s,Transparent) else (p,s,e)) gs.board
                | otherwise -> gs.board

        resetEffect : Board -> Board
        resetEffect = L.map (\(p,s,e) -> (p,s,NoEffect))

        cancelSelect : Board -> Board
        cancelSelect = L.map (\(p,s,e) -> (p,s, NoEffect))

    in if | isFinished -> gs
          | isMove -> { gs | playState <- Neutral
                        , result <- if getStateAt gs.board gs.mochiGoma1 gs.mochiGoma2 pos == Just (Lion ,opponent_) then Win gs.turn else Unfinished
                        , board <- resetEffect <| updateBoard gs.board [
                                     (justOrCrash "xxx" gs.clickedPosition, Nothing, NoEffect)
                                   , (pos, gs.clickedStateAt, NoEffect) |> \cel -> case gs.clickedPosition of
                                                                                     Just (OnBoard _) -> nari cel
                                                                                     otherwise -> cel]
                        , turn <- (opponent gs.turn)
                        , clickedPosition <- Just pos
                        , movablePositions <- []
                        , mochiGoma1 <- if gs.turn == P1 then mochiGoma_ P1 gs.clickedStateAt gs.clickedPosition else gs.mochiGoma1
                        , mochiGoma2 <- if gs.turn == P2 then mochiGoma_ P2 gs.clickedStateAt gs.clickedPosition else gs.mochiGoma2
                      }
          | isSelect -> { gs | board <- selected pos gs
                      , playState <- Selected
                      , clickedStateAt <- getStateAt gs.board gs.mochiGoma1 gs.mochiGoma2 pos -- |> Debug.log "isSelect"
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

nari : Cel -> Cel
nari cel = case cel of
    (OnBoard (x,0), Just (Chick, P1), e) -> (OnBoard (x,0), Just (Chicken, P1), e)
    (OnBoard (x,3), Just (Chick, P2), e) -> (OnBoard (x,3), Just (Chicken, P2), e)
    otherwise -> cel

updateBoard : Board -> List Cel -> Board
updateBoard b updatedCels =
  let onBoardFilter : Board -> Board
      onBoardFilter = L.filter (\(p,_,_) -> case p of
                                           OnBoard _ -> True
                                           InHand _ _ -> False)
      xy : Type.Pos -> (Int,Int)
      xy p = case p of
               OnBoard xy -> xy
      updateOnePos : Board -> Cel -> Board
      updateOnePos b (p,s,e) = case p of
        OnBoard xy_ -> boardToDict b |> D.update xy_ (\_ -> Just (s, e)) |> boardFromDict
        otherwise -> b

      boardToDict : Board -> D.Dict (Int, Int) (StateAt, Effect)
      boardToDict b = onBoardFilter b |> L.map (\(p,s,e) -> ((xy p),(s,e))) |> D.fromList

      boardFromDict : D.Dict (Int, Int) (StateAt, Effect) -> Board
      boardFromDict d = D.toList d |> L.map (\(p, (s,e)) -> (OnBoard p,s,e))

  in L.foldl (\(p,s,e) b' -> updateOnePos b' (p,s,e)) b updatedCels

justOrCrash : String -> Maybe a -> a
justOrCrash errStr m = case m of
  Just a -> a
  Nothing -> Debug.crash errStr


port exGameState : Signal ExGameState
port exGameState = toExGameState <~ gameState

port inGameState : Signal ExGameState

