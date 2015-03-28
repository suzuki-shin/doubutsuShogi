import Graphics.Element (..)
import Graphics.Collage (rotate, collage, toForm)
import Color (..)
import List as L
import Graphics.Input (clickable)
import Signal (Signal, Channel, send, channel, subscribe, (<~), (~), foldp, merge)
import Keyboard (space)
import Array as A
import Text as T
import Debug

import DoubutsuShogi.Type as Type
import DoubutsuShogi.Type (..)
import DoubutsuShogi.Port (..)
import DoubutsuShogi (..)

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

      komaElementAtKomaDai : KomaType -> Player -> Element
      komaElementAtKomaDai kt player = komaImg kt player |> fittedImage (round (toFloat komaSize.x * 0.5)) (round (toFloat komaSize.y * 0.5))

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
      komaDaiToElement pl = A.toIndexedList >> L.map (\(i, kt) -> toClickable (InHand pl i) (komaElementAtKomaDai kt pl)) >> flow right

      turnMessage : GameState -> Element
      turnMessage gs = case gs.result of
                             Win p -> flow right [p |> show |> T.plainText,  T.plainText "の勝ちです"]
                             otherwise -> flow right [gs.turn |> show |> T.plainText, T.plainText "の手番です"]

      reverseBoard : Bool -> Element -> Element
      reverseBoard reverseFlg e =
          let (w,h) = sizeOf e
          in if reverseFlg then collage w h [e |> toForm |> (rotate (degrees 180))] else e

      -- GameStateの更新を受け取って描画する
      view : Bool -> GameState -> Element
      view isReverse gs = flow down [
                      turnMessage gs
                    , T.plainText <| if isReverse then "　▽後手　▲先手" else "　▼先手　△後手"
                    , flow down [
                        komaDaiToElement P2 gs.mochiGoma2
                      , spacer 10 10
                      , boardToElement gs.board |> width (boardSize.x * komaSize.x)
                      , spacer 10 10
                      , komaDaiToElement P1 gs.mochiGoma1
                      ] |> reverseBoard isReverse
                ]

   in view <~ reverseFlg ~ gameState (fromExPos <~ inClickedPos)

reverseFlg : Signal Bool
reverseFlg = foldp (\s acc -> if s then not acc else acc) False space


port exClickedPos : Signal ExPos
port exClickedPos = toExPos <~ subscribe clickMessage

port inClickedPos : Signal ExPos
