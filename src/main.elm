module Tigris exposing (main)

import Html exposing (..)
import Html.App as Html
import Html.Attributes exposing (..)
import Html.Events exposing (..)

import String as String
import Array
import Dict

main =
  Html.beginnerProgram { model = model, view = view, update = update }


type Color = Red | Green | Blue | Black
type SpaceType = Regular | Water
type Dynasty = Archer | Bull | Pot | Lion

type alias Token = {
  color:Color,
  dynasty:Dynasty
}

type alias Tile = {
  color: Color
}
type alias Space = {
  spaceType:SpaceType,
  tile:Maybe Tile,
  token:Maybe Token
}
type alias Board = Array.Array Space
type alias Hand = Array.Array Tile
type alias Player = {
  dynasty : Dynasty,
  hand : Hand,
  selectedTile : Maybe Int
}

-- Model

type alias Model = {
  board : Board,
  players : Array.Array Player,
  activePlayer : Int
}

model : Model
model =
  {
    board = startingBoard,
    players = Array.fromList [
      (Player Archer (Array.fromList (List.map Tile [Red, Blue, Green, Black, Red, Green])) Nothing),
      (Player Bull (Array.fromList (List.map Tile [Green, Blue, Blue, Black, Red, Red])) Nothing)
    ],
    activePlayer = 0
  }

startingBoard : Board
startingBoard =
  let
    r = (Space Regular Nothing Nothing)
    w = (Space Water Nothing Nothing)
  in
    Array.fromList [
      r, r, r, r, w, w, w, w, w, r, r, r, w, r, r, r,
      r, r, r, r, w, r, r, r, r, r, r, r, w, r, r, r,
      r, r, r, w, w, r, r, r, r, r, r, r, w, w, r, r,
      w, w, w, w, r, r, r, r, r, r, r, r, r, w, w, w,
      r, r, r, r, r, r, r, r, r, r, r, r, r, r, w, w,
      r, r, r, r, r, r, r, r, r, r, r, r, r, r, w, r,
      w, w, w, w, r, r, r, r, r, r, r, r, w, w, w, r,
      r, r, r, w, w, w, w, r, r, r, r, r, w, r, r, r,
      r, r, r, r, r, r, w, w, w, w, w, w, w, r, r, r,
      r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r,
      r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r
    ]




-- Update

type Msg = NewGame | PlaceTile Int | SelectTile Player (Maybe Int)

update : Msg -> Model -> Model
update msg model =
  case msg of
    NewGame ->
      model
    PlaceTile loc ->
      let
        activePlayer =
          (Array.get model.activePlayer model.players)
        selectedTileIndex =
          activePlayer `Maybe.andThen` (\player -> player.selectedTile)
        selectedTile =
          case (activePlayer, selectedTileIndex) of
            (Just player, Just tileIndex) ->
              Array.get tileIndex player.hand
            (_, _) ->
              Nothing
        updatePlayer i player =
          if i == model.activePlayer then
            case player.selectedTile of
              Just tileIndex ->
                { player | selectedTile = Nothing,
                           hand = (arrayRemove tileIndex player.hand) }
              Nothing  -> player
          else
            player

      in
        if isValidMove model.board loc selectedTile then
          { model | board = (placeTile model.board loc selectedTile),
                    players = (Array.indexedMap updatePlayer model.players) }
        else
          model

    SelectTile player tileIndex ->
      let
        selectTiles p =
          if p == player then
            { p | selectedTile = tileIndex }
          else
            p
      in
        { model | players = Array.map selectTiles model.players }

arrayRemove : Int -> Array.Array a -> Array.Array a
arrayRemove index array =
  Array.append (Array.slice 0 index array) (Array.slice (index + 1) (Array.length array) array)

placeTile : Board -> Int -> Maybe Tile -> Board
placeTile board loc newTile =
  case (Array.get loc board) of
    Just space ->
      Array.set loc { space | tile = newTile } board
    Nothing -> board

isValidMove : Board -> Int -> Maybe Tile -> Bool
isValidMove board loc tile =
  case ((Array.get loc board), tile) of
    (Just space, Just newTile) ->
      case space.spaceType of
        Regular -> newTile.color /= Blue
        Water -> newTile.color == Blue
    (_, _) -> False




-- View

renderTile : Tile -> Html Msg
renderTile tile =
  let
    tileClass = [
      ("tile", True),
      ((String.toLower <| toString <| tile.color), True)
    ]
  in
    div [classList tileClass] []

renderSpace : Int -> Space -> Html Msg
renderSpace i s =
  let
    spaceClass = ("space " ++ (String.toLower <| toString <| s.spaceType))
    tile =
      case s.tile of
        Just t -> [renderTile t]
        Nothing -> []
  in
    div [(class spaceClass),
         onClick (PlaceTile i)] tile

renderPlayer : Player -> Html Msg
renderPlayer player =
  let
    selectedTile = case player.selectedTile of
      Just t -> toString t
      Nothing -> "Nothing Selected"
    renderHand hand =
      div [class "hand"] (Array.toList (Array.indexedMap renderHandSpace hand))
    renderHandSpace i tile =
      div [(classList [("space", True), ("selected", player.selectedTile == Just i)]), (onClick (SelectTile player (Just i)))] [
        renderTile tile
      ]
  in
    div [class "player"] [
      h1 [] [text <| toString <| player.dynasty],
      renderHand player.hand,
      div [onClick (SelectTile player Nothing)] [
        text selectedTile
      ]
    ]

view : Model -> Html Msg
view model =
  let
    board = model.board
  in
    div [] [
      div [class "board"] (Array.toList <| Array.indexedMap renderSpace board),
      div [] (Array.toList (Array.map renderPlayer model.players))
    ]
