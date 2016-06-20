module Board exposing (Color(..), SpaceType(..), Dynasty(..), Token, TileId, Tile, Space, Board, defaultBoard)

import Array exposing (Array)

type Color = Red | Green | Blue | Black
type SpaceType = Regular | Water
type Dynasty = Archer | Bull | Pot | Lion

type alias Token = {
  color:Color,
  dynasty:Dynasty
}
type alias TileId = Int
type alias Tile = {
  color: Color,
  id : TileId
}

type alias Space = {
  spaceType:SpaceType,
  tile:Maybe Tile,
  token:Maybe Token
}

type alias Board = Array Space

defaultBoard : Board
defaultBoard =
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
