module Main exposing (main)

import Html exposing (..)
import Html.App as Html
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import String as String
import Array
import Board exposing (..)
import Maybe exposing (andThen)


main : Program Never
main =
    Html.beginnerProgram { model = defaultModel, view = view, update = update }


type alias Hand =
    List Tile


type alias Player =
    { dynasty : Dynasty
    , hand : Hand
    , selectedTile : Maybe Tile
    }



-- Model


type alias Model =
    { bag : List Tile
    , board : Board
    , players : List Player
    , activeDynasty : Dynasty
    }


defaultModel : Model
defaultModel =
    let
        redTiles =
            List.map (Tile Red) [0..57]

        greenTiles =
            List.map (Tile Green) [0..30]

        blackTiles =
            List.map (Tile Black) [0..30]

        blueTiles =
            List.map (Tile Blue) [0..36]
    in
        { bag = List.concat [ redTiles, greenTiles, blackTiles, blueTiles ]
        , board = defaultBoard
        , players =
            [ (Player Archer [] Nothing)
            , (Player Bull [] Nothing)
            ]
        , activeDynasty = Archer
        }



-- Update


type Msg
    = NewGame
    | PlaceTile Int
    | SelectTile Player (Maybe Tile)
    | DrawTile Tile
    | EndTurn


isActivePlayer : Model -> Player -> Bool
isActivePlayer model player =
    player.dynasty == model.activeDynasty


getActivePlayer : Model -> Maybe Player
getActivePlayer model =
    (List.head (List.filter (isActivePlayer model) model.players))


update : Msg -> Model -> Model
update msg model =
    case msg of
        NewGame ->
            model

        EndTurn ->
            model

        DrawTile tile ->
            let
                ( drawnTiles, bag' ) =
                    List.partition ((==) tile) model.bag

                updatePlayer player =
                    { player | hand = (List.append player.hand drawnTiles) }

                players' =
                    ifMap (isActivePlayer model) updatePlayer model.players
            in
                { model | players = players', bag = bag' }

        PlaceTile loc ->
            case (getActivePlayer model) `andThen` .selectedTile of
                Just selectedTile ->
                    if isValidMove model.board loc selectedTile then
                        let
                            updatePlayer player =
                                { player
                                    | selectedTile = Nothing
                                    , hand = List.filter ((/=) selectedTile) player.hand
                                }
                        in
                            { model
                                | board = (placeTile model.board loc selectedTile)
                                , players = (ifMap (isActivePlayer model) updatePlayer model.players)
                            }
                    else
                        model

                Nothing ->
                    model

        SelectTile player tile ->
            let
                selectTiles p =
                    { p | selectedTile = tile }
            in
                { model | players = ifMap ((==) player) selectTiles model.players }


ifMap : (a -> Bool) -> (a -> a) -> List a -> List a
ifMap predicate update list =
    let
        f item =
            if (predicate item) then
                (update item)
            else
                item
    in
        List.map f list


placeTile : Board -> Int -> Tile -> Board
placeTile board loc newTile =
    case (Array.get loc board) of
        Just space ->
            Array.set loc { space | tile = Just newTile } board

        Nothing ->
            board


isValidMove : Board -> Int -> Tile -> Bool
isValidMove board loc tile =
    case (Array.get loc board) of
        Just space ->
            case ( space.tile, space.spaceType ) of
                ( Just tile, _ ) ->
                    False

                ( _, Regular ) ->
                    tile.color /= Blue

                ( _, Water ) ->
                    tile.color == Blue

        Nothing ->
            False



-- View


renderTile : Tile -> Html Msg
renderTile tile =
    let
        tileClass =
            [ ( "tile", True )
            , ( (String.toLower <| toString <| tile.color), True )
            ]
    in
        div [ classList tileClass ] [ text <| toString <| tile.id ]


renderSpace : Int -> Space -> Html Msg
renderSpace i s =
    let
        spaceClass =
            [ ( "space", True )
            , ( String.toLower <| toString <| s.spaceType, True )
            ]

        tile =
            case s.tile of
                Just t ->
                    [ renderTile t ]

                Nothing ->
                    []
    in
        div
            [ classList spaceClass
            , onClick (PlaceTile i)
            ]
            tile


renderPlayer : Player -> Html Msg
renderPlayer player =
    let
        renderHand hand =
            div [ class "hand" ] (List.map renderHandSpace hand)

        renderHandSpace tile =
            div [ (classList [ ( "space", True ), ( "selected", player.selectedTile == Just tile ) ]), (onClick (SelectTile player (Just tile))) ]
                [ renderTile tile
                ]
    in
        div [ class "player" ]
            [ h1 [] [ text <| toString <| player.dynasty ]
            , renderHand player.hand
            , div [ onClick (SelectTile player Nothing) ] []
            ]


renderBag : List Tile -> Html Msg
renderBag tiles =
    let
        renderBagSpace tile =
            div [ class "space", onClick (DrawTile tile) ]
                [ renderTile tile
                ]
    in
        div []
            [ (h1 [] [ text ("Bag " ++ (toString <| List.length <| tiles)) ])
            , div [ class "bag" ] (List.map renderBagSpace tiles)
            ]


view : Model -> Html Msg
view model =
    div []
        [ div [ class "board" ] (Array.toList <| Array.indexedMap renderSpace model.board)
        , button [ onClick EndTurn ] [ text "End Turn" ]
        , div [] (List.map renderPlayer model.players)
        , renderBag model.bag
        ]
