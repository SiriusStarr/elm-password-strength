module Internal.Zxcvbn.AdjacencyGraphs exposing (AdjacencyDict(..), dvorak, keyboardAverageDegree, keyboardStartingPositions, keypad, keypadAverageDegree, keypadStartingPositions, macKeypad, qwerty)

{-| These adjacency graphs are imported from <https://github.com/dropbox/zxcvbn/blob/master/src/adjacency_graphs.coffee>
-}

import Dict exposing (Dict)



-- ! Needed for `calcAverageDegree`
-- import List.Extra as ListX
-- import Maybe.Extra as MaybeX
-- ! Determine the average DoF for a key, e.g. on qwerty, 'g' has degree 6, being adjacent to 'ftyhbv'. '' has degree 1. This calculates the average over all keys. These values are cached, as there is no reason to recompute them every time.
--
-- calcAverageDegree : AdjacencyDict -> Float
-- calcAverageDegree graph =
--     case graph of
--         Shiftable d ->
--             Dict.values d
--                 |> List.foldl (\l acc -> acc + ListX.count MaybeX.isJust (Tuple.first l)) 0
--                 |> (\i -> toFloat i / toFloat (Dict.size d))
--         Unshiftable d ->
--             Dict.values d
--                 |> List.foldl (\l acc -> acc + ListX.count MaybeX.isJust l) 0
--                 |> (\i -> toFloat i / toFloat (Dict.size d))
--! Simply Dict.size that unwraps for the fact that `AdjacencyDict`s may be shiftable or not.
--
-- adjacencyDictSize : AdjacencyDict -> Int
-- adjacencyDictSize graph =
--     case graph of
--         Shiftable d ->
--             Dict.size d
--         Unshiftable d ->
--             Dict.size d


{-| Average DoF for a (qwerty) keyboard. Value is cached but may be generated with `calcAverageDegree`.
-}
keyboardAverageDegree : Float
keyboardAverageDegree =
    -- * calcAverageDegree qwerty
    4.595744680851064


{-| Average DoF for a numeric keypad. Value is cached by may be generated with `calcAverageDegree`. Slightly different for keypad/mac keypad, but close enough.
-}
keypadAverageDegree : Float
keypadAverageDegree =
    -- * calcAverageDegree keypad
    5.066666666666666


{-| Number of possible starting positions on a (qwerty) keyboard. This is simply the size of the adjacency graph and is cached but may be generated with `adjacencyDictSize`.
-}
keyboardStartingPositions : Float
keyboardStartingPositions =
    -- * toFloat <| adjacencyDictSize qwerty
    94


{-| Number of possible starting positions on a numeric keypad. This is simply the size of the adjacency graph and is cached but may be generated with `adjacencyDictSize`.
-}
keypadStartingPositions : Float
keypadStartingPositions =
    -- * toFloat <| adjacencyDictSize keypad
    15


{-| Graph of all possible keys and adjacencies on a keyboard/keypad. Keys are a starting key and values are adjacent keys. `Nothing` values mean no key exists in that direction, e.g. there is no key left of 'A' on a qwerty keyboard; the order of elements in the list is consistent with respect to direction (clockwise from left). In the case of "shiftable" keys (e.g. where each key may produce two different characters depending on whether Shift is held), values are a tuple of lists, where the first is unshifted and the second is shifted.
-}
type AdjacencyDict
    = Shiftable (Dict Char ( List (Maybe Char), List (Maybe Char) ))
    | Unshiftable (Dict Char (List (Maybe Char)))


{-| Adjacency graph for standard qwerty keyboards.
-}
qwerty : AdjacencyDict
qwerty =
    Shiftable <|
        Dict.fromList
            [ ( '!', ( [ Just '`', Nothing, Nothing, Just '2', Just 'q', Nothing ], [ Just '~', Nothing, Nothing, Just '@', Just 'Q', Nothing ] ) )
            , ( '"', ( [ Just ';', Just '[', Just ']', Nothing, Nothing, Just '/' ], [ Just ':', Just '{', Just '}', Nothing, Nothing, Just '?' ] ) )
            , ( '#', ( [ Just '2', Nothing, Nothing, Just '4', Just 'e', Just 'w' ], [ Just '@', Nothing, Nothing, Just '$', Just 'E', Just 'W' ] ) )
            , ( '$', ( [ Just '3', Nothing, Nothing, Just '5', Just 'r', Just 'e' ], [ Just '#', Nothing, Nothing, Just '%', Just 'R', Just 'E' ] ) )
            , ( '%', ( [ Just '4', Nothing, Nothing, Just '6', Just 't', Just 'r' ], [ Just '$', Nothing, Nothing, Just '^', Just 'T', Just 'R' ] ) )
            , ( '&', ( [ Just '6', Nothing, Nothing, Just '8', Just 'u', Just 'y' ], [ Just '^', Nothing, Nothing, Just '*', Just 'U', Just 'Y' ] ) )
            , ( '\'', ( [ Just ';', Just '[', Just ']', Nothing, Nothing, Just '/' ], [ Just ':', Just '{', Just '}', Nothing, Nothing, Just '?' ] ) )
            , ( '(', ( [ Just '8', Nothing, Nothing, Just '0', Just 'o', Just 'i' ], [ Just '*', Nothing, Nothing, Just ')', Just 'O', Just 'I' ] ) )
            , ( ')', ( [ Just '9', Nothing, Nothing, Just '-', Just 'p', Just 'o' ], [ Just '(', Nothing, Nothing, Just '_', Just 'P', Just 'O' ] ) )
            , ( '*', ( [ Just '7', Nothing, Nothing, Just '9', Just 'i', Just 'u' ], [ Just '&', Nothing, Nothing, Just '(', Just 'I', Just 'U' ] ) )
            , ( '+', ( [ Just '-', Nothing, Nothing, Nothing, Just ']', Just '[' ], [ Just '_', Nothing, Nothing, Nothing, Just '}', Just '{' ] ) )
            , ( ',', ( [ Just 'm', Just 'k', Just 'l', Just '.', Nothing, Nothing ], [ Just 'M', Just 'K', Just 'L', Just '>', Nothing, Nothing ] ) )
            , ( '-', ( [ Just '0', Nothing, Nothing, Just '=', Just '[', Just 'p' ], [ Just ')', Nothing, Nothing, Just '+', Just '{', Just 'P' ] ) )
            , ( '.', ( [ Just ',', Just 'l', Just ';', Just '/', Nothing, Nothing ], [ Just '<', Just 'L', Just ':', Just '?', Nothing, Nothing ] ) )
            , ( '/', ( [ Just '.', Just ';', Just '\'', Nothing, Nothing, Nothing ], [ Just '>', Just ':', Just '"', Nothing, Nothing, Nothing ] ) )
            , ( '0', ( [ Just '9', Nothing, Nothing, Just '-', Just 'p', Just 'o' ], [ Just '(', Nothing, Nothing, Just '_', Just 'P', Just 'O' ] ) )
            , ( '1', ( [ Just '`', Nothing, Nothing, Just '2', Just 'q', Nothing ], [ Just '~', Nothing, Nothing, Just '@', Just 'Q', Nothing ] ) )
            , ( '2', ( [ Just '1', Nothing, Nothing, Just '3', Just 'w', Just 'q' ], [ Just '!', Nothing, Nothing, Just '#', Just 'W', Just 'Q' ] ) )
            , ( '3', ( [ Just '2', Nothing, Nothing, Just '4', Just 'e', Just 'w' ], [ Just '@', Nothing, Nothing, Just '$', Just 'E', Just 'W' ] ) )
            , ( '4', ( [ Just '3', Nothing, Nothing, Just '5', Just 'r', Just 'e' ], [ Just '#', Nothing, Nothing, Just '%', Just 'R', Just 'E' ] ) )
            , ( '5', ( [ Just '4', Nothing, Nothing, Just '6', Just 't', Just 'r' ], [ Just '$', Nothing, Nothing, Just '^', Just 'T', Just 'R' ] ) )
            , ( '6', ( [ Just '5', Nothing, Nothing, Just '7', Just 'y', Just 't' ], [ Just '%', Nothing, Nothing, Just '&', Just 'Y', Just 'T' ] ) )
            , ( '7', ( [ Just '6', Nothing, Nothing, Just '8', Just 'u', Just 'y' ], [ Just '^', Nothing, Nothing, Just '*', Just 'U', Just 'Y' ] ) )
            , ( '8', ( [ Just '7', Nothing, Nothing, Just '9', Just 'i', Just 'u' ], [ Just '&', Nothing, Nothing, Just '(', Just 'I', Just 'U' ] ) )
            , ( '9', ( [ Just '8', Nothing, Nothing, Just '0', Just 'o', Just 'i' ], [ Just '*', Nothing, Nothing, Just ')', Just 'O', Just 'I' ] ) )
            , ( ':', ( [ Just 'l', Just 'p', Just '[', Just '\'', Just '/', Just '.' ], [ Just 'L', Just 'P', Just '{', Just '"', Just '?', Just '>' ] ) )
            , ( ';', ( [ Just 'l', Just 'p', Just '[', Just '\'', Just '/', Just '.' ], [ Just 'L', Just 'P', Just '{', Just '"', Just '?', Just '>' ] ) )
            , ( '<', ( [ Just 'm', Just 'k', Just 'l', Just '.', Nothing, Nothing ], [ Just 'M', Just 'K', Just 'L', Just '>', Nothing, Nothing ] ) )
            , ( '=', ( [ Just '-', Nothing, Nothing, Nothing, Just ']', Just '[' ], [ Just '_', Nothing, Nothing, Nothing, Just '}', Just '{' ] ) )
            , ( '>', ( [ Just ',', Just 'l', Just ';', Just '/', Nothing, Nothing ], [ Just '<', Just 'L', Just ':', Just '?', Nothing, Nothing ] ) )
            , ( '?', ( [ Just '.', Just ';', Just '\'', Nothing, Nothing, Nothing ], [ Just '>', Just ':', Just '"', Nothing, Nothing, Nothing ] ) )
            , ( '@', ( [ Just '1', Nothing, Nothing, Just '3', Just 'w', Just 'q' ], [ Just '!', Nothing, Nothing, Just '#', Just 'W', Just 'Q' ] ) )
            , ( 'A', ( [ Nothing, Just 'q', Just 'w', Just 's', Just 'z', Nothing ], [ Nothing, Just 'Q', Just 'W', Just 'S', Just 'Z', Nothing ] ) )
            , ( 'B', ( [ Just 'v', Just 'g', Just 'h', Just 'n', Nothing, Nothing ], [ Just 'V', Just 'G', Just 'H', Just 'N', Nothing, Nothing ] ) )
            , ( 'C', ( [ Just 'x', Just 'd', Just 'f', Just 'v', Nothing, Nothing ], [ Just 'X', Just 'D', Just 'F', Just 'V', Nothing, Nothing ] ) )
            , ( 'D', ( [ Just 's', Just 'e', Just 'r', Just 'f', Just 'c', Just 'x' ], [ Just 'S', Just 'E', Just 'R', Just 'F', Just 'C', Just 'X' ] ) )
            , ( 'E', ( [ Just 'w', Just '3', Just '4', Just 'r', Just 'd', Just 's' ], [ Just 'W', Just '#', Just '$', Just 'R', Just 'D', Just 'S' ] ) )
            , ( 'F', ( [ Just 'd', Just 'r', Just 't', Just 'g', Just 'v', Just 'c' ], [ Just 'D', Just 'R', Just 'T', Just 'G', Just 'V', Just 'C' ] ) )
            , ( 'G', ( [ Just 'f', Just 't', Just 'y', Just 'h', Just 'b', Just 'v' ], [ Just 'F', Just 'T', Just 'Y', Just 'H', Just 'B', Just 'V' ] ) )
            , ( 'H', ( [ Just 'g', Just 'y', Just 'u', Just 'j', Just 'n', Just 'b' ], [ Just 'G', Just 'Y', Just 'U', Just 'J', Just 'N', Just 'B' ] ) )
            , ( 'I', ( [ Just 'u', Just '8', Just '9', Just 'o', Just 'k', Just 'j' ], [ Just 'U', Just '*', Just '(', Just 'O', Just 'K', Just 'J' ] ) )
            , ( 'J', ( [ Just 'h', Just 'u', Just 'i', Just 'k', Just 'm', Just 'n' ], [ Just 'H', Just 'U', Just 'I', Just 'K', Just 'M', Just 'N' ] ) )
            , ( 'K', ( [ Just 'j', Just 'i', Just 'o', Just 'l', Just ',', Just 'm' ], [ Just 'J', Just 'I', Just 'O', Just 'L', Just '<', Just 'M' ] ) )
            , ( 'L', ( [ Just 'k', Just 'o', Just 'p', Just ';', Just '.', Just ',' ], [ Just 'K', Just 'O', Just 'P', Just ':', Just '>', Just '<' ] ) )
            , ( 'M', ( [ Just 'n', Just 'j', Just 'k', Just ',', Nothing, Nothing ], [ Just 'N', Just 'J', Just 'K', Just '<', Nothing, Nothing ] ) )
            , ( 'N', ( [ Just 'b', Just 'h', Just 'j', Just 'm', Nothing, Nothing ], [ Just 'B', Just 'H', Just 'J', Just 'M', Nothing, Nothing ] ) )
            , ( 'O', ( [ Just 'i', Just '9', Just '0', Just 'p', Just 'l', Just 'k' ], [ Just 'I', Just '(', Just ')', Just 'P', Just 'L', Just 'K' ] ) )
            , ( 'P', ( [ Just 'o', Just '0', Just '-', Just '[', Just ';', Just 'l' ], [ Just 'O', Just ')', Just '_', Just '{', Just ':', Just 'L' ] ) )
            , ( 'Q', ( [ Nothing, Just '1', Just '2', Just 'w', Just 'a', Nothing ], [ Nothing, Just '!', Just '@', Just 'W', Just 'A', Nothing ] ) )
            , ( 'R', ( [ Just 'e', Just '4', Just '5', Just 't', Just 'f', Just 'd' ], [ Just 'E', Just '$', Just '%', Just 'T', Just 'F', Just 'D' ] ) )
            , ( 'S', ( [ Just 'a', Just 'w', Just 'e', Just 'd', Just 'x', Just 'z' ], [ Just 'A', Just 'W', Just 'E', Just 'D', Just 'X', Just 'Z' ] ) )
            , ( 'T', ( [ Just 'r', Just '5', Just '6', Just 'y', Just 'g', Just 'f' ], [ Just 'R', Just '%', Just '^', Just 'Y', Just 'G', Just 'F' ] ) )
            , ( 'U', ( [ Just 'y', Just '7', Just '8', Just 'i', Just 'j', Just 'h' ], [ Just 'Y', Just '&', Just '*', Just 'I', Just 'J', Just 'H' ] ) )
            , ( 'V', ( [ Just 'c', Just 'f', Just 'g', Just 'b', Nothing, Nothing ], [ Just 'C', Just 'F', Just 'G', Just 'B', Nothing, Nothing ] ) )
            , ( 'W', ( [ Just 'q', Just '2', Just '3', Just 'e', Just 's', Just 'a' ], [ Just 'Q', Just '@', Just '#', Just 'E', Just 'S', Just 'A' ] ) )
            , ( 'X', ( [ Just 'z', Just 's', Just 'd', Just 'c', Nothing, Nothing ], [ Just 'Z', Just 'S', Just 'D', Just 'C', Nothing, Nothing ] ) )
            , ( 'Y', ( [ Just 't', Just '6', Just '7', Just 'u', Just 'h', Just 'g' ], [ Just 'T', Just '^', Just '&', Just 'U', Just 'H', Just 'G' ] ) )
            , ( 'Z', ( [ Nothing, Just 'a', Just 's', Just 'x', Nothing, Nothing ], [ Nothing, Just 'A', Just 'S', Just 'X', Nothing, Nothing ] ) )
            , ( '[', ( [ Just 'p', Just '-', Just '=', Just ']', Just '\'', Just ';' ], [ Just 'P', Just '_', Just '+', Just '}', Just '"', Just ':' ] ) )
            , ( '\\', ( [ Just ']', Nothing, Nothing, Nothing, Nothing, Nothing ], [ Just '}', Nothing, Nothing, Nothing, Nothing, Nothing ] ) )
            , ( ']', ( [ Just '[', Just '=', Nothing, Just '\\', Nothing, Just '\'' ], [ Just '{', Just '+', Nothing, Just '|', Nothing, Just '"' ] ) )
            , ( '^', ( [ Just '5', Nothing, Nothing, Just '7', Just 'y', Just 't' ], [ Just '%', Nothing, Nothing, Just '&', Just 'Y', Just 'T' ] ) )
            , ( '_', ( [ Just '0', Nothing, Nothing, Just '=', Just '[', Just 'p' ], [ Just ')', Nothing, Nothing, Just '+', Just '{', Just 'P' ] ) )
            , ( '`', ( [ Nothing, Nothing, Nothing, Just '1', Nothing, Nothing ], [ Nothing, Nothing, Nothing, Just '!', Nothing, Nothing ] ) )
            , ( 'a', ( [ Nothing, Just 'q', Just 'w', Just 's', Just 'z', Nothing ], [ Nothing, Just 'Q', Just 'W', Just 'S', Just 'Z', Nothing ] ) )
            , ( 'b', ( [ Just 'v', Just 'g', Just 'h', Just 'n', Nothing, Nothing ], [ Just 'V', Just 'G', Just 'H', Just 'N', Nothing, Nothing ] ) )
            , ( 'c', ( [ Just 'x', Just 'd', Just 'f', Just 'v', Nothing, Nothing ], [ Just 'X', Just 'D', Just 'F', Just 'V', Nothing, Nothing ] ) )
            , ( 'd', ( [ Just 's', Just 'e', Just 'r', Just 'f', Just 'c', Just 'x' ], [ Just 'S', Just 'E', Just 'R', Just 'F', Just 'C', Just 'X' ] ) )
            , ( 'e', ( [ Just 'w', Just '3', Just '4', Just 'r', Just 'd', Just 's' ], [ Just 'W', Just '#', Just '$', Just 'R', Just 'D', Just 'S' ] ) )
            , ( 'f', ( [ Just 'd', Just 'r', Just 't', Just 'g', Just 'v', Just 'c' ], [ Just 'D', Just 'R', Just 'T', Just 'G', Just 'V', Just 'C' ] ) )
            , ( 'g', ( [ Just 'f', Just 't', Just 'y', Just 'h', Just 'b', Just 'v' ], [ Just 'F', Just 'T', Just 'Y', Just 'H', Just 'B', Just 'V' ] ) )
            , ( 'h', ( [ Just 'g', Just 'y', Just 'u', Just 'j', Just 'n', Just 'b' ], [ Just 'G', Just 'Y', Just 'U', Just 'J', Just 'N', Just 'B' ] ) )
            , ( 'i', ( [ Just 'u', Just '8', Just '9', Just 'o', Just 'k', Just 'j' ], [ Just 'U', Just '*', Just '(', Just 'O', Just 'K', Just 'J' ] ) )
            , ( 'j', ( [ Just 'h', Just 'u', Just 'i', Just 'k', Just 'm', Just 'n' ], [ Just 'H', Just 'U', Just 'I', Just 'K', Just 'M', Just 'N' ] ) )
            , ( 'k', ( [ Just 'j', Just 'i', Just 'o', Just 'l', Just ',', Just 'm' ], [ Just 'J', Just 'I', Just 'O', Just 'L', Just '<', Just 'M' ] ) )
            , ( 'l', ( [ Just 'k', Just 'o', Just 'p', Just ';', Just '.', Just ',' ], [ Just 'K', Just 'O', Just 'P', Just ':', Just '>', Just '<' ] ) )
            , ( 'm', ( [ Just 'n', Just 'j', Just 'k', Just ',', Nothing, Nothing ], [ Just 'N', Just 'J', Just 'K', Just '<', Nothing, Nothing ] ) )
            , ( 'n', ( [ Just 'b', Just 'h', Just 'j', Just 'm', Nothing, Nothing ], [ Just 'B', Just 'H', Just 'J', Just 'M', Nothing, Nothing ] ) )
            , ( 'o', ( [ Just 'i', Just '9', Just '0', Just 'p', Just 'l', Just 'k' ], [ Just 'I', Just '(', Just ')', Just 'P', Just 'L', Just 'K' ] ) )
            , ( 'p', ( [ Just 'o', Just '0', Just '-', Just '[', Just ';', Just 'l' ], [ Just 'O', Just ')', Just '_', Just '{', Just ':', Just 'L' ] ) )
            , ( 'q', ( [ Nothing, Just '1', Just '2', Just 'w', Just 'a', Nothing ], [ Nothing, Just '!', Just '@', Just 'W', Just 'A', Nothing ] ) )
            , ( 'r', ( [ Just 'e', Just '4', Just '5', Just 't', Just 'f', Just 'd' ], [ Just 'E', Just '$', Just '%', Just 'T', Just 'F', Just 'D' ] ) )
            , ( 's', ( [ Just 'a', Just 'w', Just 'e', Just 'd', Just 'x', Just 'z' ], [ Just 'A', Just 'W', Just 'E', Just 'D', Just 'X', Just 'Z' ] ) )
            , ( 't', ( [ Just 'r', Just '5', Just '6', Just 'y', Just 'g', Just 'f' ], [ Just 'R', Just '%', Just '^', Just 'Y', Just 'G', Just 'F' ] ) )
            , ( 'u', ( [ Just 'y', Just '7', Just '8', Just 'i', Just 'j', Just 'h' ], [ Just 'Y', Just '&', Just '*', Just 'I', Just 'J', Just 'H' ] ) )
            , ( 'v', ( [ Just 'c', Just 'f', Just 'g', Just 'b', Nothing, Nothing ], [ Just 'C', Just 'F', Just 'G', Just 'B', Nothing, Nothing ] ) )
            , ( 'w', ( [ Just 'q', Just '2', Just '3', Just 'e', Just 's', Just 'a' ], [ Just 'Q', Just '@', Just '#', Just 'E', Just 'S', Just 'A' ] ) )
            , ( 'x', ( [ Just 'z', Just 's', Just 'd', Just 'c', Nothing, Nothing ], [ Just 'Z', Just 'S', Just 'D', Just 'C', Nothing, Nothing ] ) )
            , ( 'y', ( [ Just 't', Just '6', Just '7', Just 'u', Just 'h', Just 'g' ], [ Just 'T', Just '^', Just '&', Just 'U', Just 'H', Just 'G' ] ) )
            , ( 'z', ( [ Nothing, Just 'a', Just 's', Just 'x', Nothing, Nothing ], [ Nothing, Just 'A', Just 'S', Just 'X', Nothing, Nothing ] ) )
            , ( '{', ( [ Just 'p', Just '-', Just '=', Just ']', Just '\'', Just ';' ], [ Just 'P', Just '_', Just '+', Just '}', Just '"', Just ':' ] ) )
            , ( '|', ( [ Just ']', Nothing, Nothing, Nothing, Nothing, Nothing ], [ Just '}', Nothing, Nothing, Nothing, Nothing, Nothing ] ) )
            , ( '}', ( [ Just '[', Just '=', Nothing, Just '\\', Nothing, Just '\'' ], [ Just '{', Just '+', Nothing, Just '|', Nothing, Just '"' ] ) )
            , ( '~', ( [ Nothing, Nothing, Nothing, Just '1', Nothing, Nothing ], [ Nothing, Nothing, Nothing, Just '!', Nothing, Nothing ] ) )
            ]


{-| Adjacency graph for dvorak keyboards.
-}
dvorak : AdjacencyDict
dvorak =
    Shiftable <|
        Dict.fromList
            [ ( '!', ( [ Just '`', Nothing, Nothing, Just '2', Just '\'', Nothing ], [ Just '~', Nothing, Nothing, Just '@', Just '"', Nothing ] ) )
            , ( '"', ( [ Nothing, Just '1', Just '2', Just ',', Just 'a', Nothing ], [ Nothing, Just '!', Just '@', Just '<', Just 'A', Nothing ] ) )
            , ( '#', ( [ Just '2', Nothing, Nothing, Just '4', Just '.', Just ',' ], [ Just '@', Nothing, Nothing, Just '$', Just '>', Just '<' ] ) )
            , ( '$', ( [ Just '3', Nothing, Nothing, Just '5', Just 'p', Just '.' ], [ Just '#', Nothing, Nothing, Just '%', Just 'P', Just '>' ] ) )
            , ( '%', ( [ Just '4', Nothing, Nothing, Just '6', Just 'y', Just 'p' ], [ Just '$', Nothing, Nothing, Just '^', Just 'Y', Just 'P' ] ) )
            , ( '&', ( [ Just '6', Nothing, Nothing, Just '8', Just 'g', Just 'f' ], [ Just '^', Nothing, Nothing, Just '*', Just 'G', Just 'F' ] ) )
            , ( '\'', ( [ Nothing, Just '1', Just '2', Just ',', Just 'a', Nothing ], [ Nothing, Just '!', Just '@', Just '<', Just 'A', Nothing ] ) )
            , ( '(', ( [ Just '8', Nothing, Nothing, Just '0', Just 'r', Just 'c' ], [ Just '*', Nothing, Nothing, Just ')', Just 'R', Just 'C' ] ) )
            , ( ')', ( [ Just '9', Nothing, Nothing, Just '[', Just 'l', Just 'r' ], [ Just '(', Nothing, Nothing, Just '{', Just 'L', Just 'R' ] ) )
            , ( '*', ( [ Just '7', Nothing, Nothing, Just '9', Just 'c', Just 'g' ], [ Just '&', Nothing, Nothing, Just '(', Just 'C', Just 'G' ] ) )
            , ( '+', ( [ Just '/', Just ']', Nothing, Just '\\', Nothing, Just '-' ], [ Just '?', Just '}', Nothing, Just '|', Nothing, Just '_' ] ) )
            , ( ',', ( [ Just '\'', Just '2', Just '3', Just '.', Just 'o', Just 'a' ], [ Just '"', Just '@', Just '#', Just '>', Just 'O', Just 'A' ] ) )
            , ( '-', ( [ Just 's', Just '/', Just '=', Nothing, Nothing, Just 'z' ], [ Just 'S', Just '?', Just '+', Nothing, Nothing, Just 'Z' ] ) )
            , ( '.', ( [ Just ',', Just '3', Just '4', Just 'p', Just 'e', Just 'o' ], [ Just '<', Just '#', Just '$', Just 'P', Just 'E', Just 'O' ] ) )
            , ( '/', ( [ Just 'l', Just '[', Just ']', Just '=', Just '-', Just 's' ], [ Just 'L', Just '{', Just '}', Just '+', Just '_', Just 'S' ] ) )
            , ( '0', ( [ Just '9', Nothing, Nothing, Just '[', Just 'l', Just 'r' ], [ Just '(', Nothing, Nothing, Just '{', Just 'L', Just 'R' ] ) )
            , ( '1', ( [ Just '`', Nothing, Nothing, Just '2', Just '\'', Nothing ], [ Just '~', Nothing, Nothing, Just '@', Just '"', Nothing ] ) )
            , ( '2', ( [ Just '1', Nothing, Nothing, Just '3', Just ',', Just '\'' ], [ Just '!', Nothing, Nothing, Just '#', Just '<', Just '"' ] ) )
            , ( '3', ( [ Just '2', Nothing, Nothing, Just '4', Just '.', Just ',' ], [ Just '@', Nothing, Nothing, Just '$', Just '>', Just '<' ] ) )
            , ( '4', ( [ Just '3', Nothing, Nothing, Just '5', Just 'p', Just '.' ], [ Just '#', Nothing, Nothing, Just '%', Just 'P', Just '>' ] ) )
            , ( '5', ( [ Just '4', Nothing, Nothing, Just '6', Just 'y', Just 'p' ], [ Just '$', Nothing, Nothing, Just '^', Just 'Y', Just 'P' ] ) )
            , ( '6', ( [ Just '5', Nothing, Nothing, Just '7', Just 'f', Just 'y' ], [ Just '%', Nothing, Nothing, Just '&', Just 'F', Just 'Y' ] ) )
            , ( '7', ( [ Just '6', Nothing, Nothing, Just '8', Just 'g', Just 'f' ], [ Just '^', Nothing, Nothing, Just '*', Just 'G', Just 'F' ] ) )
            , ( '8', ( [ Just '7', Nothing, Nothing, Just '9', Just 'c', Just 'g' ], [ Just '&', Nothing, Nothing, Just '(', Just 'C', Just 'G' ] ) )
            , ( '9', ( [ Just '8', Nothing, Nothing, Just '0', Just 'r', Just 'c' ], [ Just '*', Nothing, Nothing, Just ')', Just 'R', Just 'C' ] ) )
            , ( ':', ( [ Nothing, Just 'a', Just 'o', Just 'q', Nothing, Nothing ], [ Nothing, Just 'A', Just 'O', Just 'Q', Nothing, Nothing ] ) )
            , ( ';', ( [ Nothing, Just 'a', Just 'o', Just 'q', Nothing, Nothing ], [ Nothing, Just 'A', Just 'O', Just 'Q', Nothing, Nothing ] ) )
            , ( '<', ( [ Just '\'', Just '2', Just '3', Just '.', Just 'o', Just 'a' ], [ Just '"', Just '@', Just '#', Just '>', Just 'O', Just 'A' ] ) )
            , ( '=', ( [ Just '/', Just ']', Nothing, Just '\\', Nothing, Just '-' ], [ Just '?', Just '}', Nothing, Just '|', Nothing, Just '_' ] ) )
            , ( '>', ( [ Just ',', Just '3', Just '4', Just 'p', Just 'e', Just 'o' ], [ Just '<', Just '#', Just '$', Just 'P', Just 'E', Just 'O' ] ) )
            , ( '?', ( [ Just 'l', Just '[', Just ']', Just '=', Just '-', Just 's' ], [ Just 'L', Just '{', Just '}', Just '+', Just '_', Just 'S' ] ) )
            , ( '@', ( [ Just '1', Nothing, Nothing, Just '3', Just ',', Just '\'' ], [ Just '!', Nothing, Nothing, Just '#', Just '<', Just '"' ] ) )
            , ( 'A', ( [ Nothing, Just '\'', Just ',', Just 'o', Just ';', Nothing ], [ Nothing, Just '"', Just '<', Just 'O', Just ':', Nothing ] ) )
            , ( 'B', ( [ Just 'x', Just 'd', Just 'h', Just 'm', Nothing, Nothing ], [ Just 'X', Just 'D', Just 'H', Just 'M', Nothing, Nothing ] ) )
            , ( 'C', ( [ Just 'g', Just '8', Just '9', Just 'r', Just 't', Just 'h' ], [ Just 'G', Just '*', Just '(', Just 'R', Just 'T', Just 'H' ] ) )
            , ( 'D', ( [ Just 'i', Just 'f', Just 'g', Just 'h', Just 'b', Just 'x' ], [ Just 'I', Just 'F', Just 'G', Just 'H', Just 'B', Just 'X' ] ) )
            , ( 'E', ( [ Just 'o', Just '.', Just 'p', Just 'u', Just 'j', Just 'q' ], [ Just 'O', Just '>', Just 'P', Just 'U', Just 'J', Just 'Q' ] ) )
            , ( 'F', ( [ Just 'y', Just '6', Just '7', Just 'g', Just 'd', Just 'i' ], [ Just 'Y', Just '^', Just '&', Just 'G', Just 'D', Just 'I' ] ) )
            , ( 'G', ( [ Just 'f', Just '7', Just '8', Just 'c', Just 'h', Just 'd' ], [ Just 'F', Just '&', Just '*', Just 'C', Just 'H', Just 'D' ] ) )
            , ( 'H', ( [ Just 'd', Just 'g', Just 'c', Just 't', Just 'm', Just 'b' ], [ Just 'D', Just 'G', Just 'C', Just 'T', Just 'M', Just 'B' ] ) )
            , ( 'I', ( [ Just 'u', Just 'y', Just 'f', Just 'd', Just 'x', Just 'k' ], [ Just 'U', Just 'Y', Just 'F', Just 'D', Just 'X', Just 'K' ] ) )
            , ( 'J', ( [ Just 'q', Just 'e', Just 'u', Just 'k', Nothing, Nothing ], [ Just 'Q', Just 'E', Just 'U', Just 'K', Nothing, Nothing ] ) )
            , ( 'K', ( [ Just 'j', Just 'u', Just 'i', Just 'x', Nothing, Nothing ], [ Just 'J', Just 'U', Just 'I', Just 'X', Nothing, Nothing ] ) )
            , ( 'L', ( [ Just 'r', Just '0', Just '[', Just '/', Just 's', Just 'n' ], [ Just 'R', Just ')', Just '{', Just '?', Just 'S', Just 'N' ] ) )
            , ( 'M', ( [ Just 'b', Just 'h', Just 't', Just 'w', Nothing, Nothing ], [ Just 'B', Just 'H', Just 'T', Just 'W', Nothing, Nothing ] ) )
            , ( 'N', ( [ Just 't', Just 'r', Just 'l', Just 's', Just 'v', Just 'w' ], [ Just 'T', Just 'R', Just 'L', Just 'S', Just 'V', Just 'W' ] ) )
            , ( 'O', ( [ Just 'a', Just ',', Just '.', Just 'e', Just 'q', Just ';' ], [ Just 'A', Just '<', Just '>', Just 'E', Just 'Q', Just ':' ] ) )
            , ( 'P', ( [ Just '.', Just '4', Just '5', Just 'y', Just 'u', Just 'e' ], [ Just '>', Just '$', Just '%', Just 'Y', Just 'U', Just 'E' ] ) )
            , ( 'Q', ( [ Just ';', Just 'o', Just 'e', Just 'j', Nothing, Nothing ], [ Just ':', Just 'O', Just 'E', Just 'J', Nothing, Nothing ] ) )
            , ( 'R', ( [ Just 'c', Just '9', Just '0', Just 'l', Just 'n', Just 't' ], [ Just 'C', Just '(', Just ')', Just 'L', Just 'N', Just 'T' ] ) )
            , ( 'S', ( [ Just 'n', Just 'l', Just '/', Just '-', Just 'z', Just 'v' ], [ Just 'N', Just 'L', Just '?', Just '_', Just 'Z', Just 'V' ] ) )
            , ( 'T', ( [ Just 'h', Just 'c', Just 'r', Just 'n', Just 'w', Just 'm' ], [ Just 'H', Just 'C', Just 'R', Just 'N', Just 'W', Just 'M' ] ) )
            , ( 'U', ( [ Just 'e', Just 'p', Just 'y', Just 'i', Just 'k', Just 'j' ], [ Just 'E', Just 'P', Just 'Y', Just 'I', Just 'K', Just 'J' ] ) )
            , ( 'V', ( [ Just 'w', Just 'n', Just 's', Just 'z', Nothing, Nothing ], [ Just 'W', Just 'N', Just 'S', Just 'Z', Nothing, Nothing ] ) )
            , ( 'W', ( [ Just 'm', Just 't', Just 'n', Just 'v', Nothing, Nothing ], [ Just 'M', Just 'T', Just 'N', Just 'V', Nothing, Nothing ] ) )
            , ( 'X', ( [ Just 'k', Just 'i', Just 'd', Just 'b', Nothing, Nothing ], [ Just 'K', Just 'I', Just 'D', Just 'B', Nothing, Nothing ] ) )
            , ( 'Y', ( [ Just 'p', Just '5', Just '6', Just 'f', Just 'i', Just 'u' ], [ Just 'P', Just '%', Just '^', Just 'F', Just 'I', Just 'U' ] ) )
            , ( 'Z', ( [ Just 'v', Just 's', Just '-', Nothing, Nothing, Nothing ], [ Just 'V', Just 'S', Just '_', Nothing, Nothing, Nothing ] ) )
            , ( '[', ( [ Just '0', Nothing, Nothing, Just ']', Just '/', Just 'l' ], [ Just ')', Nothing, Nothing, Just '}', Just '?', Just 'L' ] ) )
            , ( '\\', ( [ Just '=', Nothing, Nothing, Nothing, Nothing, Nothing ], [ Just '+', Nothing, Nothing, Nothing, Nothing, Nothing ] ) )
            , ( ']', ( [ Just '[', Nothing, Nothing, Nothing, Just '=', Just '/' ], [ Just '{', Nothing, Nothing, Nothing, Just '+', Just '?' ] ) )
            , ( '^', ( [ Just '5', Nothing, Nothing, Just '7', Just 'f', Just 'y' ], [ Just '%', Nothing, Nothing, Just '&', Just 'F', Just 'Y' ] ) )
            , ( '_', ( [ Just 's', Just '/', Just '=', Nothing, Nothing, Just 'z' ], [ Just 'S', Just '?', Just '+', Nothing, Nothing, Just 'Z' ] ) )
            , ( '`', ( [ Nothing, Nothing, Nothing, Just '1', Nothing, Nothing ], [ Nothing, Nothing, Nothing, Just '!', Nothing, Nothing ] ) )
            , ( 'a', ( [ Nothing, Just '\'', Just ',', Just 'o', Just ';', Nothing ], [ Nothing, Just '"', Just '<', Just 'O', Just ':', Nothing ] ) )
            , ( 'b', ( [ Just 'x', Just 'd', Just 'h', Just 'm', Nothing, Nothing ], [ Just 'X', Just 'D', Just 'H', Just 'M', Nothing, Nothing ] ) )
            , ( 'c', ( [ Just 'g', Just '8', Just '9', Just 'r', Just 't', Just 'h' ], [ Just 'G', Just '*', Just '(', Just 'R', Just 'T', Just 'H' ] ) )
            , ( 'd', ( [ Just 'i', Just 'f', Just 'g', Just 'h', Just 'b', Just 'x' ], [ Just 'I', Just 'F', Just 'G', Just 'H', Just 'B', Just 'X' ] ) )
            , ( 'e', ( [ Just 'o', Just '.', Just 'p', Just 'u', Just 'j', Just 'q' ], [ Just 'O', Just '>', Just 'P', Just 'U', Just 'J', Just 'Q' ] ) )
            , ( 'f', ( [ Just 'y', Just '6', Just '7', Just 'g', Just 'd', Just 'i' ], [ Just 'Y', Just '^', Just '&', Just 'G', Just 'D', Just 'I' ] ) )
            , ( 'g', ( [ Just 'f', Just '7', Just '8', Just 'c', Just 'h', Just 'd' ], [ Just 'F', Just '&', Just '*', Just 'C', Just 'H', Just 'D' ] ) )
            , ( 'h', ( [ Just 'd', Just 'g', Just 'c', Just 't', Just 'm', Just 'b' ], [ Just 'D', Just 'G', Just 'C', Just 'T', Just 'M', Just 'B' ] ) )
            , ( 'i', ( [ Just 'u', Just 'y', Just 'f', Just 'd', Just 'x', Just 'k' ], [ Just 'U', Just 'Y', Just 'F', Just 'D', Just 'X', Just 'K' ] ) )
            , ( 'j', ( [ Just 'q', Just 'e', Just 'u', Just 'k', Nothing, Nothing ], [ Just 'Q', Just 'E', Just 'U', Just 'K', Nothing, Nothing ] ) )
            , ( 'k', ( [ Just 'j', Just 'u', Just 'i', Just 'x', Nothing, Nothing ], [ Just 'J', Just 'U', Just 'I', Just 'X', Nothing, Nothing ] ) )
            , ( 'l', ( [ Just 'r', Just '0', Just '[', Just '/', Just 's', Just 'n' ], [ Just 'R', Just ')', Just '{', Just '?', Just 'S', Just 'N' ] ) )
            , ( 'm', ( [ Just 'b', Just 'h', Just 't', Just 'w', Nothing, Nothing ], [ Just 'B', Just 'H', Just 'T', Just 'W', Nothing, Nothing ] ) )
            , ( 'n', ( [ Just 't', Just 'r', Just 'l', Just 's', Just 'v', Just 'w' ], [ Just 'T', Just 'R', Just 'L', Just 'S', Just 'V', Just 'W' ] ) )
            , ( 'o', ( [ Just 'a', Just ',', Just '.', Just 'e', Just 'q', Just ';' ], [ Just 'A', Just '<', Just '>', Just 'E', Just 'Q', Just ':' ] ) )
            , ( 'p', ( [ Just '.', Just '4', Just '5', Just 'y', Just 'u', Just 'e' ], [ Just '>', Just '$', Just '%', Just 'Y', Just 'U', Just 'E' ] ) )
            , ( 'q', ( [ Just ';', Just 'o', Just 'e', Just 'j', Nothing, Nothing ], [ Just ':', Just 'O', Just 'E', Just 'J', Nothing, Nothing ] ) )
            , ( 'r', ( [ Just 'c', Just '9', Just '0', Just 'l', Just 'n', Just 't' ], [ Just 'C', Just '(', Just ')', Just 'L', Just 'N', Just 'T' ] ) )
            , ( 's', ( [ Just 'n', Just 'l', Just '/', Just '-', Just 'z', Just 'v' ], [ Just 'N', Just 'L', Just '?', Just '_', Just 'Z', Just 'V' ] ) )
            , ( 't', ( [ Just 'h', Just 'c', Just 'r', Just 'n', Just 'w', Just 'm' ], [ Just 'H', Just 'C', Just 'R', Just 'N', Just 'W', Just 'M' ] ) )
            , ( 'u', ( [ Just 'e', Just 'p', Just 'y', Just 'i', Just 'k', Just 'j' ], [ Just 'E', Just 'P', Just 'Y', Just 'I', Just 'K', Just 'J' ] ) )
            , ( 'v', ( [ Just 'w', Just 'n', Just 's', Just 'z', Nothing, Nothing ], [ Just 'W', Just 'N', Just 'S', Just 'Z', Nothing, Nothing ] ) )
            , ( 'w', ( [ Just 'm', Just 't', Just 'n', Just 'v', Nothing, Nothing ], [ Just 'M', Just 'T', Just 'N', Just 'V', Nothing, Nothing ] ) )
            , ( 'x', ( [ Just 'k', Just 'i', Just 'd', Just 'b', Nothing, Nothing ], [ Just 'K', Just 'I', Just 'D', Just 'B', Nothing, Nothing ] ) )
            , ( 'y', ( [ Just 'p', Just '5', Just '6', Just 'f', Just 'i', Just 'u' ], [ Just 'P', Just '%', Just '^', Just 'F', Just 'I', Just 'U' ] ) )
            , ( 'z', ( [ Just 'v', Just 's', Just '-', Nothing, Nothing, Nothing ], [ Just 'V', Just 'S', Just '_', Nothing, Nothing, Nothing ] ) )
            , ( '{', ( [ Just '0', Nothing, Nothing, Just ']', Just '/', Just 'l' ], [ Just ')', Nothing, Nothing, Just '}', Just '?', Just 'L' ] ) )
            , ( '|', ( [ Just '=', Nothing, Nothing, Nothing, Nothing, Nothing ], [ Just '+', Nothing, Nothing, Nothing, Nothing, Nothing ] ) )
            , ( '}', ( [ Just '[', Nothing, Nothing, Nothing, Just '=', Just '/' ], [ Just '{', Nothing, Nothing, Nothing, Just '+', Just '?' ] ) )
            , ( '~', ( [ Nothing, Nothing, Nothing, Just '1', Nothing, Nothing ], [ Nothing, Nothing, Nothing, Just '!', Nothing, Nothing ] ) )
            ]


{-| Adjacency graph for 10-key keypads.
-}
keypad : AdjacencyDict
keypad =
    Unshiftable <|
        Dict.fromList
            [ ( '*', [ Just '/', Nothing, Nothing, Nothing, Just '-', Just '+', Just '9', Just '8' ] )
            , ( '+', [ Just '9', Just '*', Just '-', Nothing, Nothing, Nothing, Nothing, Just '6' ] )
            , ( '-', [ Just '*', Nothing, Nothing, Nothing, Nothing, Nothing, Just '+', Just '9' ] )
            , ( '.', [ Just '0', Just '2', Just '3', Nothing, Nothing, Nothing, Nothing, Nothing ] )
            , ( '/', [ Nothing, Nothing, Nothing, Nothing, Just '*', Just '9', Just '8', Just '7' ] )
            , ( '0', [ Nothing, Just '1', Just '2', Just '3', Just '.', Nothing, Nothing, Nothing ] )
            , ( '1', [ Nothing, Nothing, Just '4', Just '5', Just '2', Just '0', Nothing, Nothing ] )
            , ( '2', [ Just '1', Just '4', Just '5', Just '6', Just '3', Just '.', Just '0', Nothing ] )
            , ( '3', [ Just '2', Just '5', Just '6', Nothing, Nothing, Nothing, Just '.', Just '0' ] )
            , ( '4', [ Nothing, Nothing, Just '7', Just '8', Just '5', Just '2', Just '1', Nothing ] )
            , ( '5', [ Just '4', Just '7', Just '8', Just '9', Just '6', Just '3', Just '2', Just '1' ] )
            , ( '6', [ Just '5', Just '8', Just '9', Just '+', Nothing, Nothing, Just '3', Just '2' ] )
            , ( '7', [ Nothing, Nothing, Nothing, Just '/', Just '8', Just '5', Just '4', Nothing ] )
            , ( '8', [ Just '7', Nothing, Just '/', Just '*', Just '9', Just '6', Just '5', Just '4' ] )
            , ( '9', [ Just '8', Just '/', Just '*', Just '-', Just '+', Nothing, Just '6', Just '5' ] )
            ]


{-| Adjacency graph for mac 10-key keypads.
-}
macKeypad : AdjacencyDict
macKeypad =
    Unshiftable <|
        Dict.fromList
            [ ( '*', [ Just '/', Nothing, Nothing, Nothing, Nothing, Nothing, Just '-', Just '9' ] )
            , ( '+', [ Just '6', Just '9', Just '-', Nothing, Nothing, Nothing, Nothing, Just '3' ] )
            , ( '-', [ Just '9', Just '/', Just '*', Nothing, Nothing, Nothing, Just '+', Just '6' ] )
            , ( '.', [ Just '0', Just '2', Just '3', Nothing, Nothing, Nothing, Nothing, Nothing ] )
            , ( '/', [ Just '=', Nothing, Nothing, Nothing, Just '*', Just '-', Just '9', Just '8' ] )
            , ( '0', [ Nothing, Just '1', Just '2', Just '3', Just '.', Nothing, Nothing, Nothing ] )
            , ( '1', [ Nothing, Nothing, Just '4', Just '5', Just '2', Just '0', Nothing, Nothing ] )
            , ( '2', [ Just '1', Just '4', Just '5', Just '6', Just '3', Just '.', Just '0', Nothing ] )
            , ( '3', [ Just '2', Just '5', Just '6', Just '+', Nothing, Nothing, Just '.', Just '0' ] )
            , ( '4', [ Nothing, Nothing, Just '7', Just '8', Just '5', Just '2', Just '1', Nothing ] )
            , ( '5', [ Just '4', Just '7', Just '8', Just '9', Just '6', Just '3', Just '2', Just '1' ] )
            , ( '6', [ Just '5', Just '8', Just '9', Just '-', Just '+', Nothing, Just '3', Just '2' ] )
            , ( '7', [ Nothing, Nothing, Nothing, Just '=', Just '8', Just '5', Just '4', Nothing ] )
            , ( '8', [ Just '7', Nothing, Just '=', Just '/', Just '9', Just '6', Just '5', Just '4' ] )
            , ( '9', [ Just '8', Just '=', Just '/', Just '*', Just '-', Just '+', Just '6', Just '5' ] )
            , ( '=', [ Nothing, Nothing, Nothing, Nothing, Just '/', Just '9', Just '8', Just '7' ] )
            ]
