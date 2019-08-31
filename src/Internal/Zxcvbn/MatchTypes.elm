module Internal.Zxcvbn.MatchTypes exposing (Match, TranslationDict, sortMatches)

{-| Contains various types that must be passed around between internal modules.
-}

import Dict exposing (Dict)
import Zxcvbn.MatchTypes exposing (MatchDetails)


{-| `Match` type returned by the various matching functions, before being scored for guesses.

  - `pattern` -- The type of the match, i.e. a dictionary match versus a date match.
  - `token` -- the portion of the password that triggered the match.
  - `i`, `j` -- The start/end positions of `token` in the original password.

-}
type alias Match =
    { pattern : MatchDetails
    , token : String
    , i : Int
    , j : Int
    }


{-| Dict specifying direct character substitutions.
-}
type alias TranslationDict =
    Dict Char Char


{-| Sort matches on `i` primary, `j` secondary.
-}
sortMatches : List { a | i : Int, j : Int } -> List { a | i : Int, j : Int }
sortMatches l =
    List.sortWith orderMatches l



-- * Non-exposed below here


{-| Compares matches based first on `i` and then on `j`.
-}
orderMatches : { a | i : Int, j : Int } -> { a | i : Int, j : Int } -> Order
orderMatches m1 m2 =
    let
        compareI =
            compare m1.i m2.i
    in
    case compareI of
        EQ ->
            compare m1.j m2.j

        _ ->
            compareI
