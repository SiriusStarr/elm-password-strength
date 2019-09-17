module Internal.ZxcvbnPlus.MatchTypes exposing (Match, TranslationDict)

{-| Contains various types that must be passed around between internal modules.
-}

import Dict exposing (Dict)
import ZxcvbnPlus.MatchTypes exposing (MatchDetails)


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
