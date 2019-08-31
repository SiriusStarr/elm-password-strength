module Zxcvbn.MatchTypes exposing (ScoredMatch, MatchDetails(..), DictionaryDetails(..))

{-| This module is not necessary for normal use of the `Zxcvbn` module; instead, it contains the types necessary for examining the actual matches in the most-guessable sequence contained in `Zxcvbn.sequence`.


## Types

@docs ScoredMatch, MatchDetails, DictionaryDetails

-}

import Dict exposing (Dict)
import Regex


{-| Matches after estimated guesses have been calculated.

  - `pattern` -- The type of the match, i.e. a dictionary match versus a date match.
  - `token` -- the portion of the password that triggered the match.
  - `i`, `j` -- The start/end positions of `token` in the original password.
  - `guesses` -- The estimated number of guesses to "crack" a match.
  - `guessesLog10` -- The order of magnitude of the estimated number of guesses to crack the match.

-}
type alias ScoredMatch =
    { pattern : MatchDetails
    , token : String
    , i : Int
    , j : Int
    , guesses : Int
    , guessesLog10 : Float
    }


{-| `MatchDetails` type for holding all of the fields only relevant to specific types of matches.

  - `DictionaryMatch` -- A match against common password, name, or English word dictionaries.
      - `dictionaryDetails` -- Details of the dictionary match, e.g. whether it was a reverse match or not.
      - `dictionaryName` -- The name of the dictionary that was matched against, e.g. "englishWikipedia"
      - `rank` -- The frequency rank of the match, with 1 being the most common.
      - `matchedWord` -- The word in the dictionary that was matched.

  - `SpatialMatch` -- A match against sequences on the keyboard, e.g. "asdfg".
      - `graph` -- The name of the specific keyboard layout that was matched, e.g. "qwerty" or "dvorak".
      - `turns` -- Number of directions that the sequence travels in, e.g. "zxcdert" has 3.
      - `shiftedCount` -- Number of characters that are the shifted version (e.g. 'A' instead of 'a').

  - `SequenceMatch` -- A match against repetitive increases in unicode code point, e.g. "abcdefg" or "aceg".
      - `sequenceName` -- The name of the matched sequence; will be "lower", "upper", "digits", or "unicode"
      - `sequenceSpace` -- The number of "characters" in the sequence, e.g. 26 for alphabetical or 10 for digits. Defaults to 26 for unicode per the reference implementation.
      - `ascending` -- Whether the match is ascending, e.g. "12345" or descending, e.g. "54321".

  - `RepeatMatch` -- A match against 2 or more repeats, e.g. "abcabcabc"
      - `baseToken` -- The base repeating unit, e.g. "abc" in "abcabcabc"
      - `baseGuesses` -- The estimated number of guesses to "crack" the base token.
      - `baseMatches` -- All matches for the base token.
      - `repeatCount` -- The number of times the base token is repeated.

  - `RegexMatch` -- A match agains a regex. Per reference implementation, this only matched 4-digit years.
      - `regexName` -- The name of the regex that matched. Per reference implementation, this will only ever be "recentYear"
      - `regexMatch` -- The regex match object for the match.

  - `DateMatch` -- A match on an apparent numeric date, with or without separators.
      - `separator` -- The separator of the date, e.g. "/" for "5/5/1955"; may be an empty string if no separator.
      - `year`, `month`, `day` -- The date matched.

  - `BruteforceMatch` -- A segment of password that didn't match any other match and thus would have to be bruteforced.

-}
type MatchDetails
    = DictionaryMatch
        { dictionaryDetails : DictionaryDetails
        , dictionaryName : String
        , rank : Int
        , matchedWord : String
        }
    | SpatialMatch
        { graph : String
        , turns : Int
        , shiftedCount : Int
        }
    | SequenceMatch
        { sequenceName : String
        , sequenceSpace : Int
        , ascending : Bool
        }
    | RepeatMatch
        { baseToken : String
        , baseGuesses : Int
        , baseMatches : List ScoredMatch
        , repeatCount : Int
        }
    | RegexMatch
        { regexName : String
        , regexMatch : Regex.Match
        }
    | DateMatch
        { separator : String
        , year : Int
        , month : Int
        , day : Int
        }
    | BruteforceMatch


{-| `DictionaryDetails` for specifying which kind of dictionary match a specific match is and any other details (e.g. which substitutions were made for l33t matches).

  - `Dictionary` -- A standard dictionary match.
  - `ReverseDictionary` -- A dictionary match where the token in the password was reversed, e.g. "drowssap" matches "password".
  - `L33tDictionary` -- A dictionary match featuring common "l33t" substitutions, e.g. "p@ssw0rd" matches "password".
      - `sub` -- A dictionary with keys of characters in the password and values of characters they were substituted with to make the match, e.g. `Dict.fromList [ ( '@', 'a' ), ( '0', 'o' ) ) ]` for "p@ssw0rd".
      - `subDisplay` -- A human-readable description of the substitutions listed in `sub`.

-}
type DictionaryDetails
    = Dictionary
    | ReverseDictionary
    | L33tDictionary { sub : Dict Char Char, subDisplay : String }
