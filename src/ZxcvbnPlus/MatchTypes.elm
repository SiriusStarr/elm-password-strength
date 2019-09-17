module ZxcvbnPlus.MatchTypes exposing
    ( ScoredMatch, MatchDetails(..)
    , DictionaryDetails(..), Dictionary(..), EnglishSource(..), NameType(..)
    , DateDetails(..)
    , KeyboardLayout(..)
    , OrdinalSequence(..)
    )

{-| This module is not necessary for normal use of the `ZxcvbnPlus` module; instead, it contains the types necessary for examining the actual matches in the most-guessable sequence contained in `ZxcvbnPlusResult.matchSequence`.


## Match Types

@docs ScoredMatch, MatchDetails


## Dictionary Match Specific

@docs DictionaryDetails, Dictionary, EnglishSource, NameType


## Date Match Specific

@docs DateDetails


## Key Adjacency Match Specific

@docs KeyboardLayout


## Ordinal Sequence Specific

@docs OrdinalSequence

-}

import Dict exposing (Dict)


{-| A match for which estimated guesses have been calculated.

  - `pattern` -- The type of the match, e.g. a dictionary match versus a date match.
  - `token` -- the portion of the password that triggered the match.
  - `i`, `j` -- The start/end positions of `token` in the original password.
  - `guesses` -- The estimated number of guesses to "crack" the match.

-}
type alias ScoredMatch =
    { pattern : MatchDetails
    , token : String
    , i : Int
    , j : Int
    , guesses : Int
    }


{-| `MatchDetails` type to specify the type and details of a specific match.

  - `DictionaryMatch` -- A match against common password, name, or English word dictionaries.
      - `dictionaryDetails` -- Details of the dictionary match, e.g. whether it was a reverse match or not.
      - `dictionary` -- The dictionary source that was matched against, e.g. `CommonPassword` or `EnglishWord`.
      - `rank` -- The frequency rank of the match, with 1 being the most common.
      - `matchedWord` -- The word in the dictionary that was matched.

  - `KeyAdjacencyMatch` -- A match against sequences on the keyboard, e.g. "asdfg".
      - `layout` -- The name specific keyboard layout that was matched, e.g. `Qwerty` or `Dvorak`.
      - `turns` -- Number of directions that the sequence travels in, e.g. "zxcdert" has 3.
      - `shiftedKeys` -- Number of characters that are the shifted version (e.g. 'A' instead of 'a').

  - `OrdinalSequenceMatch` -- A match against repetitive increases in unicode code point, e.g. "abcdefg" or "aceg".
      - `sequence` -- The matched sequence, e.g. `LowercaseLatin` or `Unicode`.
      - `delta` -- The offset betwen codepoints; may be positive or negative. For example, "zyxw" has `delta` of `-1`, and "aceg" has `delta` of 2.
      - `obviousStart` -- Whether the matched sequence begins somewhere "obvious", e.g. "0" or "a".

  - `RepeatMatch` -- A match against 2 or more repeats, e.g. "abcabcabc"
      - `baseToken` -- The base repeating unit, e.g. "abc" in "abcabcabc"
      - `baseGuesses` -- The estimated number of guesses to "crack" the base token.
      - `baseMatchSequence` -- The estimated most-guessable match sequence for the base token.
      - `repeatCount` -- The number of times the base token is repeated, e.g. 3 in "ababab".

  - `DateMatch` -- A match on an apparent numeric date.
      - `year` -- The year matched, e.g. 1954 for "1/1/1954".
      - `dateDetails` -- The details of the found date, e.g. month and day, separator, whether it's only a recent year, etc.

  - `BruteforceMatch` -- A segment of password that didn't match any other match and thus would have to be bruteforced.

-}
type MatchDetails
    = DictionaryMatch
        { dictionaryDetails : DictionaryDetails
        , dictionary : Dictionary
        , rank : Int
        , matchedWord : String
        }
    | KeyAdjacencyMatch
        { layout : KeyboardLayout
        , turns : Int
        , shiftedKeys : Int
        }
    | OrdinalSequenceMatch
        { sequence : OrdinalSequence
        , delta : Int
        , obviousStart : Bool
        }
    | RepeatMatch
        { baseToken : String
        , baseGuesses : Int
        , baseMatchSequence : List ScoredMatch
        , repeatCount : Int
        }
    | DateMatch
        { dateDetails : DateDetails
        , year : Int
        }
    | BruteforceMatch


{-| The specific details for a date match.

  - `RecentYear` -- A 4-digit year only, from 1900-present.
  - `FullDate` -- A full date, specifying a month and day, as well as a possible separator character (e.g. '/').

-}
type DateDetails
    = RecentYear
    | FullDate
        { separator : Maybe Char
        , month : Int
        , day : Int
        }


{-| The specific ordinal sequence of characters that was matched.

  - `UppercaseLatin` -- A sequence like "ABCDEF".
  - `LowercaseLatin` -- A sequence like "zyxwv".
  - `Digits` -- A sequence like "5678".
  - `Unicode` -- Any other sequence, e.g. "ㄅㄆㄇㄈ".

-}
type OrdinalSequence
    = UppercaseLatin
    | LowercaseLatin
    | Digits
    | Unicode


{-| The specific keyboard layout a key adjacency match was made against.
-}
type KeyboardLayout
    = Qwerty
    | Dvorak
    | Keypad
    | MacKeypad


{-| The source for a dictionary match.

  - `CommonPassword` -- Matched against a list of the 30,000 most-common passwords.
  - `EnglishWord` -- Matched a common English-language word.
  - `Name` -- Matched a common (English-language) given name/surname.
  - `UserPattern` -- Matched one of the strings provided to `zxcvbnPlus` (typically site-specific words and/or user information).

-}
type Dictionary
    = CommonPassword
    | EnglishWord EnglishSource
    | Name NameType
    | UserPattern


{-| The type of name (e.g. surname, female given name, etc.) a common name is.
-}
type NameType
    = Surname
    | MaleName
    | FemaleName


{-| The frequency list against which an English word was matched.
-}
type EnglishSource
    = Wikipedia
    | TvAndFilm


{-| `DictionaryDetails` specifies which kind of dictionary match a specific match is and any other relevant details (e.g. which substitutions were made for l33t matches).

  - `Dictionary` -- A standard dictionary match.
  - `ReverseDictionary` -- A dictionary match where the token in the password was reversed, e.g. "drowssap" matches "password".
  - `L33tDictionary` -- A dictionary match featuring common "l33t" substitutions, e.g. "p@ssw0rd" matches "password".
      - `sub` -- A dictionary with keys of characters in the password and values of characters they were substituted with to make the match, e.g. `Dict.fromList [ ( '@', 'a' ), ( '0', 'o' ) ) ]` for "p@ssw0rd".

-}
type DictionaryDetails
    = NormalDictionary
    | ReverseDictionary
    | L33tDictionary (Dict Char Char)
