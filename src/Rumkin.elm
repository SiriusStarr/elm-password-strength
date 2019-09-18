module Rumkin exposing
    ( getStats, RumkinResult, Strength(..)
    , getCustomStats, parseFrequencyList, parseCommonList
    )

{-| This module implements the popular F/OSS password strength check available at [rumkin.com](http://rumkin.com/tools/password/passchk.php). A more detailed description may be found at that link, as well as the rationale behind it.


## Usage Notes

The results returned by this module should be identical to those obtained from the live version of the strength test at [rumkin.com](http://rumkin.com/tools/password/passchk.php). They will, however, differ from the version available as source for download, as there is a bug in which characters in keyboard punctuation \`~-\_=+[{]}|;:'",<.>/?\\ contribute an additional sequence space of only 20 instead of the correct 22. As this bug is fixed in the live version, it is fixed in this package as well.

**Warning:** Common password warnings returned by this package are _not_ factored into the overall strength of the password. As such, while a warning of "Common password!" will be returned for the password "raidersofthelostark", its strength will be `Strong` and its supposed entropy 70.5 bits, despite it being a weak, common password. Consider using `Zxcvbn`/`ZxcvbnPlus` if you require common passwords to impact the overall score and not merely be presented as warnings.

The computational performance of this library is quite good, with essentially instantaneous results.

While it does contain a common word list, the library is relatively light. Sizes for an extremely simple example application follow:

  - Compiled size -- 170 kB
  - Minified size -- 67 kB
  - Gzipped size -- 32 kB


## Normal Usage

For normal usage of this module, only the function `getStats` and the returned types `RumkinResult` and `Strength` are necessary.

@docs getStats, RumkinResult, Strength


## Custom Usage

The sequence frequency and common password lists used by this package are compressed and require parsing prior to use. If you wish to control exactly when that happens (and cache it in your model), the following functions are available.

@docs getCustomStats, parseFrequencyList, parseCommonList

-}

import Array exposing (Array)
import Internal.Rumkin exposing (CommonPasswordList(..), FrequencyList(..), getCharsetSize)
import Parser as P exposing ((|.), (|=), Parser)
import Rumkin.Common exposing (commonList)
import Rumkin.Frequency exposing (frequencyList)
import Set exposing (Set)


{-| Given a password, check it for common passwords and score it, returning the result.
-}
getStats : String -> RumkinResult
getStats =
    getCustomStats defaultFrequencyList defaultCommonList


{-| The results returned by `getStats`, containing the following fields:

  - `warnings` -- A (possible empty) list of printed warnings, suitable for display to the user. If the password is a common one, a warning will be contained in this list, as will warnings about length.
  - `length` -- The length of the password.
  - `strength` -- The overall strength of the password, e.g. for display as a growing bar. This does **not** take into account common passwords.
  - `strengthComment` -- A comment on the strength of the password, suitable for display to the user; a list of the comments is found in the documentation for `Strength`.
  - `entropy` -- The estimated bits of entropy of the password; this does **not** take into account common passwords, and so the actual entropy is quite possibly considerably lower.
  - `charsetSize` -- The total sequence space of the password, e.g. "kjethrsfdsdgf" would be 26, since it consists solely of lowercase Latin characters. This is _not_ Unicode aware, so it is conservative when outside the "standard" range.

-}
type alias RumkinResult =
    { warnings : List String
    , length : Int
    , strength : Strength
    , strengthComment : String
    , entropy : Float
    , charsetSize : Int
    }


{-| Given a ompressed/encoded sequence frequency list, try to parse it. In general, this should only be used to parse the list at `Rumkin.Frequency.frequencyList`, but it may be used to parse custom frequency lists, if they are encoded the same. The description of the frequency list encoding follows:

The frequency thing is a bit more interesting, but still not too complex.
Each three letters are base-95 encoded number representing the chance that
this combination comes next. Subtract the value of ' ' from each of the
three, then ((((first\_value \* 95) + second\_value) \* 95) + third\_value) will
give you the odds that this pair is grouped together. The first is " "
(non-alpha chars), then " a", " b", etc. " y", " z", "a ", "aa", "ab", and
so on. If you decrypt the table successfully, you should see a really large
number for "qu".

-}
parseFrequencyList : String -> Result String FrequencyList
parseFrequencyList s =
    P.run parseFrequency s
        |> Result.mapError P.deadEndsToString


{-| Given a compressed/encoded common password list, try to parse it. In general, this should only be used to parse the list at `Rumkin.Common.commonList`, but it may be used to parse custom common password lists, if they are encoded the same. The description of the common password list encoding follows:

The compression algorithm is very basic - the first letter is upper case, and it means to copy X letters from the previous word. A = 0, B = 1, etc. So, if I had "apple apricot banana", it would compress to "AappleCricotAbanana".

-}
parseCommonList : String -> Result String CommonPasswordList
parseCommonList s =
    P.run parseCommon s
        |> Result.mapError P.deadEndsToString


{-| `getStats` with a provided frequency list and common password list. This may be used to control the point at which parsing these lists is performed or to run the algorithm with custom frequency/password lists.
-}
getCustomStats : FrequencyList -> CommonPasswordList -> String -> RumkinResult
getCustomStats wrappedFreq wrappedCommon password =
    let
        passwordLower =
            String.toLower password

        passLength =
            String.length password

        lenWarning =
            if passLength <= 4 then
                [ "Very short password!" ]

            else if passLength < 8 then
                [ "Short password!" ]

            else
                []

        commonSet =
            case wrappedCommon of
                CommonPasswordList set ->
                    set

        freqList =
            case wrappedFreq of
                FrequencyList a ->
                    a

        commonWarning =
            -- First, see if it is a common password.
            if Set.member passwordLower commonSet then
                "Common password!" :: lenWarning

            else
                lenWarning

        charsetSize =
            getCharsetSize password

        -- Calculate frequency chance
        charSet =
            logBase 2 <| toFloat charsetSize

        ( firstAidx, tail ) =
            String.uncons passwordLower
                |> Maybe.map (Tuple.mapFirst getIndex)
                |> Maybe.withDefault ( -1, "" )

        bits =
            String.foldl
                (\c ( aidx, bitAcc ) ->
                    let
                        bidx =
                            getIndex c

                        freq =
                            Array.get (aidx * 27 + bidx) freqList
                                |> Maybe.withDefault -1
                    in
                    -- Squared = assume they are good guessers
                    ( bidx, bitAcc + charSet * (1.0 - freq) ^ 2 )
                )
                ( firstAidx, 0 )
                tail
                |> Tuple.second

        ( strength, strengthComment ) =
            if bits < 28 then
                ( VeryWeak, "Try making your password longer, including CAPITALS, or adding symbols." )

            else if bits < 36 then
                ( Weak, "Usually good enough for computer login passwords and to keep out the average person." )

            else if bits < 60 then
                ( Reasonable, "This password is fairly secure cryptographically and skilled hackers may need some good computing power to crack it.  (Depends greatly on implementation!)" )

            else if bits < 128 then
                ( Strong, "This password is typically good enough to safely guard sensitive information like financial records." )

            else
                ( VeryStrong, "More often than not, this level of security is overkill." )

        entropy =
            toFloat (round (bits * 10)) / 10
    in
    -- if passLength == 0 then
    --     "Enter a password to see its strength."
    -- else
    --
    { warnings = commonWarning
    , length = passLength
    , strength = strength
    , strengthComment = strengthComment
    , entropy = entropy
    , charsetSize = charsetSize
    }


{-| The overall strength of the password, along with the string that will be found in `strengthComment`. These do **not** take into account common passwords.

  - `VeryWeak` -- "Try making your password longer, including CAPITALS, or adding symbols."
  - `Weak` -- "Usually good enough for computer login passwords and to keep out the average person."
  - `Reasonable` -- "This password is fairly secure cryptographically and skilled hackers may need some good computing power to crack it. (Depends greatly on implementation!)"
  - `Strong` -- "This password is typically good enough to safely guard sensitive information like financial records."
  - `VeryStrong` -- "More often than not, this level of security is overkill."

-}
type Strength
    = VeryWeak
    | Weak
    | Reasonable
    | Strong
    | VeryStrong



-- * Non-exposed below here


{-| The default frequency list, parsed already.
-}
defaultFrequencyList : FrequencyList
defaultFrequencyList =
    P.run parseFrequency frequencyList
        |> Result.withDefault (FrequencyList Array.empty)


{-| The default commmon password list, parsed already.
-}
defaultCommonList : CommonPasswordList
defaultCommonList =
    P.run parseCommon commonList
        |> Result.withDefault (CommonPasswordList Set.empty)


{-| Given a character, get its index for the frequency list.
-}
getIndex : Char -> Int
getIndex c =
    -- * 97 is the char code of 'a', 122 is the char code of 'z', -96 is -97 + 1 (obvious, I know).
    let
        code =
            Char.toCode << Char.toLower <| c
    in
    if code < 97 || code > 122 then
        0

    else
        code - 96


{-| Parse a compressed/encoded frequency list.
-}
parseFrequency : Parser FrequencyList
parseFrequency =
    P.loop Array.empty parseFrequencyLoopHelper
        |> P.map FrequencyList


{-| Parse a compressed/encoded common password list.
-}
parseCommon : Parser CommonPasswordList
parseCommon =
    P.loop ( "", Set.empty ) parseCommonLoopHelper
        |> P.map CommonPasswordList


{-| Parse the next frequency or succeed at the end of the string.
-}
parseFrequencyLoopHelper : Array Float -> Parser (P.Step (Array Float) (Array Float))
parseFrequencyLoopHelper acc =
    P.oneOf
        [ P.succeed (\c -> P.Loop (Array.push c acc))
            |= parseNextFrequency
        , P.succeed (P.Done acc)
            |. P.end
        ]


{-| Parse the next common password or succeed at the end of the string.
-}
parseCommonLoopHelper : ( String, Set String ) -> Parser (P.Step ( String, Set String ) (Set String))
parseCommonLoopHelper ( lastWord, acc ) =
    P.oneOf
        [ P.succeed (\word -> P.Loop ( word, Set.insert word acc ))
            |= parseCommonWord lastWord
        , P.succeed (P.Done acc)
            |. P.end
        ]


{-| Parse three characters andd convert them to a frequency.
-}
parseNextFrequency : Parser Float
parseNextFrequency =
    parse3Char
        |> P.map
            (\c123 ->
                String.foldl
                    (\c acc ->
                        -- * 32 is the char code of ' '
                        (acc - 32 + (toFloat << Char.toCode <| c)) / 95
                    )
                    0
                    c123
            )


{-| Parse exactly three characters.
-}
parse3Char : Parser String
parse3Char =
    P.getChompedString <|
        P.succeed ()
            |. P.chompIf (always True)
            |. P.chompIf (always True)
            |. P.chompIf (always True)


{-| parse the next common password and decode it, given the last common password.
-}
parseCommonWord : String -> Parser String
parseCommonWord lastWord =
    P.succeed (\first word -> ( first, word ))
        |= (P.getChompedString <| P.chompIf Char.isUpper)
        |= (P.getChompedString <| P.chompWhile (\c -> c == Char.toLower c))
        |> P.andThen
            (\( first, word ) ->
                case String.uncons first of
                    Just ( firstChar, _ ) ->
                        -- * 65 is char code of 'A'
                        P.succeed <| String.left (Char.toCode firstChar - 65) lastWord ++ word

                    Nothing ->
                        P.problem "Empty word."
            )
