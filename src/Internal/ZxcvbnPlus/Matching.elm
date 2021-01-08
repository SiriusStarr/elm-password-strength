module Internal.ZxcvbnPlus.Matching exposing
    ( SourcedAdjacencyDict
    , SourcedFrequencyDict
    , adjacencyDicts
    , dateMatch
    , dictionaryMatch
    , enumerateL33tSubs
    , frequencyDicts
    , mapIntsToDmy
    , omnimatch
    , relevantL33tSubtable
    , repeatMatch
    , sequenceMatch
    , spatialMatch
    , translate
    )

{-| Given a password string, handles finding all likely matches within it, e.g. sequences like "abcdef", common words like "password", dates, etc.
-}

import Dict exposing (Dict)
import Dict.Extra as DictX
import Internal.Zxcvbn.AdjacencyGraphs exposing (AdjacencyDict(..), dvorak, keypad, macKeypad, qwerty)
import Internal.Zxcvbn.FrequencyLists exposing (FrequencyDict, englishWikipedia, femaleNames, maleNames, passwords, surnames, usTvAndFilm)
import Internal.Zxcvbn.L33tTable exposing (SubstitutionDict, l33tTable)
import Internal.ZxcvbnPlus.MatchTypes exposing (Match, TranslationDict)
import Internal.ZxcvbnPlus.Scoring exposing (mostGuessableMatchSequence, referenceYear)
import List.Extra as ListX
import Parser as P exposing ((|.), (|=), Parser)
import Set exposing (Set)
import ZxcvbnPlus.MatchTypes exposing (DateDetails(..), Dictionary(..), DictionaryDetails(..), EnglishSource(..), KeyboardLayout(..), MatchDetails(..), NameType(..), OrdinalSequence(..))


{-| A convenience alias for a `FrequencyDict` with associated source, e.g. `CommonPassword`.
-}
type alias SourcedFrequencyDict =
    ( Dictionary, FrequencyDict )


{-| A convenience alias for an `AdjacencyDict` with associated source, e.g. `Qwerty`.
-}
type alias SourcedAdjacencyDict =
    ( KeyboardLayout, AdjacencyDict )


{-| All non-user-provided frequencey dict aggregated into a single list.
-}
frequencyDicts : List SourcedFrequencyDict
frequencyDicts =
    [ ( EnglishWord Wikipedia, englishWikipedia ), ( Name FemaleName, femaleNames ), ( Name MaleName, maleNames ), ( CommonPassword, passwords ), ( Name Surname, surnames ), ( EnglishWord TvAndFilm, usTvAndFilm ) ]


{-| All adjacency graphs aggregated into a single list.
-}
adjacencyDicts : List SourcedAdjacencyDict
adjacencyDicts =
    [ ( Qwerty, qwerty ), ( Dvorak, dvorak ), ( Keypad, keypad ), ( MacKeypad, macKeypad ) ]


{-| Makes all substitutions specified by charDict to a string.
-}
translate : String -> TranslationDict -> String
translate s charDict =
    let
        replaceChar char =
            case Dict.get char charDict of
                Nothing ->
                    char

                Just c ->
                    c
    in
    String.map replaceChar s


{-| Returns a list of matches with common passwords, English words, last names, etc. Also checks for reversed versions.
-}
dictionaryMatch : List SourcedFrequencyDict -> SubstitutionDict -> String -> List Match
dictionaryMatch freqDicts subDict password =
    let
        len =
            String.length password

        passwordLower =
            String.toLower password

        passwordReversed =
            String.reverse passwordLower

        translatedPasswords =
            relevantL33tSubtable subDict password
                |> enumerateL33tSubs
                |> List.map (\sub -> ( sub, translate passwordLower sub ))

        findMatches i j =
            let
                -- List of tuples to try to match to a dictionary, in the form (Dictionary Type, Slice of Password)
                matchers =
                    ( NormalDictionary, String.slice i j passwordLower ) :: ( ReverseDictionary, String.slice i j passwordReversed ) :: List.map (\( sub, transpw ) -> ( L33tDictionary sub, String.slice i j transpw )) translatedPasswords
            in
            List.foldl
                (\dict acc ->
                    List.filterMap (\( details, token ) -> matchDict details token password ( i, j ) dict) matchers ++ acc
                )
                []
                freqDicts
    in
    -- Generate all continuous substring indices.
    -- * There is no reason to check for length 1 matches, because the only one in the frequency lists is "i", which has a higher rank than the minimum single character guesses anyways, so this only checks length 2 and above.
    List.range 0 (len - 2)
        |> List.concatMap
            (\i ->
                List.range (i + 2) len
                    |> List.concatMap (findMatches i)
            )


{-| Makes a pruned copy of a substitution dictionary that only includes a password's possible substitutions, since there is no reason to check other subs.
-}
relevantL33tSubtable : SubstitutionDict -> String -> SubstitutionDict
relevantL33tSubtable dict password =
    let
        charSet =
            String.toList password
                |> Set.fromList

        filterFunc _ subs =
            let
                relevantSubs =
                    List.filter (\c -> Set.member c charSet) subs
            in
            if List.isEmpty relevantSubs then
                Nothing

            else
                Just relevantSubs
    in
    DictX.filterMap filterFunc dict


{-| Returns the list of all possible l33t replacement dictionaries for a given password.
-}
enumerateL33tSubs : SubstitutionDict -> List TranslationDict
enumerateL33tSubs dict =
    if Dict.isEmpty dict then
        -- Don't bother trying substitution-less l33t matching
        []

    else
        Dict.toList dict
            |> List.map
                -- Turn (char, [subs]) into [(sub1, char), (sub2, char), ..]
                (\( char, subs ) ->
                    List.map
                        (\subChar ->
                            ( subChar, char )
                        )
                        subs
                )
            |> ListX.cartesianProduct
            |> List.map Dict.fromList


{-| Check password for spatial matches, e.g. "asdfgh" for various keyboard layouts (qwerty/dvorak/keypad/mac keypad).
-}
spatialMatch : List SourcedAdjacencyDict -> String -> List Match
spatialMatch adjGraphs password =
    List.concatMap (findSpatialMatches password) adjGraphs


{-| Identifies sequences by looking for repeated differences in unicode codepoint. This allows skipping, such as 9753, and also matches some extended unicode sequences such as Greek and Cyrillic alphabets. For example, consider the input "abcdb975zy":

    Password: a   b   c   d    b     9    7    5    z   y
    Index:    0   1   2   3    4     5    6    7    8   9
    Delta:      1   1   1   -2   -41   -2   -2   69   1

    Expected result:
    [(i, j, delta), ...] = [(0, 3, 1), (5, 7, -2), (8, 9, 1)]

-}
sequenceMatch : String -> List Match
sequenceMatch password =
    let
        addSequenceMatch delta ( i, j ) matches =
            let
                absDelta =
                    abs delta
            in
            if j - i > 1 || absDelta == 1 then
                if 0 < absDelta && absDelta < maxDelta then
                    let
                        token =
                            String.slice i (j + 1) password

                        isObvious =
                            String.uncons token
                                |> Maybe.map Tuple.first
                                |> Maybe.map isObviousStart
                                |> Maybe.withDefault False

                        sequence =
                            if String.all Char.isLower token then
                                LowercaseLatin

                            else if String.all Char.isUpper token then
                                UppercaseLatin

                            else if String.all Char.isDigit token then
                                Digits

                            else
                                Unicode
                    in
                    { pattern =
                        OrdinalSequenceMatch
                            { sequence = sequence
                            , delta = delta
                            , obviousStart = isObvious
                            }
                    , i = i
                    , j = j
                    , token = token
                    }
                        :: matches

                else
                    matches

            else
                matches

        accumSequenceMatches :
            Char
            -> ( Char, List Match, { ij : ( Int, Int ), delta : Int } )
            -> ( Char, List Match, { ij : ( Int, Int ), delta : Int } )
        accumSequenceMatches currChar ( prevChar, matches, { ij, delta } ) =
            let
                ( i, j ) =
                    ij

                newDelta =
                    Char.toCode currChar - Char.toCode prevChar
            in
            if newDelta == delta then
                -- Continue current match
                ( currChar
                , matches
                , { ij = ( i, j + 1 ), delta = newDelta }
                )

            else
                let
                    newMatches =
                        addSequenceMatch delta ( i, j ) matches
                in
                ( currChar
                , newMatches
                , { ij = ( j, j + 1 ), delta = newDelta }
                )
    in
    case String.uncons password of
        Just ( firstChar, _ ) ->
            let
                ( _, matches, lastMatch ) =
                    String.foldl
                        accumSequenceMatches
                        ( firstChar, [], { ij = ( -1, -1 ), delta = maxDelta } )
                        password

                ( lastI, lastJ ) =
                    lastMatch.ij
            in
            addSequenceMatch lastMatch.delta ( lastI, lastJ ) matches

        Nothing ->
            []


{-| Find repeats in a password, e.g. "aaa", "abcabcabc", etc. Note that this requires user dictionary patterns for recursively matching and scoring the base repeat token.
-}
repeatMatch : FrequencyDict -> String -> List Match
repeatMatch userPatterns password =
    let
        drops =
            -- Last drop leaves a tail of 2, since you can't have a 1 character repeat
            List.range 0 <| String.length password - 2

        getRepeatsAt i (( matches, checkFrom ) as acc) =
            if i >= checkFrom then
                String.dropLeft i password
                    |> appendRepeat userPatterns i checkFrom matches

            else
                acc
    in
    List.foldl getRepeatsAt ( [], 0 ) drops
        |> Tuple.first


{-| Find all dates in password. A "date" is recognized as: any 3-tuple that starts or ends with a 2- or 4-digit year, with 2 or 0 separator chars (1.1.91 or 1191), maybe zero-padded (01-01-91 vs 1-1-91), a month between 1 and 12, a day between 1 and 31.

    A date match may also simply be a recent year, e.g. "1990".

-}
dateMatch : String -> List Match
dateMatch password =
    let
        drops =
            -- Last drop leaves a tail of 4, since you can't have a <4 character date
            List.range 0 <| String.length password - 4

        getDatesAt i ( matches, lastEnd ) =
            let
                tail =
                    String.dropLeft i password

                ( matchesWithRecentYear, lastEndWithRecentYear ) =
                    case P.run parseRecentYear tail of
                        Ok year ->
                            -- Can update offset if there was a recent year match, since the only way it could cause a conflict is with a 4-digit date match, and a recent year match should always be preferred to that.
                            let
                                j =
                                    i + 4
                            in
                            ( { pattern =
                                    DateMatch
                                        { year = year
                                        , dateDetails = RecentYear
                                        }
                              , i = i
                              , j = j - 1
                              , token = String.slice i j password
                              }
                                :: matches
                            , max j lastEnd
                            )

                        Err _ ->
                            ( matches, lastEnd )
            in
            case P.run parseDate tail of
                Ok ( sep, { year, month, day }, offset ) ->
                    let
                        j =
                            i + offset
                    in
                    if j > lastEndWithRecentYear then
                        -- Only append if not a submatch
                        ( { pattern =
                                DateMatch
                                    { year = year
                                    , dateDetails =
                                        FullDate
                                            { separator = sep
                                            , month = month
                                            , day = day
                                            }
                                    }
                          , i = i
                          , j = j - 1
                          , token = String.slice i j password
                          }
                            :: matchesWithRecentYear
                        , j
                        )

                    else
                        ( matchesWithRecentYear, lastEndWithRecentYear )

                Err _ ->
                    ( matchesWithRecentYear, lastEnd )
    in
    List.foldl getDatesAt ( [], 0 ) drops
        |> Tuple.first


{-| Given three integers, discard if:

  - Middle int is over 31 (for all dmy formats, years are never allowed in the middle)
  - Middle int is zero
  - Any int is over the max allowable year
  - Any int is over two digits but under the min allowable year
  - 2 ints are over 31, the max allowable day
  - 2 ints are zero
  - All ints are over 12, the max allowable month

Otherwise, return a record for the date.

-}
mapIntsToDmy : Int -> Int -> Int -> Maybe Date
mapIntsToDmy i1 i2 i3 =
    let
        year1 =
            asFourDigitYear i1

        year3 =
            asFourDigitYear i3

        -- Year can't be in middle
        possibleDates =
            [ Date year1 i2 i3, Date year1 i3 i2, Date year3 i1 i2, Date year3 i2 i1 ]
    in
    -- Remove invalid years then keep best match
    List.filter (not << isInvalidDate) possibleDates
        |> ListX.foldl1 keepMostRecentYear


{-| Performs all of the various matching techniques.
-}
omnimatch : List String -> String -> List Match
omnimatch userPatterns password =
    let
        userDict =
            List.indexedMap (\i s -> ( String.toLower s, i + 1 )) userPatterns
                |> Dict.fromList
    in
    omnimatchHelper userDict password



-- * Non-exposed below here


{-| Internal omnimatch that uses user patterns in dict already. Used for repeat matching.
-}
omnimatchHelper : FrequencyDict -> String -> List Match
omnimatchHelper userDict password =
    let
        dicts =
            if Dict.isEmpty userDict then
                frequencyDicts

            else
                ( UserPattern, userDict ) :: frequencyDicts

        matchers =
            [ dictionaryMatch dicts l33tTable
            , spatialMatch adjacencyDicts
            , repeatMatch userDict
            , sequenceMatch
            , dateMatch
            ]
    in
    List.concatMap ((|>) password) matchers


{-| Returns a `Maybe Match` for a slice of a password and a dictionary. Used to `filterMap` every `(i, j)` for every frequency dictionary.
-}
matchDict : DictionaryDetails -> String -> String -> ( Int, Int ) -> SourcedFrequencyDict -> Maybe Match
matchDict dictionaryDetails substring password ( i, j ) ( dictSource, dict ) =
    let
        makeMatch r =
            case dictionaryDetails of
                NormalDictionary ->
                    Just
                        { pattern =
                            DictionaryMatch
                                { dictionaryDetails = NormalDictionary
                                , matchedWord = substring
                                , rank = r
                                , dictionary = dictSource
                                }
                        , i = i
                        , j = j - 1 -- Elm string slicing works differently, so j's are off by one.
                        , token = String.slice i j password
                        }

                ReverseDictionary ->
                    let
                        len =
                            String.length password
                    in
                    Just
                        { pattern =
                            DictionaryMatch
                                { dictionaryDetails = ReverseDictionary
                                , matchedWord = substring
                                , rank = r
                                , dictionary = dictSource
                                }
                        , i = len - j
                        , j = len - 1 - i
                        , token = String.slice (len - j) (len - i) password
                        }

                L33tDictionary sub ->
                    let
                        token =
                            String.slice i j password
                    in
                    if String.toLower token == substring then
                        -- Only return the matches that contain an actual substitution
                        Nothing

                    else
                        let
                            usedSubs =
                                Dict.filter (\k _ -> String.any ((==) k) token) sub
                        in
                        Just
                            { pattern =
                                DictionaryMatch
                                    { dictionaryDetails = L33tDictionary usedSubs
                                    , matchedWord = substring
                                    , rank = r
                                    , dictionary = dictSource
                                    }
                            , i = i
                            , j = j - 1 -- Elm string slicing works differently, so j's are off by one.
                            , token = token
                            }
    in
    Maybe.andThen makeMatch <| Dict.get substring dict


{-| Given a last character, the next character, and an adjacency dict, determine which direction the sequence moved in (or if it broke) and whether or not the new character is shifted.
-}
getDirectionAndShift : Char -> Char -> AdjacencyDict -> ( Maybe Int, Bool )
getDirectionAndShift prevChar currChar graph =
    case graph of
        Unshiftable g ->
            let
                adjacents =
                    Maybe.withDefault [] <| Dict.get prevChar g
            in
            ( ListX.elemIndex (Just currChar) adjacents, False )

        Shiftable g ->
            let
                ( adjacents, shiftedAdjacents ) =
                    Maybe.withDefault ( [], [] ) <| Dict.get prevChar g

                unshiftedIndex =
                    ListX.elemIndex (Just currChar) adjacents
            in
            case unshiftedIndex of
                Nothing ->
                    case ListX.elemIndex (Just currChar) shiftedAdjacents of
                        Nothing ->
                            ( Nothing, False )

                        Just i ->
                            ( Just i, True )

                Just i ->
                    ( Just i, False )


{-| Given a password and an adjacency graph, finds all spatial sequences for it, e.g. "asdfgh".
-}
findSpatialMatches : String -> ( KeyboardLayout, AdjacencyDict ) -> List Match
findSpatialMatches password ( layout, graph ) =
    let
        makeSpatialMatch turns shiftedKeys ( i, j ) =
            { pattern =
                KeyAdjacencyMatch
                    { layout = layout
                    , turns = turns
                    , shiftedKeys = shiftedKeys
                    }
            , i = i
            , j = j
            , token = String.slice i (j + 1) password
            }

        accumSpatialMatches :
            Char
            -> ( Char, List Match, { ij : ( Int, Int ), dir : Int, shifts : Int, turns : Int } )
            -> ( Char, List Match, { ij : ( Int, Int ), dir : Int, shifts : Int, turns : Int } )
        accumSpatialMatches currChar ( prevChar, matches, { ij, dir, shifts, turns } ) =
            let
                ( i, j ) =
                    ij
            in
            case getDirectionAndShift prevChar currChar graph of
                ( Nothing, _ ) ->
                    let
                        newMatches =
                            if j - i > 1 then
                                makeSpatialMatch turns shifts ( i, j ) :: matches

                            else
                                -- Don't consider length 1 or 2 chains.
                                matches

                        firstShifted =
                            case graph of
                                Unshiftable _ ->
                                    0

                                Shiftable _ ->
                                    if Set.member currChar shiftedChars then
                                        1

                                    else
                                        0
                    in
                    ( currChar
                    , newMatches
                    , { ij = ( j + 1, j + 1 ), dir = -1, shifts = firstShifted, turns = 0 }
                    )

                ( Just newDir, isShifted ) ->
                    let
                        newTurns =
                            if newDir == dir then
                                turns

                            else
                                turns + 1

                        newShifts =
                            if isShifted then
                                shifts + 1

                            else
                                shifts
                    in
                    ( currChar
                    , matches
                    , { ij = ( i, j + 1 ), dir = newDir, shifts = newShifts, turns = newTurns }
                    )

        ( _, foundMatches, lastMatch ) =
            String.foldl
                accumSpatialMatches
                ( ' '
                , []
                , { ij = ( -1, -1 ), dir = -1, shifts = 0, turns = 0 }
                )
                password

        ( lastI, lastJ ) =
            lastMatch.ij
    in
    if lastJ - lastI > 1 then
        -- Append last match
        makeSpatialMatch lastMatch.turns lastMatch.shifts lastMatch.ij :: foundMatches

    else
        -- Don't consider length 1 or 2 chains.
        foundMatches


{-| The greatest skip between codepoints that will be identified as a sequence.
-}
maxDelta : Int
maxDelta =
    5


{-| Map a year and two ints to a Date, if possible.
-}
mapIntsToMd : Int -> ( Int, Int ) -> Maybe Date
mapIntsToMd year ( i1, i2 ) =
    let
        d1 =
            Date year i1 i2
    in
    if isInvalidDate d1 then
        let
            d2 =
                Date year i2 i1
        in
        if isInvalidDate d2 then
            Nothing

        else
            Just d2

    else
        Just d1


{-| Enumerates all possible ways a date (without separators) can be divided into years, months, and days, keyed by length of the date match.
-}
possibleDateSplits : Dict Int (List ( Int, Int ))
possibleDateSplits =
    Dict.fromList
        [ ( 4
          , -- For length-4 strings, eg 1191 or 9111, two ways to split:
            [ -- 1 1 91 (2nd split starts at index 1, 3rd at index 2)
              ( 1, 2 )
            , -- 91 1 1
              ( 2, 3 )
            ]
          )
        , ( 5
          , [ -- 1 11 91
              ( 1, 3 )
            , -- 11 1 91
              ( 2, 3 )
            , -- 91 11 1
              ( 2, 4 )
            ]
          )
        , ( 6
          , [ -- 1 1 1991
              ( 1, 2 )
            , -- 11 11 91
              ( 2, 4 )
            , -- 1991 1 1
              ( 4, 5 )
            ]
          )
        , ( 7
          , [ -- 1 11 1991
              ( 1, 3 )
            , -- 11 1 1991
              ( 2, 3 )
            , -- 1991 1 11
              ( 4, 5 )
            , -- 1991 11 1
              ( 4, 6 )
            ]
          )
        , ( 8
          , [ -- 11 11 1991
              ( 2, 4 )
            , -- 1991 11 11
              ( 4, 6 )
            ]
          )
        ]


{-| Date record passed around during date matching.
-}
type alias Date =
    { year : Int
    , month : Int
    , day : Int
    }


{-| Given a new date, compare to the previous best and keep the one closer to Scoring.referenceYear.
-}
keepMostRecentYear : Date -> Date -> Date
keepMostRecentYear currDate bestDate =
    let
        metric m =
            abs <| m.year - referenceYear
    in
    if metric currDate < metric bestDate then
        currDate

    else
        bestDate


{-| Define the maximum year recognized for date matches.
-}
dateMaxYear : Int
dateMaxYear =
    2050


{-| Define the minimum year recognized for date matches.
-}
dateMinYear : Int
dateMinYear =
    1000


{-| Convert a two-digit number to a four-digit year.
-}
asFourDigitYear : Int -> Int
asFourDigitYear y =
    if y > 99 then
        y

    else if y > 50 then
        -- 87 -> 1987
        y + 1900

    else
        -- 15 -> 2015
        y + 2000


{-| Given a date, return true if it is invalid, (e.g. February 30th).
-}
isInvalidDate : Date -> Bool
isInvalidDate d =
    let
        yearBounds =
            dateMinYear > d.year || d.year > dateMaxYear

        monthBounds =
            d.month < 1 || d.month > 12

        dayNegative =
            d.day < 1

        dayMaxBound =
            -- Handle February month with leap year
            if d.month == 2 then
                if isLeapYear d.year then
                    d.day > 29

                else
                    d.day > 28

            else if d.month == 4 || d.month == 6 || d.month == 9 || d.month == 11 then
                -- Months of Apr, Jun, Sept, and Nov 30 days
                d.day > 30

            else
                d.day > 31
    in
    List.any identity [ yearBounds, monthBounds, dayNegative, dayMaxBound ]


{-| Returns true if given year is a leap year.
-}
isLeapYear : Int -> Bool
isLeapYear y =
    -- Multiple of 4 and not multiple of 100 or is multiple of 400
    (remainderBy 4 y == 0 && remainderBy 100 y /= 0) || remainderBy 400 y == 0


{-| Try to parse the beginning of a string as a date, returning the separator (if there is one), the date, and the length of the parse match.
-}
parseDate : Parser ( Maybe Char, Date, Int )
parseDate =
    P.oneOf
        [ P.backtrackable
            (P.succeed (\( s, d ) j -> ( s, d, j ))
                |= parseDateWithSep
                -- get j
                |= P.getOffset
            )
        , P.succeed (\d j -> ( Nothing, d, j ))
            |= parseDateWithoutSep
            -- get j
            |= P.getOffset
        ]


{-| A parser for dates with separators, e.g. "11/11/1999" rather than "11111999".
-}
parseDateWithSep : Parser ( Maybe Char, Date )
parseDateWithSep =
    P.succeed (\in1 s1 in2 s2 -> ( ( s1, s2 ), ( in1, in2 ) ))
        |= parse1Or2Or4Digits
        |= parseDateSeparator
        |= parse1Or2Digits
        |= parseDateSeparator
        |> P.andThen
            (\( seps, ( ( l1, i1 ), i2 ) ) ->
                case l1 of
                    4 ->
                        -- Began with 4 digits, first must be a year
                        P.succeed (\i3 -> makeSepDate seps ( i2, i3 ) i1)
                            |= parse1Or2Digits

                    2 ->
                        -- Began with 2 digits, try to parse up to 4 in last slot
                        P.succeed
                            (\( l3, i3 ) ->
                                case l3 of
                                    4 ->
                                        -- Ended with 4 digits, last must be year
                                        makeSepDate seps ( i1, i2 ) i3

                                    2 ->
                                        -- Pick best year, since both first and last are 2 digits
                                        makeUncertainSepDate seps ( i1, i2, i3 )

                                    1 ->
                                        -- Ended with 1 digit, first must be year
                                        makeSepDate seps ( i2, i3 ) (asFourDigitYear i1)

                                    _ ->
                                        Nothing
                            )
                            |= parse1Or2Or4Digits

                    1 ->
                        -- Began with 1 digit, last must be year.
                        P.succeed (\i3 -> makeSepDate seps ( i1, i2 ) (asFourDigitYear i3))
                            |= parse2Or4Digits

                    _ ->
                        P.problem "Invalid date."
            )
        |> P.andThen
            (\maybe ->
                case maybe of
                    Just r ->
                        P.succeed r

                    Nothing ->
                        P.problem "Invalid date."
            )


{-| Try to turn a tuple of separators and three integers into the "best" possible separated date or return `Nothing` if it's not possible.
-}
makeUncertainSepDate : ( Char, Char ) -> ( Int, Int, Int ) -> Maybe ( Maybe Char, Date )
makeUncertainSepDate ( s1, s2 ) ( i1, i2, i3 ) =
    if s1 == s2 then
        Maybe.map (\d -> ( Just s1, d )) <| mapIntsToDmy i1 i2 i3

    else
        Nothing


{-| Try to turn a tuple of separators and three integers, one of which is known to be the year, into the "best" possible separated date or return `Nothing` if it's not possible.
-}
makeSepDate : ( Char, Char ) -> ( Int, Int ) -> Int -> Maybe ( Maybe Char, Date )
makeSepDate ( s1, s2 ) ( i1, i2 ) y =
    if s1 == s2 then
        Maybe.map (Tuple.pair <| Just s1) <| mapIntsToMd y ( i1, i2 )

    else
        Nothing


{-| Parse a single character as a valid date separator, i.e. '/', '-', '.', '', '\_', ' ', tab, or newline.
-}
parseDateSeparator : Parser Char
parseDateSeparator =
    P.oneOf
        [ P.succeed '/'
            |. P.symbol "/"
        , P.succeed '-'
            |. P.symbol "-"
        , P.succeed '.'
            |. P.symbol "."
        , P.succeed '\\'
            |. P.symbol "\\"
        , P.succeed '_'
            |. P.symbol "_"
        , P.succeed ' '
            |. P.symbol " "
        , P.succeed '\t'
            |. P.symbol "\t"
        , P.succeed '\n'
            |. P.symbol "\n"
        ]


{-| A parser for dates without separators, e.g. "11111999" rather than "11/11/1999".
-}
parseDateWithoutSep : Parser Date
parseDateWithoutSep =
    P.loop ( 0, [] ) parse4To8Helper
        |> P.andThen parseAsBestDate


{-| Looped parser for chomping 4-8 digits.
-}
parse4To8Helper : ( Int, List Int ) -> Parser (P.Step ( Int, List Int ) ( Int, List Int ))
parse4To8Helper ( numParsed, acc ) =
    if numParsed >= 8 then
        P.succeed (P.Done ( 8, acc ))

    else
        P.oneOf
            [ P.succeed (\i -> P.Loop ( numParsed + 1, i :: acc ))
                |= parseSingleDigit
            , if numParsed >= 4 then
                P.succeed (P.Done ( numParsed, acc ))

              else
                P.problem "Too few digits."
            ]


{-| Given a list of integers (and its length), attempt to parse it as the "best" possible date or fail.
-}
parseAsBestDate : ( Int, List Int ) -> Parser Date
parseAsBestDate ( numDigits, l ) =
    let
        possibleSplits =
            -- Lookup valid splits for a date of this length
            Dict.get numDigits possibleDateSplits
                |> Maybe.withDefault []
    in
    List.filterMap (splitListToDate l) possibleSplits
        -- At this point: different possible dmy mappings for the same i,j substring.  Match the candidate date that likely takes the fewest guesses: a year closest to Scoring.referenceYear.  I.e., considering "111504", prefer 11-15-04 to 1-1-1504 (interpreting "04" as 2004).
        |> ListX.foldl1 keepMostRecentYear
        |> Maybe.map P.succeed
        |> Maybe.withDefault (P.problem "No valid dates.")


{-| Given a matched list of integers and a way to split it (of the type stored in `possibleDateSplits`), return a date if possible.
-}
splitListToDate : List Int -> ( Int, Int ) -> Maybe Date
splitListToDate l splits =
    let
        ( i1, i2, i3 ) =
            splitListToInts splits l
    in
    mapIntsToDmy i1 i2 i3


{-| Given a tuple of split points and a (reversed) list of digits, split it into three ints.
-}
splitListToInts : ( Int, Int ) -> List Int -> ( Int, Int, Int )
splitListToInts ( s1, s2 ) =
    let
        splitListToIntsHelper i digit ( int1, int2, int3 ) =
            if i < s1 then
                ( int1 + digit * 10 ^ i, int2, int3 )

            else if i < s2 then
                ( int1, int2 + digit * 10 ^ (i - s1), int3 )

            else
                ( int1, int2, int3 + digit * 10 ^ (i - s2) )
    in
    ListX.indexedFoldl splitListToIntsHelper ( 0, 0, 0 )


{-| Parse exactly 2 digits as an integer or fail.
-}
parse2Digits : Parser Int
parse2Digits =
    P.succeed (\t o -> 10 * t + o)
        |= parseSingleDigit
        |= parseSingleDigit


{-| Attempt to parse the first 4 characters of a string as a recent year or fail.
-}
parseRecentYear : Parser Int
parseRecentYear =
    parse4Digits
        |> P.andThen checkRecentYear


{-| Check if a (4-digit) integer is a recent year.
-}
checkRecentYear : Int -> Parser Int
checkRecentYear y =
    -- ! This needs updating in 2022
    if 1900 <= y && y <= 2021 then
        P.succeed y

    else
        P.problem "Not a recent year."


{-| Attempt to parse 1 or 2 digits.
-}
parse1Or2Digits : Parser Int
parse1Or2Digits =
    parseSingleDigit
        |> P.andThen
            (\i ->
                P.oneOf
                    [ P.succeed (\o -> 10 * i + o)
                        |= parseSingleDigit
                    , P.succeed i
                    ]
            )


{-| Attempt to parse 1 or 2 or 4 digits and return a tuple of the number of digits parsed and the actual result.
-}
parse1Or2Or4Digits : Parser ( Int, Int )
parse1Or2Or4Digits =
    parseSingleDigit
        |> P.andThen
            (\i ->
                P.oneOf
                    [ P.succeed (\o -> 10 * i + o)
                        |= parseSingleDigit
                        -- At least 2 digits
                        |> P.andThen
                            (\tens ->
                                P.oneOf
                                    [ -- 4 Digits
                                      P.backtrackable
                                        (P.succeed (\newTens -> ( 4, 100 * tens + newTens ))
                                            |= parse2Digits
                                        )
                                    , P.succeed ( 2, tens ) -- Only 2 digits
                                    ]
                            )
                    , P.succeed ( 1, i ) -- Only 1 digit
                    ]
            )


{-| Attempt to parse 2 or 4 digits (but not three).
-}
parse2Or4Digits : Parser Int
parse2Or4Digits =
    parse2Digits
        |> P.andThen
            (\i ->
                P.oneOf
                    [ P.succeed (\o -> 100 * i + o)
                        |= parse2Digits
                    , P.succeed i
                    ]
            )


{-| Parse exactly 4 digits.
-}
parse4Digits : Parser Int
parse4Digits =
    P.succeed (\thou hun ten one -> thou * 1000 + hun * 100 + ten * 10 + one)
        |= parseSingleDigit
        |= parseSingleDigit
        |= parseSingleDigit
        |= parseSingleDigit


{-| Parse the next character, if it is a digit.
-}
parseSingleDigit : Parser Int
parseSingleDigit =
    P.chompIf Char.isDigit
        |> P.getChompedString
        |> P.andThen
            (\s ->
                case String.toInt s of
                    Just i ->
                        P.succeed i

                    Nothing ->
                        P.problem "Not a digit."
            )


{-| True if the character is an obvious place to start a sequence, false otherwise.
-}
isObviousStart : Char -> Bool
isObviousStart c =
    Set.member c obviousStartSet


{-| Set of characters constituing obvious starts for a sequence.
-}
obviousStartSet : Set Char
obviousStartSet =
    Set.fromList [ 'a', 'A', 'z', 'Z', '0', '1', '9' ]


{-| Repeat type used in repeat matching.
-}
type alias Repeat =
    -- ((Base Repeat, Left To Consume, Times Repeated (starts at zero)), Total Length)
    ( ( List Char, List Char, Int ), Int )


{-| Given a string, find the longest repeating sequence it begins with (if any).
-}
getLongestRepeat : String -> Maybe ( String, Int )
getLongestRepeat token =
    let
        ( _, repeats, _ ) =
            String.foldl (updateRepeatState (String.length token // 2)) ( [], ( [], ( ( [], [], 0 ), 0 ) ), 0 ) token
    in
    repeats
        |> Tuple.second
        |> (\( ( baseToken, _, repeatNum ), _ ) ->
                if repeatNum > 0 then
                    Just ( String.fromList baseToken, repeatNum + 1 )

                else
                    Nothing
           )


{-| Helper function for getLongestRepeat.
-}
updateRepeatState : Int -> Char -> ( List Char, ( List Repeat, Repeat ), Int ) -> ( List Char, ( List Repeat, Repeat ), Int )
updateRepeatState halfwayMark c ( baseSequence, ( openRepeats, bestRepeat ), i ) =
    if List.isEmpty baseSequence then
        -- Base case, begin the sequence with the single character as the best.
        ( [ c ], ( [], ( ( [ c ], [ c ], 0 ), 1 ) ), 1 )

    else
        let
            updatedBest =
                updateRepeat c bestRepeat

            (( newOpenRepeats, newBest ) as newState) =
                List.foldl (\r acc -> keepBestRepeats (updateRepeat c r) acc) ( [], updatedBest ) openRepeats
        in
        if i > halfwayMark then
            -- No reason to keep baseSequence or keep updating i once past halfway
            ( baseSequence, newState, i )

        else
            -- Extend base sequence, update running repeats, and push the new possible repeat
            let
                newBaseSequence =
                    baseSequence ++ [ c ]
            in
            ( newBaseSequence, ( ( ( newBaseSequence, newBaseSequence, 0 ), List.length newBaseSequence ) :: newOpenRepeats, newBest ), i + 1 )


{-| Discard clearly inferior repeat sequences; helper function for updateRepeatState.
-}
keepBestRepeats : Repeat -> ( List Repeat, Repeat ) -> ( List Repeat, Repeat )
keepBestRepeats (( ( _, newToConsume, newRepeats ), newLength ) as new) ( acc, ( ( _, oldToConsume, oldRepeats ), oldLength ) as old ) =
    let
        oldBetter =
            if List.isEmpty newToConsume then
                -- Discard finished repeats if we've found better ones
                ( acc, old )

            else
                -- Always keep open repeats
                ( new :: acc, old )

        newBetter =
            if List.isEmpty oldToConsume then
                -- Discard finished repeats if we've found better ones
                ( acc, new )

            else
                -- Always keep open repeats
                ( old :: acc, new )
    in
    -- First only consider new if it actually is repeated, then go by length first (more better), then by number of repeats (more better), then by whether it's still open (open better)
    if newRepeats <= 0 then
        oldBetter

    else if newLength < oldLength then
        oldBetter

    else if newLength > oldLength then
        newBetter

    else if oldRepeats > newRepeats then
        oldBetter

    else if oldRepeats < newRepeats then
        newBetter

    else if List.isEmpty oldToConsume then
        newBetter

    else
        oldBetter


{-| Given the next character, updates a repeat and returns its length.
-}
updateRepeat : Char -> Repeat -> Repeat
updateRepeat c (( ( baseRepeat, toConsume, numRepeats ), length ) as repAndLength) =
    case toConsume of
        x :: xs ->
            if c /= x then
                -- Repeat has broken
                ( ( baseRepeat, [], numRepeats ), length )

            else if List.isEmpty xs then
                -- Fully consumed a repeat, so increment and start over
                ( ( baseRepeat, baseRepeat, numRepeats + 1 ), length + List.length baseRepeat )

            else
                -- Continue the current repeat
                ( ( baseRepeat, xs, numRepeats ), length )

        [] ->
            -- Repeat has already broken
            repAndLength


{-| Check the substring for the longest repeat from the start and append it if it exists. Update new check-from value as well.
-}
appendRepeat : FrequencyDict -> Int -> Int -> List Match -> String -> ( List Match, Int )
appendRepeat userPatterns offset oldCheckFrom matches substr =
    case getLongestRepeat substr of
        Just ( baseToken, repeats ) ->
            let
                baseLen =
                    String.length baseToken

                len =
                    baseLen * repeats

                ( i, j ) =
                    ( offset, offset + len - 1 )

                -- Recursively match and score the base string
                baseAnalysis =
                    mostGuessableMatchSequence False baseToken <| omnimatchHelper userPatterns baseToken

                newCheckFrom =
                    offset + len - baseLen + 1
            in
            ( { pattern =
                    RepeatMatch
                        { baseToken = baseToken
                        , repeatCount = repeats
                        , baseGuesses = baseAnalysis.guesses
                        , baseMatchSequence = baseAnalysis.sequence
                        }
              , i = i
              , j = j
              , token = String.left len substr
              }
                :: matches
            , newCheckFrom
            )

        Nothing ->
            ( matches, oldCheckFrom )


{-| Used for determining whether a character is "shifted" or not for spatial matching.
-}
shiftedChars : Set Char
shiftedChars =
    Set.fromList [ '[', '~', '!', '@', '#', '$', '%', '^', '&', '*', '(', ')', '_', '+', 'Q', 'W', 'E', 'R', 'T', 'Y', 'U', 'I', 'O', 'P', '{', '}', '|', 'A', 'S', 'D', 'F', 'G', 'H', 'J', 'K', 'L', ':', '"', 'Z', 'X', 'C', 'V', 'B', 'N', 'M', '<', '>', '?', ']' ]
