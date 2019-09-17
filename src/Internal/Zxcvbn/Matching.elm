module Internal.Zxcvbn.Matching exposing
    ( NamedAdjacencyDict
    , NamedFrequencyDict
    , adjacencyDicts
    , dateMatch
    , dictionaryMatch
    , enumerateL33tSubs
    , frequencyDicts
    , l33tMatch
    , mapIntsToDmy
    , matchAsNoSeparatorDate
    , matchAsSeparatorDate
    , omnimatch
    , regexMatch
    , relevantL33tSubtable
    , repeatMatch
    , reverseDictionaryMatch
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
import Internal.Zxcvbn.MatchTypes exposing (Match, TranslationDict, sortMatches)
import Internal.Zxcvbn.Scoring exposing (mostGuessableMatchSequence, referenceYear)
import List.Extra as ListX
import Regex exposing (Regex)
import Set exposing (Set)
import Zxcvbn.MatchTypes exposing (DictionaryDetails(..), MatchDetails(..))


{-| A convenience alias for a `FrequencyDict` with associated name, e.g. "englishWikipedia".
-}
type alias NamedFrequencyDict =
    ( String, FrequencyDict )


{-| A convenience alias for an `AdjacencyDict` with associated name, e.g. "qwerty".
-}
type alias NamedAdjacencyDict =
    ( String, AdjacencyDict )


{-| All non-user-provided frequencey dict aggregated into a single list.
-}
frequencyDicts : List NamedFrequencyDict
frequencyDicts =
    [ ( "englishWikipedia", englishWikipedia ), ( "femaleNames", femaleNames ), ( "maleNames", maleNames ), ( "passwords", passwords ), ( "surnames", surnames ), ( "usTvAndFilm", usTvAndFilm ) ]


{-| All adjacency graphs aggregated into a single list.
-}
adjacencyDicts : List NamedAdjacencyDict
adjacencyDicts =
    [ ( "qwerty", qwerty ), ( "dvorak", dvorak ), ( "keypad", keypad ), ( "macKeypad", macKeypad ) ]


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


{-| Returns a list of matches with common passwords, English words, last names, etc.
-}
dictionaryMatch : List NamedFrequencyDict -> String -> List Match
dictionaryMatch freqDicts password =
    let
        len =
            String.length password

        passwordLower =
            String.toLower password

        continuousSubsequenceIndices =
            -- Generate all continuous substring indices.
            List.concatMap
                (\i ->
                    List.map (Tuple.pair i) <| List.range (i + 1) len
                )
            <|
                List.range 0 (len - 1)

        findMatches ( i, j ) =
            let
                substring =
                    String.slice i j passwordLower
            in
            List.filterMap (matchDict substring password ( i, j )) freqDicts
    in
    sortMatches << List.concatMap findMatches <| continuousSubsequenceIndices


{-| Returns a list of matches for a password after being reversed with common passwords, english, last names, etc.
-}
reverseDictionaryMatch : List NamedFrequencyDict -> String -> List Match
reverseDictionaryMatch freqDicts password =
    let
        len =
            String.length password

        changeToReverseMatch m =
            case m.pattern of
                DictionaryMatch dictDetails ->
                    { m
                        | token = String.reverse m.token

                        -- Map coordinates back to original string
                        , i = len - 1 - m.j
                        , j = len - 1 - m.i
                        , pattern =
                            -- Change type to reverse match
                            DictionaryMatch { dictDetails | dictionaryDetails = ReverseDictionary }
                    }

                _ ->
                    m
    in
    String.reverse password
        |> dictionaryMatch freqDicts
        |> List.map changeToReverseMatch
        |> sortMatches


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


{-| Returns a list of matches for a password after undergoing common l33t subsitutions with common passwords, english, last names, etc.
-}
l33tMatch : List NamedFrequencyDict -> SubstitutionDict -> String -> List Match
l33tMatch freqDicts subDict password =
    let
        possibleSubs =
            enumerateL33tSubs <| relevantL33tSubtable subDict password

        dictMatchWithSub sub =
            let
                filterPossibleL33tMatches m =
                    case m.pattern of
                        DictionaryMatch ({ matchedWord } as details) ->
                            let
                                token =
                                    String.slice m.i (m.j + 1) password
                            in
                            if String.toLower token == matchedWord then
                                -- Only return the matches that contain an actual substitution
                                Nothing

                            else if m.j - m.i <= 0 then
                                -- Filter single-character l33t matches to reduce noise.  Otherwise '1' matches 'i', '4' matches 'a', both very common English words with low dictionary rank.
                                Nothing

                            else
                                let
                                    usedSubs =
                                        Dict.filter (\k _ -> String.any ((==) k) token) sub

                                    l33tDetails =
                                        L33tDictionary
                                            { sub = usedSubs
                                            , subDisplay = displaySub usedSubs
                                            }
                                in
                                Just
                                    { m
                                        | pattern =
                                            DictionaryMatch
                                                { details
                                                    | dictionaryDetails =
                                                        l33tDetails
                                                }
                                        , token = token
                                    }

                        _ ->
                            Nothing
            in
            translate password sub
                |> dictionaryMatch freqDicts
                |> List.filterMap filterPossibleL33tMatches
    in
    sortMatches << List.concatMap dictMatchWithSub <| possibleSubs


{-| Check password for spatial matches, e.g. "asdfgh" for various keyboard layouts (qwerty/dvorak/keypad/mac keypad).
-}
spatialMatch : List NamedAdjacencyDict -> String -> List Match
spatialMatch adjGraphs password =
    sortMatches << List.concatMap (findSpatialMatches password) <| adjGraphs


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

                        ( sequenceName, sequenceSpace ) =
                            if String.all Char.isLower token then
                                ( "lower", 26 )

                            else if String.all Char.isUpper token then
                                ( "upper", 26 )

                            else if String.all Char.isDigit token then
                                ( "digits", 10 )

                            else
                                ( "unicode", 26 )
                    in
                    { pattern =
                        SequenceMatch
                            { sequenceName = sequenceName
                            , sequenceSpace = sequenceSpace
                            , ascending = delta > 0
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
                |> sortMatches

        Nothing ->
            []


{-| Find repeats in a password, e.g. "aaa", "abcabcabc", etc. Note that this requires user dictionary patterns for recursively matching and scoring the base repeat token.
-}
repeatMatch : List String -> String -> List Match
repeatMatch userPatterns password =
    let
        tailIndices =
            List.range 1 <| String.length password - 1

        matchOnTails i ( lastMatchOffset, matches ) =
            case lastMatchOffset of
                Nothing ->
                    -- Once a greedy match fails, finished
                    ( Nothing, matches )

                Just offset ->
                    if i > offset then
                        String.dropLeft offset password
                            |> addRepeatMatch userPatterns offset matches

                    else
                        ( lastMatchOffset, matches )
    in
    List.foldl matchOnTails ( Just 0, [] ) tailIndices
        |> Tuple.second
        |> sortMatches


{-| Find matches based on regexes. Per reference implementation, this only finds recent years (1900--2019).
-}
regexMatch : String -> List Match
regexMatch password =
    let
        generateMatch m =
            { pattern =
                RegexMatch
                    { regexName = "recentYear"
                    , regexMatch = m
                    }
            , i = m.index
            , j = m.index + String.length m.match - 1
            , token = m.match
            }
    in
    Regex.find recentYearRegex password
        |> List.map generateMatch
        |> sortMatches


{-| Find all dates in password. A "date" is recognized as: any 3-tuple that starts or ends with a 2- or 4-digit year, with 2 or 0 separator chars (1.1.91 or 1191), maybe zero-padded (01-01-91 vs 1-1-91), a month between 1 and 12, a day between 1 and 31.

    Note: this isn't true date parsing in that "feb 31st" is allowed, this doesn't check for leap years, etc.

    Recipe:
    Start with regex to find potential-dates, then attempt to map the integers onto month-day-year to filter the potential-dates into dates.  Finally, remove matches that are substrings of other matches to reduce noise.

    Note: Instead of using a lazy or greedy regex to find many dates over the full string, this uses a ^...$ regex against every substring of the password -- less performant but leads to every possible date match.

-}
dateMatch : String -> List Match
dateMatch password =
    let
        len =
            String.length password

        noSepMatches =
            List.range 0 (len - 3)
                |> ListX.lift2 (\j i -> ( i, i + j )) [ 4, 5, 6, 7, 8 ]
                |> List.filter (\( _, j ) -> j <= len)
                -- All possible substrings of length 4-8; dates without separators are between length 4 ("1191") and 8 ("11111991")
                |> List.filterMap (matchAsNoSeparatorDate password)

        sepMatches =
            List.range 0 (len - 6)
                |> ListX.lift2 (\j i -> ( i, i + j )) [ 6, 7, 8, 9, 10 ]
                |> List.filter (\( _, j ) -> j <= len)
                -- Dates with separators are between length 6 '1/1/91' and 10 '11/11/1991'
                |> List.filterMap (matchAsSeparatorDate password)
    in
    -- Matches now contains all valid date strings in a way that is tricky to capture with regexes only.  While thorough, it will contain some unintuitive noise:
    -- "2015_06_04", in addition to matching 2015_06_04, will also contain 5(!) other date matches: 15_06_04, 5_06_04, ..., even 2015 (matched as 5/1/2020)
    --  To reduce noise, remove date matches that are strict substrings of others
    List.append noSepMatches sepMatches
        |> List.sortBy (\{ i, j } -> j - i)
        -- Sort shortest to longest
        |> List.foldr appendIfNonSubmatch []
        -- Fold from longest to shortest, keeping only non-substrings (impossible for a longer to be a substring of a shorter)
        |> sortMatches


{-| Match a specified substring of a password as a date with separators, e.g. "11/7/10".
-}
matchAsSeparatorDate : String -> ( Int, Int ) -> Maybe Match
matchAsSeparatorDate password ( i, j ) =
    let
        token =
            String.slice i j password
    in
    case Regex.findAtMost 1 potentialDateWithSeparatorRegex token of
        m :: _ ->
            groupsToSeparatedDate m.submatches
                |> Maybe.map
                    (\( sep, date ) -> makeDateMatch token ( i, j ) sep date)

        _ ->
            Nothing


{-| Match a specified substring of a password as a date without separators, e.g. "11710".
-}
matchAsNoSeparatorDate : String -> ( Int, Int ) -> Maybe Match
matchAsNoSeparatorDate password ( i, j ) =
    let
        token =
            String.slice i j password
    in
    case Regex.findAtMost 1 potentialDateNoSeparatorRegex token of
        m :: _ ->
            let
                possibleSplits =
                    -- Lookup valid splits for a date of this length
                    Dict.get (String.length m.match) possibleDateSplits
                        |> Maybe.withDefault []
            in
            List.filterMap (splitToDate token) possibleSplits
                -- At this point: different possible dmy mappings for the same i,j substring.  Match the candidate date that likely takes the fewest guesses: a year closest to Scoring.referenceYear.  I.e., considering "111504", prefer 11-15-04 to 1-1-1504 (interpreting "04" as 2004).
                |> ListX.foldl1 keepMostRecentYear
                |> Maybe.map (makeDateMatch token ( i, j ) "")

        _ ->
            Nothing


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
mapIntsToDmy int1 int2 int3 =
    let
        middleIntCondition =
            -- Years cannot be in center of a date
            int2 > 31 || int2 <= 0

        over31Condition =
            (countIfOver31 int1 + countIfOver31 int2 + countIfOver31 int3) >= 2

        over12Condition =
            (countIfOver12 int1 + countIfOver12 int2 + countIfOver12 int3) == 3

        under1Condition =
            (countIfUnder1 int1 + countIfUnder1 int2 + countIfUnder1 int3) >= 2

        -- If any condition is true, can't be a date.
        conditions =
            [ yearBoundCheck int1, yearBoundCheck int2, yearBoundCheck int3, middleIntCondition, over31Condition, over12Condition, under1Condition ]
    in
    if List.any identity conditions then
        -- If any condition is true, can't be a date.
        Nothing

    else
        -- Try actually mapping the potential date.
        -- First look for a four digit year: yyyy + daymonth or daymonth + yyyy
        let
            possibleYearSplits =
                -- Year last
                [ ( int3, ( int1, int2 ) )
                , -- Year first
                  ( int1, ( int2, int3 ) )
                ]

            mapToDate ( y, rest ) =
                if dateMinYear > y || y > dateMaxYear then
                    -- If the year is out of bounds, not a date.
                    Nothing

                else
                    -- For a candidate that includes a four-digit year, when the remaining ints don't match to a day and month, it is not a date.
                    mapIntsToDm rest
                        |> Maybe.map
                            (\( month, day ) ->
                                { year = y, month = month, day = day }
                            )
        in
        case List.filterMap mapToDate possibleYearSplits of
            h :: _ ->
                -- Found a 4-digit year date
                Just h

            [] ->
                -- Given no four-digit year, two digit years are the most flexible int to match, so try to parse a day-month out of the remainder
                case
                    List.filterMap
                        (\( y, rest ) ->
                            mapToDate ( twoToFourDigitYear y, rest )
                        )
                        possibleYearSplits
                of
                    h :: _ ->
                        -- Found a 2-digit year date
                        Just h

                    [] ->
                        Nothing


{-| Performs all of the various matching techniques.
-}
omnimatch : List String -> String -> List Match
omnimatch userPatterns password =
    let
        userDict =
            List.indexedMap (\i s -> ( s, i + 1 )) userPatterns
                |> Dict.fromList

        dicts =
            if Dict.isEmpty userDict then
                frequencyDicts

            else
                ( "userInputs", userDict ) :: frequencyDicts

        matchers =
            [ dictionaryMatch dicts
            , reverseDictionaryMatch dicts
            , l33tMatch dicts l33tTable
            , spatialMatch adjacencyDicts
            , repeatMatch userPatterns
            , sequenceMatch
            , regexMatch
            , dateMatch
            ]
    in
    List.concatMap ((|>) password) matchers
        |> sortMatches



-- * Non-exposed below here


{-| Returns a `Maybe Match` for a slice of a password and a dictionary. Used to `filterMap` every `(i, j)` for every frequency dictionary.
-}
matchDict : String -> String -> ( Int, Int ) -> NamedFrequencyDict -> Maybe Match
matchDict substring password ( i, j ) ( dictName, dict ) =
    let
        makeMatch r =
            { pattern =
                DictionaryMatch
                    { dictionaryDetails = Dictionary
                    , matchedWord = substring
                    , rank = r
                    , dictionaryName = dictName
                    }
            , i = i
            , j = j - 1 -- Elm string slicing works differently, so j's are off by one.
            , token = String.slice i j password
            }
    in
    Maybe.map makeMatch <| Dict.get substring dict


{-| Given a translation dictionary, create a human-readable version, like "@ -> a, ! -> 1".
-}
displaySub : TranslationDict -> String
displaySub sub =
    sub
        |> Dict.foldl
            (\k v acc ->
                (String.cons k " -> " ++ String.fromChar v) :: acc
            )
            []
        |> String.join ", "


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
findSpatialMatches : String -> ( String, AdjacencyDict ) -> List Match
findSpatialMatches password ( graphName, graph ) =
    let
        makeSpatialMatch turns shiftedCount ( i, j ) =
            { pattern =
                SpatialMatch
                    { graph = graphName
                    , turns = turns
                    , shiftedCount = shiftedCount
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


{-| Greedy repeat matching.
-}
greedyRegex : Regex
greedyRegex =
    Regex.fromString "(.+)\\1+"
        |> Maybe.withDefault Regex.never


{-| Lazy repeat matching.
-}
lazyRegex : Regex
lazyRegex =
    Regex.fromString "(.+?)\\1+"
        |> Maybe.withDefault Regex.never


{-| Lazy matching anchored to start of string for determining base token.
-}
lazyAnchoredRegex : Regex
lazyAnchoredRegex =
    Regex.fromString "^(.+?)\\1+$"
        |> Maybe.withDefault Regex.never


{-| Matches a substring for repeating sequences, appending the first that is encountered and returning the new offset or `Nothing`.
-}
addRepeatMatch : List String -> Int -> List Match -> String -> ( Maybe Int, List Match )
addRepeatMatch userPatterns offset matches substring =
    let
        getBaseToken m =
            case Regex.findAtMost 1 lazyAnchoredRegex m of
                h :: _ ->
                    h.submatches

                _ ->
                    []
    in
    case Regex.findAtMost 1 greedyRegex substring of
        gM :: _ ->
            let
                gLength =
                    String.length gM.match

                ( match, matchLength, baseTokenList ) =
                    case Regex.findAtMost 1 lazyRegex substring of
                        lM :: _ ->
                            let
                                lLength =
                                    String.length lM.match
                            in
                            if gLength > lLength then
                                -- Greedy beats lazy for "aabaab"
                                -- Greedy: [aabaab, aab]
                                -- Lazy:   [aa,     a]
                                -- Greedy's repeated string might itself be repeated, eg. aabaab in aabaabaabaab.  Run an anchored lazy match on greedy's repeated string to find the shortest repeated string
                                ( gM, gLength, getBaseToken gM.match )

                            else
                                -- Lazy beats greedy for "aaaaa"
                                -- Greedy: [aaaa,  aa]
                                -- Lazy:   [aaaaa, a]
                                ( lM, lLength, lM.submatches )

                        _ ->
                            ( gM, gLength, getBaseToken gM.match )

                baseToken =
                    case baseTokenList of
                        (Just s) :: _ ->
                            s

                        _ ->
                            ""

                ( i, j ) =
                    ( match.index, match.index + matchLength - 1 )

                -- Recursively match and score the base string
                baseAnalysis =
                    mostGuessableMatchSequence False baseToken <| omnimatch userPatterns baseToken
            in
            ( Just (offset + j + 1)
            , { pattern =
                    RepeatMatch
                        { baseToken = baseToken
                        , repeatCount = matchLength // String.length baseToken
                        , baseGuesses = baseAnalysis.guesses
                        , baseMatches = baseAnalysis.sequence
                        }
              , i = i + offset
              , j = j + offset
              , token = match.match
              }
                :: matches
            )

        _ ->
            ( Nothing, matches )


{-| Regex used to find recent years (1900--2019) for Regex matching.
-}
recentYearRegex : Regex
recentYearRegex =
    -- ! This will need updating in 2020
    Regex.fromString "19\\d\\d|200\\d|201\\d"
        |> Maybe.withDefault Regex.never


{-| Regex for finding a potential date without separators (4--8 digits in a row). Matches will be subsequently checked to confirm they are actual dates, rather than invalid posibilities like "99999999".
-}
potentialDateNoSeparatorRegex : Regex
potentialDateNoSeparatorRegex =
    Regex.fromString "^\\d{4,8}$"
        |> Maybe.withDefault Regex.never


{-| Regex for finding a potential date with separators (1--4 digits, separators, 1--2 digits, separator, 1--4 digits). Matches will be subsequently checked to confirm they are actual dates, rather than invalid posibilities like "99/99/99".
-}
potentialDateWithSeparatorRegex : Regex
potentialDateWithSeparatorRegex =
    --     ^
    --     ( \d{1,4} )    -- day, month, year
    --     ( [\s/\\_.-] ) -- separator
    --     ( \d{1,2} )    -- day, month
    --     \2             -- same separator
    --     ( \d{1,4} )    -- day, month, year
    --     $
    Regex.fromString "^(\\d{1,4})([\\s\\/\\\\_.-])(\\d{1,2})\\2(\\d{1,4})$"
        |> Maybe.withDefault Regex.never


{-| Append a new match only if it isn't a substring of another match. Matches must be sorted by increasing length for a `foldr`.
-}
appendIfNonSubmatch : Match -> List Match -> List Match
appendIfNonSubmatch m matches =
    if List.any (\{ i, j } -> i <= m.i && j >= m.j) matches then
        -- If the new match is a substring of another, drop it
        matches

    else
        -- Otherwise, keep it.
        m :: matches


{-| Map two ints to a (month, day) if possible.
-}
mapIntsToDm : ( Int, Int ) -> Maybe ( Int, Int )
mapIntsToDm ( int1, int2 ) =
    if 1 <= int1 && int1 <= 31 && 1 <= int2 && int2 <= 12 then
        Just ( int2, int1 )

    else if 1 <= int2 && int2 <= 31 && 1 <= int1 && int1 <= 12 then
        Just ( int1, int2 )

    else
        -- If something is out of bounds, can't be a date.
        Nothing


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


{-| Turn a token, ij tuple, separator, and date into a match.
-}
makeDateMatch : String -> ( Int, Int ) -> String -> Date -> Match
makeDateMatch token ( i, j ) sep { day, month, year } =
    { pattern =
        DateMatch
            { separator = sep
            , year = year
            , month = month
            , day = day
            }
    , i = i
    , j = j - 1
    , token = token
    }


{-| Given the regex groups from `potentialDateWithSeparatorRegex`, try to map them to a valid date.
-}
groupsToSeparatedDate : List (Maybe String) -> Maybe ( String, Date )
groupsToSeparatedDate groups =
    case groups of
        -- Pattern-match required groups
        (Just int1) :: (Just sep) :: (Just int2) :: (Just int3) :: _ ->
            case ( String.toInt int1, String.toInt int2, String.toInt int3 ) of
                -- Convert to ints
                ( Just i1, Just i2, Just i3 ) ->
                    mapIntsToDmy i1 i2 i3
                        |> Maybe.map (\d -> ( sep, d ))

                _ ->
                    Nothing

        _ ->
            Nothing


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


{-| Given a matched string and a way to split it (of the type stored in `possibleDateSplits`), return a date if possible.
-}
splitToDate : String -> ( Int, Int ) -> Maybe Date
splitToDate token ( s1, s2 ) =
    case
        ( String.toInt <| String.left s1 token
        , String.toInt <| String.slice s1 s2 token
        , String.toInt <| String.dropLeft s2 token
        )
    of
        ( Just i1, Just i2, Just i3 ) ->
            mapIntsToDmy i1 i2 i3

        _ ->
            Nothing


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
twoToFourDigitYear : Int -> Int
twoToFourDigitYear y =
    if y > 99 then
        y

    else if y > 50 then
        -- 87 -> 1987
        y + 1900

    else
        -- 15 -> 2015
        y + 2000


{-| Ensure that an integer is a valid year.
-}
yearBoundCheck : Int -> Bool
yearBoundCheck i =
    -- If a number is a four-digit year, it must be between `dateMinYear` and `dateMaxYear`
    (99 < i && i < dateMinYear) || i > dateMaxYear


{-| 1 if an integer is over 31, 0 otherwise.
-}
countIfOver31 : Int -> Int
countIfOver31 i =
    if i > 31 then
        1

    else
        0


{-| 1 if an integer is over 12, 0 otherwise.
-}
countIfOver12 : Int -> Int
countIfOver12 i =
    if i > 12 then
        1

    else
        0


{-| 1 if an integer is under 1, 0 otherwise
-}
countIfUnder1 : Int -> Int
countIfUnder1 i =
    if i <= 0 then
        1

    else
        0


{-| Used for determining whether a character is "shifted" or not for spatial matching.
-}
shiftedChars : Set Char
shiftedChars =
    Set.fromList [ '[', '~', '!', '@', '#', '$', '%', '^', '&', '*', '(', ')', '_', '+', 'Q', 'W', 'E', 'R', 'T', 'Y', 'U', 'I', 'O', 'P', '{', '}', '|', 'A', 'S', 'D', 'F', 'G', 'H', 'J', 'K', 'L', ':', '"', 'Z', 'X', 'C', 'V', 'B', 'N', 'M', '<', '>', '?', ']' ]
