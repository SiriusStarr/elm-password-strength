module TestZxcvbnScoring exposing (suiteDateGuessses, suiteDictionaryGuesses, suiteEstimateGuesses, suiteL33tVariations, suiteRegexGuesses, suiteRepeatGuesses, suiteSearch, suiteSequenceGuesses, suiteSpatialGuesses, suiteUppercaseVariations, suitenCk)

import Dict
import Expect
import Internal.Zxcvbn.AdjacencyGraphs exposing (keyboardAverageDegree, keyboardStartingPositions)
import Internal.Zxcvbn.Matching exposing (omnimatch)
import Internal.Zxcvbn.Scoring exposing (dateGuesses, dictionaryGuesses, estimateGuesses, l33tVariations, minYearSpace, mostGuessableMatchSequence, mostGuessableMatchSequenceHelper, nCk, referenceYear, regexGuesses, repeatGuesses, sequenceGuesses, spatialGuesses, uppercaseVariations)
import Test exposing (Test, describe, test)
import Zxcvbn.MatchTypes exposing (DictionaryDetails(..), MatchDetails(..), ScoredMatch)


suitenCk : Test
suitenCk =
    describe "nCk"
        [ test "Matches expected values, including edge cases." <|
            \() ->
                let
                    nkResults =
                        [ ( 0, 0, 1 )
                        , ( 1, 0, 1 )
                        , ( 5, 0, 1 )
                        , ( 0, 1, 0 )
                        , ( 0, 5, 0 )
                        , ( 2, 1, 2 )
                        , ( 4, 2, 6 )
                        , ( 33, 7, 4272048 )
                        ]

                    tests =
                        List.map
                            (\( n, k, result ) ->
                                \() -> Expect.equal result <| nCk n k
                            )
                            nkResults
                in
                Expect.all tests ()
        , test "Mirror identity" <|
            \() ->
                let
                    n =
                        49

                    k =
                        12
                in
                Expect.equal (nCk n k) (nCk n (n - k))
        , test "Pascal's triangle identity" <|
            \() ->
                let
                    n =
                        49

                    k =
                        12
                in
                Expect.equal (nCk n k) (nCk (n - 1) (k - 1) + nCk (n - 1) k)
        ]


makePlaceholderMatch : Int -> Int -> Int -> ScoredMatch
makePlaceholderMatch i j guesses =
    { i = i
    , j = j
    , token = "placeholder"
    , pattern =
        SpatialMatch
            { graph = ""
            , turns = -1
            , shiftedCount = -1
            }
    , guesses = guesses
    , guessesLog10 = logBase 10 <| toFloat guesses
    }


suiteSequenceGuesses : Test
suiteSequenceGuesses =
    describe "sequenceGuesses"
        [ test "Sequence patterns should have correct guesses" <|
            \() ->
                let
                    tokens =
                        [ "ab", "XYZ", "4567", "7654", "ZYX" ]

                    ascending =
                        [ True, True, True, False, False ]

                    guesses =
                        [ 4 * 2 -- Obvious start * len-2
                        , 26 * 3 -- base26 * len-3
                        , 10 * 4 -- base10 * len-4
                        , 10 * 4 * 2 -- base10 * len 4 * descending
                        , 4 * 3 * 2 -- obvious start * len-3 * descending
                        ]

                    details =
                        List.map
                            (\a -> { ascending = a })
                            ascending

                    expectations =
                        List.map3 (\t d g -> \() -> Expect.equal g <| sequenceGuesses t d) tokens details guesses
                in
                Expect.all expectations ()
        ]


suiteRegexGuesses : Test
suiteRegexGuesses =
    describe "regexGuesses"
        [ -- ! These simply aren't used so they have been optimized out
          --     test "Guesses of 26^7 for 7-char lowercase regex" <|
          --     \() ->
          --         let
          --             token =
          --                 "aizocdk"
          --             regexName =
          --                 "alphaLower"
          --             match =
          --                 { pattern =
          --                     RegexMatch
          --                         { regexName = regexName
          --                         , regexMatch =
          --                             { match = ""
          --                             , index = 0
          --                             , number = 0
          --                             , submatches = []
          --                             }
          --                         }
          --                 , token = token
          --                 }
          --         in
          --         Expect.equal (26 ^ 7) <| regexGuesses match
          -- , test "Guesses of 62^5 for 5-char alphanumeric regex" <|
          --     \() ->
          --         let
          --             token =
          --                 "ag7C8"
          --             regexName =
          --                 "alphanumeric"
          --             match =
          --                 { pattern =
          --                     RegexMatch
          --                         { regexName = regexName
          --                         , regexMatch =
          --                             { match = ""
          --                             , index = 0
          --                             , number = 0
          --                             , submatches = []
          --                             }
          --                         }
          --                 , token = token
          --                 }
          --         in
          --         Expect.equal ((2 * 26 + 10) ^ 5) <| regexGuesses match
          -- ,
          test "Guesses of |year - referenceYear| for distant year matches" <|
            \() ->
                let
                    token =
                        "1972"

                    -- ! Not used
                    -- regexName =
                    --     "recentYear"
                    -- match =
                    --     { pattern =
                    --         RegexMatch
                    --             { regexName = regexName
                    --             , regexMatch =
                    --                 { match = ""
                    --                 , index = 0
                    --                 , number = 0
                    --                 , submatches = []
                    --                 }
                    --             }
                    --     , token = token
                    --     }
                in
                Expect.equal (abs (referenceYear - 1972)) <| regexGuesses token
        , test "Guesses of minYearSpace for a year close to referenceYear" <|
            \() ->
                let
                    token =
                        "2005"

                    -- ! Not used
                    -- regexName =
                    --     "recentYear"
                    -- match =
                    --     { pattern =
                    --         RegexMatch
                    --             { regexName = regexName
                    --             , regexMatch =
                    --                 { match = ""
                    --                 , index = 0
                    --                 , number = 0
                    --                 , submatches = []
                    --                 }
                    --             }
                    --     , token = token
                    --     }
                in
                Expect.equal minYearSpace <| regexGuesses token
        ]


suiteDateGuessses : Test
suiteDateGuessses =
    describe "dateGuesses"
        [ test "Guesses for date is 365 * distance from the ref year" <|
            \() ->
                let
                    year =
                        1923

                    details =
                        { separator = ""
                        , year = year
                        }
                in
                Expect.equal (365 * (abs referenceYear - year)) <| dateGuesses details
        , test "Recent years assume minYearSpace; extra guesses are added for separators." <|
            \() ->
                let
                    details =
                        { separator = "/"
                        , year = 2010
                        }
                in
                Expect.equal (365 * minYearSpace * 4) <| dateGuesses details
        ]


suiteSpatialGuesses : Test
suiteSpatialGuesses =
    describe "spatialGuesses"
        [ test "With no turns or shifts, guesses is starts * degree * (len-1)" <|
            \() ->
                let
                    token =
                        "zxcvbn"

                    details =
                        { graph = "qwerty"
                        , turns = 1
                        , shiftedCount = 0
                        }

                    -- -1 term because: not counting spatial patterns of length 1, eg for length==6, multiplier is 5 for needing to try len2,len3,..,len6
                    baseGuesses =
                        keyboardStartingPositions * keyboardAverageDegree * toFloat (String.length token - 1)
                in
                Expect.equal (round baseGuesses) <| spatialGuesses token details
        , test "Guesses is added for shifted keys, similar to capitals in dictionary matching" <|
            \() ->
                let
                    token =
                        "ZxCvbn"

                    details =
                        { graph = "qwerty"
                        , turns = 1
                        , shiftedCount = 2
                        }

                    baseGuesses =
                        keyboardStartingPositions * keyboardAverageDegree * toFloat (String.length token - 1)

                    shiftedGuesses =
                        round baseGuesses * (nCk 6 2 + nCk 6 1)
                in
                Expect.equal shiftedGuesses <| spatialGuesses token details
        , test "When everything is shifted, guesses are doubled" <|
            \() ->
                let
                    token =
                        "ZXCVBN"

                    details =
                        { graph = "qwerty"
                        , turns = 1
                        , shiftedCount = 6
                        }

                    baseGuesses =
                        keyboardStartingPositions * keyboardAverageDegree * toFloat (String.length token - 1)

                    shiftedGuesses =
                        round baseGuesses * 2
                in
                Expect.equal shiftedGuesses <| spatialGuesses token details
        , test "Spatial guesses accounts for turn positions, directions and starting keys" <|
            \() ->
                let
                    turns =
                        3

                    token =
                        "zxcft6yh"

                    details =
                        { graph = "qwerty"
                        , turns = turns
                        , shiftedCount = 0
                        }

                    l =
                        String.length token

                    s =
                        keyboardStartingPositions

                    d =
                        keyboardAverageDegree

                    guesses =
                        List.range 2 l
                            |> List.concatMap
                                (\i ->
                                    List.range 1 (min turns (i - 1))
                                        |> List.map
                                            (\j ->
                                                (d ^ toFloat j) * s * toFloat (nCk (i - 1) (j - 1))
                                            )
                                )
                            |> List.sum
                            |> round
                in
                Expect.equal guesses <| spatialGuesses token details
        ]


suiteDictionaryGuesses : Test
suiteDictionaryGuesses =
    describe "dictionaryGuesses"
        [ test "Base guesses == the rank" <|
            \() ->
                let
                    token =
                        "aaaaa"

                    details =
                        { dictionaryDetails = Dictionary
                        , rank = 32
                        }
                in
                Expect.equal 32 <| dictionaryGuesses token details
        , test "Extra guesses are added for capitalization" <|
            \() ->
                let
                    token =
                        "AAAaaa"

                    details =
                        { dictionaryDetails = Dictionary
                        , rank = 32
                        }
                in
                Expect.equal (32 * uppercaseVariations token) <| dictionaryGuesses token details
        , test "Guesses are doubled when word is reversed" <|
            \() ->
                let
                    token =
                        "aaa"

                    details =
                        { dictionaryDetails = ReverseDictionary
                        , rank = 32
                        }
                in
                Expect.equal (32 * 2) <| dictionaryGuesses token details
        , test "Extra guesses are added for common l33t substitutions" <|
            \() ->
                let
                    sub =
                        Dict.fromList [ ( '@', 'a' ) ]

                    token =
                        "aaa@@@"

                    details =
                        { dictionaryDetails =
                            L33tDictionary
                                { subDisplay = ""
                                , sub = sub
                                }
                        , rank = 32
                        }
                in
                Expect.equal (32 * l33tVariations sub token) <| dictionaryGuesses token details
        , test "Extra guesses are added for both capitalization and common l33t substitutions" <|
            \() ->
                let
                    sub =
                        Dict.fromList [ ( '@', 'a' ) ]

                    token =
                        "AaA@@@"

                    details =
                        { dictionaryDetails =
                            L33tDictionary
                                { subDisplay = ""
                                , sub = sub
                                }
                        , rank = 32
                        }
                in
                Expect.equal (32 * l33tVariations sub token * uppercaseVariations token) <| dictionaryGuesses token details
        ]


suiteUppercaseVariations : Test
suiteUppercaseVariations =
    describe "uppercaseVariations"
        [ test "Guess multipliers should be correct" <|
            \() ->
                let
                    wordVariants =
                        [ ( "", 1 )
                        , ( "a", 1 )
                        , ( "A", 2 )
                        , ( "Abcdef", 2 )
                        , ( "abcdef", 1 )
                        , ( "abcdeF", 2 )
                        , ( "ABCDEF", 2 )
                        , ( "aBcdef", nCk 6 1 )
                        , ( "aBcDef", nCk 6 1 + nCk 6 2 )
                        , ( "ABCDEf", nCk 6 1 )
                        , ( "aBCDEf", nCk 6 1 + nCk 6 2 )
                        , ( "ABCdef", nCk 6 1 + nCk 6 2 + nCk 6 3 )
                        ]

                    tests =
                        List.map
                            (\( token, result ) ->
                                \() -> Expect.equal result <| uppercaseVariations token
                            )
                            wordVariants
                in
                Expect.all tests ()
        ]


suiteL33tVariations : Test
suiteL33tVariations =
    describe "l33tVariations"
        [ test "Correct extra l33t guesses" <|
            \() ->
                let
                    testCases =
                        [ ( "", 1, Dict.empty )
                        , ( "a", 1, Dict.empty )
                        , ( "4", 2, Dict.fromList [ ( '4', 'a' ) ] )
                        , ( "4pple", 2, Dict.fromList [ ( '4', 'a' ) ] )
                        , ( "abcet", 1, Dict.empty )
                        , ( "4bcet", 2, Dict.fromList [ ( '4', 'a' ) ] )
                        , ( "a8cet", 2, Dict.fromList [ ( '8', 'b' ) ] )
                        , ( "abce+", 2, Dict.fromList [ ( '+', 't' ) ] )
                        , ( "48cet", 4, Dict.fromList [ ( '4', 'a' ), ( '8', 'b' ) ] )
                        , ( "a4a4aa", nCk 6 2 + nCk 6 1, Dict.fromList [ ( '4', 'a' ) ] )
                        , ( "4a4a44", nCk 6 2 + nCk 6 1, Dict.fromList [ ( '4', 'a' ) ] )
                        , ( "a44att+", (nCk 4 2 + nCk 4 1) * nCk 3 1, Dict.fromList [ ( '4', 'a' ), ( '+', 't' ) ] )
                        ]

                    tests =
                        List.map
                            (\( word, variants, sub ) ->
                                \() -> Expect.equal variants <| l33tVariations sub word
                            )
                            testCases
                in
                Expect.all tests ()
        , test "Capitalization doesn't affect extra l33t guesses calc" <|
            \() ->
                let
                    token =
                        "Aa44aA"

                    sub =
                        Dict.fromList [ ( '4', 'a' ) ]

                    variants =
                        nCk 6 2 + nCk 6 1
                in
                Expect.equal variants <| l33tVariations sub token
        ]


suiteEstimateGuesses : Test
suiteEstimateGuesses =
    describe "estimateGuesses"
        [ test "estimateGuesses delegates based on pattern" <|
            \() ->
                let
                    details =
                        { year = 1977
                        , month = 7
                        , day = 14
                        , separator = ""
                        }

                    match =
                        { pattern =
                            DateMatch details
                        , token = "1977"
                        }
                in
                Expect.equal (dateGuesses details) (estimateGuesses match "1977")
        ]


suiteSearch : Test
suiteSearch =
    describe "mostGuessableMatchSequence"
        -- * All cases use mostGuessableMatchSequenceHelper, which skips the guess scoring, so as to control match guesses.
        [ test "Returns one bruteforce match given an empty match sequence" <|
            \() ->
                let
                    password =
                        "0123456789"

                    -- For tests, set additive penalty to zero.
                    result =
                        mostGuessableMatchSequenceHelper True password []

                    match =
                        List.head result.sequence
                            |> Maybe.withDefault
                                { i = -1
                                , j = -1
                                , pattern =
                                    RegexMatch
                                        { regexName = "bad"
                                        , regexMatch =
                                            { match = ""
                                            , index = 0
                                            , number = 0
                                            , submatches = []
                                            }
                                        }
                                , token = ""
                                , guesses = -1
                                , guessesLog10 = -1
                                }

                    tests =
                        [ \() -> Expect.equal 1 <| List.length result.sequence
                        , \() -> Expect.equal BruteforceMatch match.pattern
                        , \() -> Expect.equal password match.token
                        , \() -> Expect.equal ( 0, 9 ) ( match.i, match.j )
                        ]
                in
                Expect.all tests ()
        , test "Returns one match given a match that spans the full password" <|
            \() ->
                let
                    password =
                        "0123456789"

                    inputMatch =
                        makePlaceholderMatch 0 9 1

                    -- For tests, set additive penalty to zero.
                    result =
                        mostGuessableMatchSequenceHelper True password [ inputMatch ]

                    match =
                        List.head result.sequence
                            |> Maybe.withDefault
                                { i = -1
                                , j = -1
                                , pattern =
                                    RegexMatch
                                        { regexName = "bad"
                                        , regexMatch =
                                            { match = ""
                                            , index = 0
                                            , number = 0
                                            , submatches = []
                                            }
                                        }
                                , token = ""
                                , guesses = -1
                                , guessesLog10 = -1
                                }

                    tests =
                        [ \() -> Expect.equal 1 <| List.length result.sequence
                        , \() -> Expect.equal 1 match.guesses
                        , \() -> Expect.equal "placeholder" match.token
                        , \() -> Expect.equal ( 0, 9 ) ( match.i, match.j )
                        ]
                in
                Expect.all tests ()
        , test "Returns match + bruteforce when match covers a prefix of password" <|
            \() ->
                let
                    password =
                        "0123456789"

                    inputMatch =
                        makePlaceholderMatch 0 5 1

                    -- For tests, set additive penalty to zero.
                    result =
                        mostGuessableMatchSequenceHelper True password [ inputMatch ]

                    tests =
                        [ \{ sequence } -> Expect.equal 2 <| List.length sequence
                        , \{ sequence } ->
                            case sequence of
                                first :: _ :: [] ->
                                    Expect.equal inputMatch first

                                _ ->
                                    Expect.fail "Received the wrong number of matches!"
                        , \{ sequence } ->
                            case sequence of
                                _ :: second :: [] ->
                                    Expect.equal BruteforceMatch second.pattern

                                _ ->
                                    Expect.fail "Received the wrong number of matches!"
                        , \{ sequence } ->
                            case sequence of
                                _ :: second :: [] ->
                                    Expect.equal ( 6, 9 ) ( second.i, second.j )

                                _ ->
                                    Expect.fail "Received the wrong number of matches!"
                        ]
                in
                Expect.all tests result
        , test "Returns bruteforce + match when match covers a suffix" <|
            \() ->
                let
                    password =
                        "0123456789"

                    inputMatch =
                        makePlaceholderMatch 3 9 1

                    -- For tests, set additive penalty to zero.
                    result =
                        mostGuessableMatchSequenceHelper True password [ inputMatch ]

                    tests =
                        [ \{ sequence } -> Expect.equal 2 <| List.length sequence
                        , \{ sequence } ->
                            case sequence of
                                _ :: second :: [] ->
                                    Expect.equal inputMatch second

                                _ ->
                                    Expect.fail "Received the wrong number of matches!"
                        , \{ sequence } ->
                            case sequence of
                                first :: _ :: [] ->
                                    Expect.equal BruteforceMatch first.pattern

                                _ ->
                                    Expect.fail "Received the wrong number of matches!"
                        , \{ sequence } ->
                            case sequence of
                                first :: _ :: [] ->
                                    Expect.equal ( 0, 2 ) ( first.i, first.j )

                                _ ->
                                    Expect.fail "Received the wrong number of matches!"
                        ]
                in
                Expect.all tests result
        , test "Returns bruteforce + match + bruteforce when match covers an infix" <|
            \() ->
                let
                    password =
                        "0123456789"

                    inputMatch =
                        makePlaceholderMatch 1 8 1

                    -- For tests, set additive penalty to zero.
                    result =
                        mostGuessableMatchSequenceHelper True password [ inputMatch ]

                    tests =
                        [ \{ sequence } -> Expect.equal 3 <| List.length sequence
                        , \{ sequence } ->
                            case sequence of
                                _ :: second :: _ :: [] ->
                                    Expect.equal inputMatch second

                                _ ->
                                    Expect.fail "Received the wrong number of matches!"
                        , \{ sequence } ->
                            case sequence of
                                first :: _ :: _ :: [] ->
                                    Expect.equal BruteforceMatch first.pattern

                                _ ->
                                    Expect.fail "Received the wrong number of matches!"
                        , \{ sequence } ->
                            case sequence of
                                first :: _ :: _ :: [] ->
                                    Expect.equal ( 0, 0 ) ( first.i, first.j )

                                _ ->
                                    Expect.fail "Received the wrong number of matches!"
                        , \{ sequence } ->
                            case sequence of
                                _ :: _ :: third :: [] ->
                                    Expect.equal BruteforceMatch third.pattern

                                _ ->
                                    Expect.fail "Received the wrong number of matches!"
                        , \{ sequence } ->
                            case sequence of
                                _ :: _ :: third :: [] ->
                                    Expect.equal ( 9, 9 ) ( third.i, third.j )

                                _ ->
                                    Expect.fail "Received the wrong number of matches!"
                        ]
                in
                Expect.all tests result
        , test "Chooses lower-guesses match given two matches of the same span" <|
            \() ->
                let
                    password =
                        "0123456789"

                    match1 =
                        makePlaceholderMatch 0 9 1

                    match2 =
                        makePlaceholderMatch 0 9 2

                    -- For tests, set additive penalty to zero.
                    result =
                        mostGuessableMatchSequenceHelper True password [ match1, match2 ]

                    match =
                        List.head result.sequence
                            |> Maybe.withDefault
                                { i = -1
                                , j = -1
                                , pattern =
                                    RegexMatch
                                        { regexName = "bad"
                                        , regexMatch =
                                            { match = ""
                                            , index = 0
                                            , number = 0
                                            , submatches = []
                                            }
                                        }
                                , token = ""
                                , guesses = -1
                                , guessesLog10 = -1
                                }

                    tests =
                        [ \() -> Expect.equal 1 <| List.length result.sequence
                        , \() -> Expect.equal match match1
                        ]
                in
                Expect.all tests ()
        , test "Make sure ordering doesn't matter" <|
            \() ->
                let
                    password =
                        "0123456789"

                    match1 =
                        makePlaceholderMatch 0 9 3

                    match2 =
                        makePlaceholderMatch 0 9 2

                    -- For tests, set additive penalty to zero.
                    result =
                        mostGuessableMatchSequenceHelper True password [ match1, match2 ]

                    match =
                        List.head result.sequence
                            |> Maybe.withDefault
                                { i = -1
                                , j = -1
                                , pattern =
                                    RegexMatch
                                        { regexName = "bad"
                                        , regexMatch =
                                            { match = ""
                                            , index = 0
                                            , number = 0
                                            , submatches = []
                                            }
                                        }
                                , token = ""
                                , guesses = -1
                                , guessesLog10 = -1
                                }

                    tests =
                        [ \() -> Expect.equal 1 <| List.length result.sequence
                        , \() -> Expect.equal match match2
                        ]
                in
                Expect.all tests ()
        , test "When m0 covers m1 and m2, choose [m0] when m0 < m1 * m2 * 2!" <|
            \() ->
                let
                    password =
                        "0123456789"

                    match1 =
                        makePlaceholderMatch 0 9 3

                    match2 =
                        makePlaceholderMatch 0 3 2

                    match3 =
                        makePlaceholderMatch 4 9 1

                    -- For tests, set additive penalty to zero.
                    result =
                        mostGuessableMatchSequenceHelper True password [ match1, match2, match3 ]

                    match =
                        List.head result.sequence
                            |> Maybe.withDefault
                                { i = -1
                                , j = -1
                                , pattern =
                                    RegexMatch
                                        { regexName = "bad"
                                        , regexMatch =
                                            { match = ""
                                            , index = 0
                                            , number = 0
                                            , submatches = []
                                            }
                                        }
                                , token = ""
                                , guesses = -1
                                , guessesLog10 = -1
                                }

                    tests =
                        [ \() -> Expect.equal 1 <| List.length result.sequence
                        , \() -> Expect.equal 3 result.guesses
                        , \() -> Expect.equal match match1
                        ]
                in
                Expect.all tests ()
        , test "When m0 covers m1 and m2, choose [m1, m2] when m0 > m1 * m2 * 2!" <|
            \() ->
                let
                    password =
                        "0123456789"

                    match1 =
                        makePlaceholderMatch 0 9 5

                    match2 =
                        makePlaceholderMatch 0 3 2

                    match3 =
                        makePlaceholderMatch 4 9 1

                    -- For tests, set additive penalty to zero.
                    result =
                        mostGuessableMatchSequenceHelper True password [ match1, match2, match3 ]

                    tests =
                        [ \() -> Expect.equal 2 <| List.length result.sequence
                        , \() -> Expect.equal 4 result.guesses
                        , \() -> Expect.equal [ match2, match3 ] result.sequence
                        ]
                in
                Expect.all tests ()
        ]


suiteRepeatGuesses : Test
suiteRepeatGuesses =
    describe "repeatGuesses"
        [ test "Repeat patterns have appropriate guesses" <|
            \() ->
                let
                    testCases =
                        [ ( "aa", "a", 2 )
                        , ( "999", "9", 3 )
                        , ( "$$$$", "$", 4 )
                        , ( "abab", "ab", 2 )
                        , ( "batterystaplebatterystaplebatterystaple", "batterystaple", 3 )
                        ]

                    baseGuesses =
                        List.map
                            (\( _, baseToken, _ ) ->
                                omnimatch [] baseToken
                                    |> mostGuessableMatchSequence False baseToken
                                    |> .guesses
                            )
                            testCases

                    detailsAndExpectedGuesses =
                        List.map2
                            (\( _, _, repeatCount ) bG ->
                                ( { baseGuesses = bG
                                  , repeatCount = repeatCount
                                  }
                                , bG * repeatCount
                                )
                            )
                            testCases
                            baseGuesses

                    tests =
                        List.map
                            (\( details, expectedGuesses ) ->
                                \() -> Expect.equal expectedGuesses <| repeatGuesses details
                            )
                            detailsAndExpectedGuesses
                in
                Expect.all tests ()
        ]
