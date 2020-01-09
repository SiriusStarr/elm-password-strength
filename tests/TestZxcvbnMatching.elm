module TestZxcvbnMatching exposing (suiteDateMatch, suiteDictionaryMatch, suiteL33tMatch, suiteOmnimatch, suiteRegexMatch, suiteRepeatMatch, suiteReverseDictionaryMatch, suiteSequenceMatch, suiteSortMatches, suiteSpatialMatch, suiteTranslate)

import Dict exposing (Dict)
import Expect
import Internal.Zxcvbn.AdjacencyGraphs exposing (dvorak, keypad, macKeypad, qwerty)
import Internal.Zxcvbn.FrequencyLists exposing (FrequencyDict)
import Internal.Zxcvbn.L33tTable exposing (SubstitutionDict)
import Internal.Zxcvbn.MatchTypes exposing (Match, sortMatches)
import Internal.Zxcvbn.Matching exposing (NamedFrequencyDict, dateMatch, dictionaryMatch, enumerateL33tSubs, frequencyDicts, l33tMatch, omnimatch, regexMatch, relevantL33tSubtable, repeatMatch, reverseDictionaryMatch, sequenceMatch, spatialMatch, translate)
import List.Extra as ListX
import Test exposing (Test, describe, test)
import Zxcvbn.MatchTypes exposing (DictionaryDetails(..), MatchDetails(..))


{-| Takes a pattern and list of prefixes/suffixes and returns a bunch of variants of that pattern embedded with each possible prefix/suffix combination, including no prefix/suffix. Returns a list of tuples (variant, (i, j)) where (i,j) is the start/end of the pattern, inclusive
-}
genPws : String -> List String -> List String -> List ( String, ( Int, Int ) )
genPws pattern inPrefixes inSuffixes =
    let
        prefixes =
            if List.member "" inPrefixes then
                inPrefixes

            else
                "" :: inPrefixes

        suffixes =
            if List.member "" inSuffixes then
                inSuffixes

            else
                "" :: inSuffixes
    in
    ListX.cartesianProduct [ prefixes, suffixes ]
        |> List.filterMap
            (\l ->
                case l of
                    p :: s :: [] ->
                        Just ( p ++ pattern ++ s, ( String.length p, String.length p + String.length pattern - 1 ) )

                    _ ->
                        Nothing
            )


suiteSortMatches : Test
suiteSortMatches =
    describe "sortMatches"
        [ test "Sorting an empty list leaves it empty" <|
            \() ->
                Expect.equal [] <| sortMatches []
        , test "Matches are sorted on i index primary, j secondary." <|
            \() ->
                let
                    m1 =
                        { i = 5, j = 5 }

                    m2 =
                        { i = 6, j = 7 }

                    m3 =
                        { i = 2, j = 5 }

                    m4 =
                        { i = 0, j = 0 }

                    m5 =
                        { i = 2, j = 3 }

                    m6 =
                        { i = 0, j = 3 }
                in
                Expect.equalLists [ m4, m6, m5, m3, m1, m2 ] <| sortMatches [ m1, m2, m3, m4, m5, m6 ]
        ]


suiteTranslate : Test
suiteTranslate =
    describe "translate"
        [ test "translates a string to a result with a provided charmap" <|
            \() ->
                let
                    charMap =
                        Dict.fromList [ ( 'a', 'A' ), ( 'b', 'B' ) ]

                    input =
                        [ ( "a", charMap, "A" )
                        , ( "c", charMap, "c" )
                        , ( "ab", charMap, "AB" )
                        , ( "abc", charMap, "ABc" )
                        , ( "aa", charMap, "AA" )
                        , ( "abab", charMap, "ABAB" )
                        , ( "", charMap, "" )
                        , ( "", Dict.empty, "" )
                        , ( "abc", Dict.empty, "abc" )
                        ]
                in
                Expect.true "Expected proper character substitution!" <| List.all (\( i, dict, o ) -> o == translate i dict) input
        ]


d1 : FrequencyDict
d1 =
    Dict.fromList [ ( "motherboard", 1 ), ( "mother", 2 ), ( "board", 3 ), ( "abcd", 4 ), ( "cdef", 5 ) ]


d2 : FrequencyDict
d2 =
    Dict.fromList [ ( "z", 1 ), ( "8", 2 ), ( "99", 3 ), ( "$", 4 ), ( "asdf1234&*", 5 ) ]


d3 : FrequencyDict
d3 =
    Dict.fromList
        [ ( "123", 1 )
        , ( "321", 2 )
        , ( "456", 3 )
        , ( "654", 4 )
        ]


testDicts : List NamedFrequencyDict
testDicts =
    [ ( "d1", d1 ), ( "d2", d2 ) ]


type PropertyExpectation
    = Dictionary
    | MatchedWord String
    | Rank Int
    | DictionaryName String
    | Reversed
    | L33t
    | Sub (Dict Char Char)
    | Graph String
    | Turns Int
    | ShiftedCount Int
    | Spatial
    | Sequence
    | Ascending Bool
    | SequenceName String
    | Repeat
    | BaseToken String
    | Regex
    | RegexName String
    | Date
    | Separator String
    | Year Int
    | Month Int
    | Day Int


checkProp : PropertyExpectation -> Match -> Expect.Expectation
checkProp e m =
    case e of
        Dictionary ->
            case m.pattern of
                DictionaryMatch _ ->
                    Expect.pass

                _ ->
                    Expect.fail "Expected dictionary match!"

        MatchedWord s ->
            case m.pattern of
                DictionaryMatch { matchedWord } ->
                    Expect.equal s matchedWord

                _ ->
                    Expect.fail "Expected dictionary match!"

        Rank i ->
            case m.pattern of
                DictionaryMatch { rank } ->
                    Expect.equal i rank

                _ ->
                    Expect.fail "Expected dictionary match!"

        DictionaryName n ->
            case m.pattern of
                DictionaryMatch { dictionaryName } ->
                    Expect.equal n dictionaryName

                _ ->
                    Expect.fail "Expected dictionary match!"

        Reversed ->
            case m.pattern of
                DictionaryMatch { dictionaryDetails } ->
                    case dictionaryDetails of
                        ReverseDictionary ->
                            Expect.pass

                        _ ->
                            Expect.fail "Expected reverse dictionary match!"

                _ ->
                    Expect.fail "Expected reverse dictionary match!"

        L33t ->
            case m.pattern of
                DictionaryMatch { dictionaryDetails } ->
                    case dictionaryDetails of
                        L33tDictionary _ ->
                            Expect.pass

                        _ ->
                            Expect.fail "Expected l33t match!"

                _ ->
                    Expect.fail "Expected l33t match!"

        Sub d ->
            case m.pattern of
                DictionaryMatch { dictionaryDetails } ->
                    case dictionaryDetails of
                        L33tDictionary { sub } ->
                            Expect.equal d sub

                        _ ->
                            Expect.fail "Expected l33t match!"

                _ ->
                    Expect.fail "Expected l33t match!"

        Spatial ->
            case m.pattern of
                SpatialMatch _ ->
                    Expect.pass

                _ ->
                    Expect.fail "Expected spatial match!"

        Graph s ->
            case m.pattern of
                SpatialMatch { graph } ->
                    Expect.equal s graph

                _ ->
                    Expect.fail "Expected spatial match!"

        Turns i ->
            case m.pattern of
                SpatialMatch { turns } ->
                    Expect.equal i turns

                _ ->
                    Expect.fail "Expected spatial match!"

        ShiftedCount i ->
            case m.pattern of
                SpatialMatch { shiftedCount } ->
                    Expect.equal i shiftedCount

                _ ->
                    Expect.fail "Expected spatial match!"

        Sequence ->
            case m.pattern of
                SequenceMatch _ ->
                    Expect.pass

                _ ->
                    Expect.fail "Expected sequence match!"

        Ascending b ->
            case m.pattern of
                SequenceMatch { ascending } ->
                    Expect.equal b ascending

                _ ->
                    Expect.fail "Expected sequence match!"

        SequenceName s ->
            case m.pattern of
                SequenceMatch { sequenceName } ->
                    Expect.equal s sequenceName

                _ ->
                    Expect.fail "Expected sequence match!"

        Repeat ->
            case m.pattern of
                RepeatMatch _ ->
                    Expect.pass

                _ ->
                    Expect.fail "Expected repeat match!"

        BaseToken s ->
            case m.pattern of
                RepeatMatch { baseToken } ->
                    Expect.equal s baseToken

                _ ->
                    Expect.fail "Expected repeat match!"

        Regex ->
            case m.pattern of
                RegexMatch _ ->
                    Expect.pass

                _ ->
                    Expect.fail "Expected regex match!"

        RegexName s ->
            case m.pattern of
                RegexMatch { regexName } ->
                    Expect.equal s regexName

                _ ->
                    Expect.fail "Expected regex match!"

        Date ->
            case m.pattern of
                DateMatch _ ->
                    Expect.pass

                _ ->
                    Expect.fail "Expected date match!"

        Separator s ->
            case m.pattern of
                DateMatch { separator } ->
                    Expect.equal s separator

                _ ->
                    Expect.fail "Expected date match!"

        Year i ->
            case m.pattern of
                DateMatch { year } ->
                    Expect.equal i year

                _ ->
                    Expect.fail "Expected date match!"

        Month i ->
            case m.pattern of
                DateMatch { month } ->
                    Expect.equal i month

                _ ->
                    Expect.fail "Expected date match!"

        Day i ->
            case m.pattern of
                DateMatch { day } ->
                    Expect.equal i day

                _ ->
                    Expect.fail "Expected date match!"


checkMatches : List Match -> List String -> List ( Int, Int ) -> List (List PropertyExpectation) -> Expect.Expectation
checkMatches matches patterns ijs propertyExpectations =
    let
        len =
            List.length patterns

        isEqualLengthArgs =
            List.all ((==) len) [ List.length ijs, List.length propertyExpectations ]

        matchesEqualLengthPatterns =
            List.length matches == List.length patterns

        checkMatch match pat ij properties =
            Expect.all
                ([ \{ i, j } -> Expect.equal ij ( i, j )
                 , \{ token } -> Expect.equal pat token
                 ]
                    ++ List.map checkProp properties
                )
                match
    in
    if isEqualLengthArgs then
        if matchesEqualLengthPatterns then
            Expect.all (List.map (\e -> \() -> e) <| List.map4 checkMatch matches patterns ijs propertyExpectations) ()

        else
            Expect.fail <| "Number of matches did not match expected number: got " ++ String.fromInt (List.length matches) ++ " expected " ++ String.fromInt (List.length patterns)

    else
        Expect.fail "Unequal argument lists passed to checkMatches"


suiteDictionaryMatch : Test
suiteDictionaryMatch =
    describe "dictionaryMatch"
        [ test "Matches words that contain other words" <|
            \() ->
                let
                    matches =
                        dictionaryMatch testDicts "motherboard"

                    patterns =
                        [ "mother", "motherboard", "board" ]

                    ijs =
                        [ ( 0, 5 ), ( 0, 10 ), ( 6, 10 ) ]

                    props =
                        [ [ Dictionary, MatchedWord "mother", Rank 2, DictionaryName "d1" ]
                        , [ Dictionary, MatchedWord "motherboard", Rank 1, DictionaryName "d1" ]
                        , [ Dictionary, MatchedWord "board", Rank 3, DictionaryName "d1" ]
                        ]
                in
                checkMatches matches patterns ijs props
        , test "Ignores uppercasing" <|
            \() ->
                let
                    matches =
                        dictionaryMatch testDicts "BoaRdZ"

                    patterns =
                        [ "BoaRd", "Z" ]

                    ijs =
                        [ ( 0, 4 ), ( 5, 5 ) ]

                    props =
                        [ [ Dictionary, MatchedWord "board", Rank 3, DictionaryName "d1" ]
                        , [ Dictionary, MatchedWord "z", Rank 1, DictionaryName "d2" ]
                        ]
                in
                checkMatches matches patterns ijs props
        , test "Identifies words surrounded by non-words" <|
            \() ->
                let
                    prefixes =
                        [ "q", "%%" ]

                    suffixes =
                        [ "%", "qq" ]

                    combinations =
                        genPws word prefixes suffixes

                    word =
                        "asdf1234&*"

                    props =
                        [ [ Dictionary, MatchedWord word, Rank 5, DictionaryName "d2" ] ]
                in
                Expect.all
                    (List.map
                        (\( p, ( i, j ) ) ->
                            let
                                matches =
                                    dictionaryMatch testDicts p
                            in
                            \() -> checkMatches matches [ word ] [ ( i, j ) ] props
                        )
                        combinations
                    )
                    ()
        , test "Matches against all words in provided dictionaries" <|
            \() ->
                Expect.all
                    (List.concatMap
                        (\( name, dict ) ->
                            List.map
                                (\( word, rank ) ->
                                    if word == "motherboard" then
                                        -- Skip words that contain others
                                        \() -> Expect.pass

                                    else
                                        let
                                            matches =
                                                dictionaryMatch testDicts word

                                            props =
                                                [ [ Dictionary, MatchedWord word, Rank rank, DictionaryName name ] ]
                                        in
                                        \() -> checkMatches matches [ word ] [ ( 0, String.length word - 1 ) ] props
                                )
                            <|
                                Dict.toList dict
                        )
                        testDicts
                    )
                    ()
        , test "Default dictionaries" <|
            \() ->
                let
                    matches =
                        dictionaryMatch frequencyDicts "wow"

                    props =
                        [ [ Dictionary, MatchedWord "wow", Rank 322, DictionaryName "usTvAndFilm" ]
                        ]
                in
                checkMatches matches [ "wow" ] [ ( 0, 2 ) ] props
        ]


suiteReverseDictionaryMatch : Test
suiteReverseDictionaryMatch =
    describe "reverseDictionaryMatch"
        [ test "Matches against reversed words" <|
            \() ->
                let
                    password =
                        "0123456789"

                    matches =
                        reverseDictionaryMatch [ ( "d3", d3 ) ] password

                    props =
                        [ [ Dictionary, MatchedWord "321", Reversed, Rank 2, DictionaryName "d3" ]
                        , [ Dictionary, MatchedWord "654", Reversed, Rank 4, DictionaryName "d3" ]
                        ]
                in
                checkMatches matches [ "123", "456" ] [ ( 1, 3 ), ( 4, 6 ) ] props
        ]


testTable : SubstitutionDict
testTable =
    Dict.fromList [ ( 'a', [ '4', '@' ] ), ( 'c', [ '(', '{', '[', '<' ] ), ( 'g', [ '6', '9' ] ), ( 'o', [ '0' ] ) ]


l33tDicts : List NamedFrequencyDict
l33tDicts =
    [ ( "words", Dict.fromList [ ( "aac", 1 ), ( "password", 3 ), ( "paassword", 4 ), ( "asdf0", 5 ) ] )
    , ( "words2", Dict.fromList [ ( "cgo", 1 ) ] )
    ]


suiteL33tMatch : Test
suiteL33tMatch =
    describe "L33t matching"
        [ test "relevantL33tSubtable reduces l33t table to only the substitutions that a password might be employing" <|
            \() ->
                let
                    combinations =
                        [ ( "", [] )
                        , ( "abcdefgo123578!#$&*)]}>", [] )
                        , ( "a", [] )
                        , ( "4", [ ( 'a', [ '4' ] ) ] )
                        , ( "4@", [ ( 'a', [ '4', '@' ] ) ] )
                        , ( "4({60", [ ( 'a', [ '4' ] ), ( 'c', [ '(', '{' ] ), ( 'g', [ '6' ] ), ( 'o', [ '0' ] ) ] )
                        ]
                in
                Expect.all (List.map (\( pw, expect ) -> always <| Expect.equalLists expect (Dict.toList <| relevantL33tSubtable testTable pw)) combinations) ()
        , test "enumerateL33tSubs enumerates the different sets of l33t substitutions a password might be using" <|
            \() ->
                let
                    combinations =
                        [ ( Dict.empty
                          , [ Dict.empty
                            ]
                          )
                        , ( Dict.fromList [ ( 'a', [ '@' ] ) ]
                          , [ Dict.fromList [ ( '@', 'a' ) ]
                            ]
                          )
                        , ( Dict.fromList [ ( 'a', [ '@', '4' ] ) ]
                          , [ Dict.fromList [ ( '@', 'a' ) ]
                            , Dict.fromList [ ( '4', 'a' ) ]
                            ]
                          )
                        , ( Dict.fromList [ ( 'a', [ '@', '4' ] ), ( 'c', [ '(' ] ) ]
                          , [ Dict.fromList [ ( '@', 'a' ), ( '(', 'c' ) ]
                            , Dict.fromList [ ( '4', 'a' ), ( '(', 'c' ) ]
                            ]
                          )
                        ]
                in
                Expect.all (List.map (\( table, subs ) -> always <| Expect.equalLists subs (enumerateL33tSubs table)) combinations) ()
        , test "Doesn't match \"\"" <|
            \() ->
                Expect.equal [] <| l33tMatch l33tDicts testTable ""
        , test "Doesn't match pure dictionary words" <|
            \() ->
                Expect.equal [] <| l33tMatch l33tDicts testTable "password"
        , test "Matches against common l33t substitutions" <|
            \() ->
                let
                    passwords =
                        [ "p4ssword", "p@ssw0rd", "aSdfO{G0asDfO" ]

                    patterns =
                        [ "p4ssword", "p@ssw0rd", "{G0" ]

                    ijs =
                        [ ( 0, 7 ), ( 0, 7 ), ( 5, 7 ) ]

                    props =
                        [ [ Dictionary, L33t, Sub (Dict.fromList [ ( '4', 'a' ) ]), MatchedWord "password", Rank 3, DictionaryName "words" ]
                        , [ Dictionary, L33t, Sub (Dict.fromList [ ( '@', 'a' ), ( '0', 'o' ) ]), MatchedWord "password", Rank 3, DictionaryName "words" ]
                        , [ Dictionary, L33t, Sub (Dict.fromList [ ( '{', 'c' ), ( '0', 'o' ) ]), MatchedWord "cgo", Rank 1, DictionaryName "words2" ]
                        ]

                    matches =
                        List.concatMap (l33tMatch l33tDicts testTable) passwords
                in
                checkMatches matches patterns ijs props
        , test "Matches against overlapping l33t patterns" <|
            \() ->
                let
                    patterns =
                        [ "@a(", "(go", "{G0" ]

                    ijs =
                        [ ( 0, 2 ), ( 2, 4 ), ( 5, 7 ) ]

                    props =
                        [ [ Dictionary, L33t, Sub (Dict.fromList [ ( '@', 'a' ), ( '(', 'c' ) ]), MatchedWord "aac", Rank 1, DictionaryName "words" ]
                        , [ Dictionary, L33t, Sub (Dict.fromList [ ( '(', 'c' ) ]), MatchedWord "cgo", Rank 1, DictionaryName "words2" ]
                        , [ Dictionary, L33t, Sub (Dict.fromList [ ( '{', 'c' ), ( '0', 'o' ) ]), MatchedWord "cgo", Rank 1, DictionaryName "words2" ]
                        ]

                    matches =
                        l33tMatch l33tDicts testTable "@a(go{G0"
                in
                checkMatches matches patterns ijs props
        , test "Doesn't match when multiple l33t substitutions are needed for the same letter" <|
            \() ->
                Expect.equal [] <| l33tMatch l33tDicts testTable "p4@ssword"
        , test "Doesn't match single-character l33ted words" <|
            \() ->
                Expect.equal [] <| l33tMatch l33tDicts testTable "4 1 @"
        , test "Doesn't match with subsets of possible l33t substitutions" <|
            \() ->
                Expect.equal [] <| l33tMatch l33tDicts testTable "4sdf0"
        ]


suiteSpatialMatch : Test
suiteSpatialMatch =
    describe "Spatial matching"
        [ test "Doesn't match 1- and 2-character spatial patterns" <|
            \() ->
                let
                    passwords =
                        [ "", "/", "qw", "*/" ]
                in
                Expect.equal [] <| List.concatMap (spatialMatch [ ( "qwerty", qwerty ) ]) passwords
        , test "Matches against spatial patterns surrounded by non-spatial patterns" <|
            \() ->
                let
                    matches =
                        spatialMatch [ ( "qwerty", qwerty ) ] "rz!6tfGHJ%z"

                    props =
                        [ [ Spatial, Graph "qwerty", Turns 2, ShiftedCount 3 ] ]
                in
                checkMatches matches [ "6tfGHJ" ] [ ( 3, 8 ) ] props
        , test "Matches various spatial patterns" <|
            \() ->
                let
                    patterns =
                        [ "12345", "@WSX", "6tfGHJ", "hGFd", "/;p09876yhn", "Xdr%", "159-", "*84", "/8520", "369", "/963.", "*-632.0214", "aoEP%yIxkjq:", ";qoaOQ:Aoq;a" ]

                    graphs =
                        [ [ ( "qwerty", qwerty ) ], [ ( "qwerty", qwerty ) ], [ ( "qwerty", qwerty ) ], [ ( "qwerty", qwerty ) ], [ ( "qwerty", qwerty ) ], [ ( "qwerty", qwerty ) ], [ ( "keypad", keypad ) ], [ ( "keypad", keypad ) ], [ ( "keypad", keypad ) ], [ ( "keypad", keypad ) ], [ ( "macKeypad", macKeypad ) ], [ ( "macKeypad", macKeypad ) ], [ ( "dvorak", dvorak ) ], [ ( "dvorak", dvorak ) ] ]

                    ijs =
                        List.map (\p -> ( 0, String.length p - 1 )) patterns

                    keyboard =
                        [ "qwerty", "qwerty", "qwerty", "qwerty", "qwerty", "qwerty", "keypad", "keypad", "keypad", "keypad", "macKeypad", "macKeypad", "dvorak", "dvorak" ]

                    turns =
                        [ 1, 1, 2, 1, 3, 1, 1, 1, 1, 1, 1, 9, 4, 11 ]

                    shifts =
                        [ 0, 4, 3, 2, 0, 2, 0, 0, 0, 0, 0, 0, 5, 4 ]

                    matches =
                        List.concat << List.map2 spatialMatch graphs <| patterns

                    props =
                        List.map3 (\g t s -> [ Spatial, Graph g, Turns t, ShiftedCount s ]) keyboard turns shifts
                in
                checkMatches matches patterns ijs props
        ]


suiteSequenceMatch : Test
suiteSequenceMatch =
    describe "Sequence matching"
        [ test "Doesn't match 1-character sequences" <|
            \() ->
                let
                    passwords =
                        [ "", "a", "1" ]
                in
                Expect.equal [] <| List.concatMap sequenceMatch passwords
        , test "Matches overlapping patterns" <|
            \() ->
                checkMatches (sequenceMatch "abcbabc") [ "abc", "cba", "abc" ] [ ( 0, 2 ), ( 2, 4 ), ( 4, 6 ) ] [ [ Sequence, Ascending True ], [ Sequence, Ascending False ], [ Sequence, Ascending True ] ]
        , test "Matches embedded sequence patterns" <|
            \() ->
                let
                    prefixes =
                        [ "!", "22" ]

                    suffixes =
                        [ "!", "22" ]

                    word =
                        "jihg"

                    combinations =
                        genPws word prefixes suffixes

                    props =
                        [ [ Sequence, Ascending False, SequenceName "lower" ] ]
                in
                Expect.all
                    (List.map
                        (\( p, ( i, j ) ) ->
                            let
                                matches =
                                    sequenceMatch p
                            in
                            \() -> checkMatches matches [ word ] [ ( i, j ) ] props
                        )
                        combinations
                    )
                    ()
        , test "Matches sequences" <|
            \() ->
                let
                    patterns =
                        [ "ABC", "CBA", "PQR", "RQP", "XYZ", "ZYX", "abcd", "dcba", "jihg", "wxyz", "zxvt", "0369", "97531" ]

                    names =
                        [ "upper", "upper", "upper", "upper", "upper", "upper", "lower", "lower", "lower", "lower", "lower", "digits", "digits" ]

                    isAscending =
                        [ True, False, True, False, True, False, True, False, False, True, False, True, False ]

                    matches =
                        List.concatMap sequenceMatch patterns

                    props =
                        List.map2 (\n a -> [ Sequence, Ascending a, SequenceName n ]) names isAscending

                    ijs =
                        List.map (\p -> ( 0, String.length p - 1 )) patterns
                in
                checkMatches matches patterns ijs props
        ]


suiteRepeatMatch : Test
suiteRepeatMatch =
    describe "Repeat matching"
        [ test "Doesn't match 1-character repeats" <|
            \() ->
                let
                    passwords =
                        [ "", "#" ]
                in
                Expect.equal [] <| List.concatMap (repeatMatch []) passwords
        , test "Matches embedded repeat patterns" <|
            \() ->
                let
                    prefixes =
                        [ "@", "y4@" ]

                    suffixes =
                        [ "u", "u%7" ]

                    word =
                        "&&&&&"

                    combinations =
                        genPws word prefixes suffixes

                    props =
                        [ [ Repeat, BaseToken "&" ] ]
                in
                Expect.all
                    (List.map
                        (\( p, ( i, j ) ) ->
                            let
                                matches =
                                    repeatMatch [] p
                            in
                            \() -> checkMatches matches [ word ] [ ( i, j ) ] props
                        )
                        combinations
                    )
                    ()
        , test "Matches repeats with base character" <|
            \() ->
                let
                    chars =
                        [ "a", "Z", "4", "&" ]

                    lengths =
                        [ 3, 12 ]

                    patterns =
                        ListX.lift2 (\c i -> ( c, String.repeat i c )) chars lengths

                    matches =
                        List.concatMap (repeatMatch [] << Tuple.second) patterns

                    ijs =
                        List.map (\( _, p ) -> ( 0, String.length p - 1 )) patterns

                    props =
                        List.map (\( c, _ ) -> [ Repeat, BaseToken c ]) patterns
                in
                checkMatches matches (List.map Tuple.second patterns) ijs props
        , test "Matches multiple adjacent repeats" <|
            \() ->
                let
                    patterns =
                        [ "BBB", "1111", "aaaaa", "@@@@@@" ]

                    matches =
                        repeatMatch [] "BBB1111aaaaa@@@@@@"

                    ijs =
                        [ ( 0, 2 ), ( 3, 6 ), ( 7, 11 ), ( 12, 17 ) ]

                    props =
                        [ [ Repeat, BaseToken "B" ]
                        , [ Repeat, BaseToken "1" ]
                        , [ Repeat, BaseToken "a" ]
                        , [ Repeat, BaseToken "@" ]
                        ]
                in
                checkMatches matches patterns ijs props
        , test "Matches multiple repeats with non-repeats in-between" <|
            \() ->
                let
                    patterns =
                        [ "BBB", "1111", "aaaaa", "@@@@@@" ]

                    matches =
                        repeatMatch [] "2818BBBbzsdf1111@*&@!aaaaaEUDA@@@@@@1729"

                    ijs =
                        [ ( 4, 6 ), ( 12, 15 ), ( 21, 25 ), ( 30, 35 ) ]

                    props =
                        [ [ Repeat, BaseToken "B" ]
                        , [ Repeat, BaseToken "1" ]
                        , [ Repeat, BaseToken "a" ]
                        , [ Repeat, BaseToken "@" ]
                        ]
                in
                checkMatches matches patterns ijs props
        , test "Matches multi-character repeat pattern" <|
            \() ->
                let
                    pattern =
                        "abab"

                    matches =
                        repeatMatch [] pattern

                    ijs =
                        [ ( 0, String.length pattern - 1 ) ]

                    props =
                        [ [ Repeat, BaseToken "ab" ] ]
                in
                checkMatches matches [ pattern ] ijs props
        , test "Matches aabaab as a repeat instead of the aa prefix" <|
            \() ->
                let
                    pattern =
                        "aabaab"

                    matches =
                        repeatMatch [] pattern

                    ijs =
                        [ ( 0, String.length pattern - 1 ) ]

                    props =
                        [ [ Repeat, BaseToken "aab" ] ]
                in
                checkMatches matches [ pattern ] ijs props
        , test "Identifies ab as repeat string, even though abab is also repeated" <|
            \() ->
                let
                    pattern =
                        "abababab"

                    matches =
                        repeatMatch [] pattern

                    ijs =
                        [ ( 0, String.length pattern - 1 ) ]

                    props =
                        [ [ Repeat, BaseToken "ab" ] ]
                in
                checkMatches matches [ pattern ] ijs props
        ]


suiteRegexMatch : Test
suiteRegexMatch =
    describe "Regex matching"
        [ test "Matches recentYear pattern" <|
            \() ->
                let
                    patterns =
                        [ "1922", "2017", "2020" ]

                    names =
                        [ "recentYear", "recentYear", "recentYear" ]

                    matches =
                        List.concatMap regexMatch patterns

                    ijs =
                        List.map (\p -> ( 0, String.length p - 1 )) patterns

                    props =
                        List.map (\n -> [ Regex, RegexName n ]) names
                in
                checkMatches matches patterns ijs props
        ]


suiteDateMatch : Test
suiteDateMatch =
    describe "Date matching"
        [ test "Matches dates with various separators" <|
            \() ->
                let
                    separators =
                        [ "", " ", "-", "/", "\\", "_", "." ]

                    passwords =
                        List.map (\s -> String.join s [ "13", "2", "1921" ]) separators

                    matches =
                        List.concatMap dateMatch passwords

                    ijs =
                        List.map (\p -> ( 0, String.length p - 1 )) passwords

                    props =
                        List.map (\s -> [ Date, Year 1921, Month 2, Day 13, Separator s ]) separators
                in
                checkMatches matches passwords ijs props
        , test "Matches dates with any order" <|
            \() ->
                let
                    ( day, month, year ) =
                        ( "8", "8", "88" )

                    passwords =
                        [ month ++ day ++ year, day ++ month ++ year, year ++ month ++ day, year ++ day ++ month ]

                    matches =
                        List.concatMap dateMatch passwords

                    ijs =
                        List.map (\p -> ( 0, String.length p - 1 )) passwords

                    props =
                        List.repeat 4 [ Date, Year 1988, Month 8, Day 8, Separator "" ]
                in
                checkMatches matches passwords ijs props
        , test "Matches the date with year closest to Internal.Zxcvbn.Sorting.referenceYear when ambiguous" <|
            \() ->
                let
                    password =
                        "111504"

                    matches =
                        dateMatch password

                    ijs =
                        [ ( 0, String.length password - 1 ) ]

                    props =
                        -- picks "04" -> 2004 as year, not "1504"
                        [ [ Date, Year 2004, Month 11, Day 15, Separator "" ] ]
                in
                checkMatches matches [ password ] ijs props
        , test "Matches various dates" <|
            \() ->
                let
                    dates =
                        [ ( 1, 1, 1999 )
                        , ( 11, 8, 2000 )
                        , ( 9, 12, 2005 )
                        , ( 22, 11, 1551 )
                        ]

                    passwords =
                        List.map (\( d, m, y ) -> String.fromInt y ++ String.fromInt m ++ String.fromInt d) dates

                    matches =
                        List.concatMap dateMatch passwords

                    ijs =
                        List.map (\p -> ( 0, String.length p - 1 )) passwords

                    props =
                        List.map (\( _, _, y ) -> [ Date, Year y, Separator "" ]) dates
                in
                checkMatches matches passwords ijs props
        , test "Matches various dates with separator" <|
            \() ->
                let
                    dates =
                        [ ( 1, 1, 1999 )
                        , ( 11, 8, 2000 )
                        , ( 9, 12, 2005 )
                        , ( 22, 11, 1551 )
                        ]

                    passwords =
                        List.map (\( d, m, y ) -> String.join "." [ String.fromInt y, String.fromInt m, String.fromInt d ]) dates

                    matches =
                        List.concatMap dateMatch passwords

                    ijs =
                        List.map (\p -> ( 0, String.length p - 1 )) passwords

                    props =
                        List.map (\( _, _, y ) -> [ Date, Year y, Separator "." ]) dates
                in
                checkMatches matches passwords ijs props
        , test "Matches zero-padded dates" <|
            \() ->
                let
                    password =
                        "02/02/02"

                    matches =
                        dateMatch password

                    ijs =
                        [ ( 0, String.length password - 1 ) ]

                    props =
                        [ [ Date, Year 2002, Month 2, Day 2, Separator "/" ] ]
                in
                checkMatches matches [ password ] ijs props
        , test "Matches embedded dates" <|
            \() ->
                let
                    prefixes =
                        [ "a", "ab" ]

                    suffixes =
                        [ "!" ]

                    pattern =
                        "1/1/91"

                    combinations =
                        genPws pattern prefixes suffixes

                    props =
                        [ [ Date, Year 1991, Month 1, Day 1 ] ]
                in
                Expect.all
                    (List.map
                        (\( p, ( i, j ) ) ->
                            let
                                matches =
                                    dateMatch p
                            in
                            \() -> checkMatches matches [ pattern ] [ ( i, j ) ] props
                        )
                        combinations
                    )
                    ()
        , test "Matches overlapping dates" <|
            \() ->
                let
                    password =
                        "12/20/1991.12.20"

                    matches =
                        dateMatch password

                    ijs =
                        [ ( 0, 9 ), ( 6, 15 ) ]

                    props =
                        [ [ Date, Year 1991, Month 12, Day 20, Separator "/" ]
                        , [ Date, Year 1991, Month 12, Day 20, Separator "." ]
                        ]
                in
                checkMatches matches [ "12/20/1991", "1991.12.20" ] ijs props
        , test "Matches dates padded by non-ambiguous digits" <|
            \() ->
                let
                    matches =
                        dateMatch "912/20/919"

                    ijs =
                        [ ( 1, 8 ) ]

                    props =
                        [ [ Date, Year 1991, Month 12, Day 20, Separator "/" ] ]
                in
                checkMatches matches [ "12/20/91" ] ijs props
        ]


suiteOmnimatch : Test
suiteOmnimatch =
    describe "Omnimatching"
        [ test "Doesn't match empty passwords" <|
            \() ->
                Expect.equal [] <| omnimatch [] ""
        , test "Matches all expected patterns" <|
            let
                password =
                    "r0sebudmaelstrom11/20/91aaaa"

                matches =
                    omnimatch [] password

                tokens =
                    [ "r0sebud", "maelstrom", "11/20/91", "aaaa" ]

                ijs =
                    [ ( 0, 6 ), ( 7, 15 ), ( 16, 23 ), ( 24, 27 ) ]

                types =
                    [ "dictionary", "dictionary", "date", "repeat" ]

                compTypes a b =
                    case ( a, b ) of
                        ( "dictionary", DictionaryMatch _ ) ->
                            True

                        ( "spatial", SpatialMatch _ ) ->
                            True

                        ( "sequence", SequenceMatch _ ) ->
                            True

                        ( "repeat", RepeatMatch _ ) ->
                            True

                        ( "regex", RegexMatch _ ) ->
                            True

                        ( "date", DateMatch _ ) ->
                            True

                        _ ->
                            False

                isInMatches : String -> ( Int, Int ) -> String -> Bool
                isInMatches expTok ( expI, expJ ) expType =
                    List.filter (\{ i, j, token, pattern } -> i == expI && j == expJ && token == expTok && compTypes expType pattern) matches
                        |> List.isEmpty
                        |> not
            in
            \() ->
                List.map3 isInMatches tokens ijs types
                    |> List.all identity
                    |> Expect.true "All matches found."
        , test "Matches user-provided dictionaries" <|
            \() ->
                let
                    matches =
                        omnimatch [ "foo", "bar" ] "foobar"
                            |> List.filter
                                (\m ->
                                    case m.pattern of
                                        DictionaryMatch { dictionaryName } ->
                                            if dictionaryName == "userInputs" then
                                                True

                                            else
                                                False

                                        _ ->
                                            False
                                )

                    patterns =
                        [ "foo", "bar" ]

                    ijs =
                        [ ( 0, 2 ), ( 3, 5 ) ]

                    props =
                        [ [ Dictionary, MatchedWord "foo", Rank 1 ]
                        , [ Dictionary, MatchedWord "bar", Rank 2 ]
                        ]
                in
                checkMatches matches patterns ijs props
        ]
