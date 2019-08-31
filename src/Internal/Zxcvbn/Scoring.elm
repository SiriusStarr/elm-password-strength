module Internal.Zxcvbn.Scoring exposing (dateGuesses, dictionaryGuesses, estimateGuesses, isAllCaps, isCapitalized, isLastCapitalized, l33tVariations, minYearSpace, mostGuessableMatchSequence, mostGuessableMatchSequenceHelper, nCk, referenceYear, regexGuesses, repeatGuesses, sequenceGuesses, spatialGuesses, uppercaseVariations)

import Array exposing (Array)
import Basics.Extra as BasicsX
import Dict exposing (Dict)
import Dict.Extra as DictX
import Internal.Zxcvbn.AdjacencyGraphs exposing (keyboardAverageDegree, keyboardStartingPositions, keypadAverageDegree, keypadStartingPositions)
import Internal.Zxcvbn.MatchTypes exposing (Match, TranslationDict, sortMatches)
import List.Extra as ListX
import Zxcvbn.MatchTypes exposing (DictionaryDetails(..), MatchDetails(..), ScoredMatch)


{-| Minimum guesses to crack a year, regardless of date.
-}
minYearSpace : Int
minYearSpace =
    20


{-| Year relative to which to score date matches. Should be the current year.
-}
referenceYear : Int
referenceYear =
    -- ! A new version will need to be pushed every year, since elm cannot get the current year.
    2019


{-| n choose k
-}
nCk : Int -> Int -> Int
nCk n k =
    List.range 1 (min k <| abs (n - k))
        |> List.foldl (\i acc -> acc * (toFloat (n + 1 - i) / toFloat i)) 1
        |> round


{-| Given match token and sequence-specific data, estimate number of guesses to crack a sequence match.
-}
sequenceGuesses : String -> { a | ascending : Bool } -> Int
sequenceGuesses token { ascending } =
    let
        baseGuesses =
            case String.uncons token of
                Just ( firstChar, _ ) ->
                    if List.member firstChar [ 'a', 'A', 'z', 'Z', '0', '1', '9' ] then
                        -- Lower guesses for obvious starting points
                        4

                    else if Char.isDigit firstChar then
                        10

                    else
                        -- Could give a higher base for uppercase, assigning 26 to both upper and lower sequences is more conservative.
                        26

                _ ->
                    26
    in
    if ascending then
        baseGuesses * String.length token

    else
        -- Need to try a descending sequence in addition to every ascending sequence -> 2x guesses
        2 * baseGuesses * String.length token


{-| Given match token, estimate number of guesses to crack a regex match.
-}
regexGuesses : String -> Int
regexGuesses token =
    -- ! This simply isn't used, since the only regex is `recentYear`
    -- let
    -- charClassBases =
    --     Dict.fromList
    --         [ ( "alphaLower", 26 )
    --         , ( "alphaUpper", 26 )
    --         , ( "alpha", 52 )
    --         , ( "alphanumeric", 62 )
    --         , ( "digits", 10 )
    --         , ( "symbols", 33 )
    --         ]
    -- len =
    --     String.length token
    -- in
    -- case Dict.get regexName charClassBases of
    --     Just i ->
    --         i ^ len
    --     Nothing ->
    -- if regexName == "recentYear" then
    -- Conservative estimate of year space: num years from referenceYear.  If year is close to referenceYear, estimate a year space of minYearSpace.
    String.toInt token
        |> Maybe.map (\i -> abs (i - referenceYear))
        |> Maybe.map (max minYearSpace)
        |> Maybe.withDefault 1


{-| Given date-specific match data, estimate guesses to crack a date match.
-}
dateGuesses : { a | separator : String, year : Int } -> Int
dateGuesses { separator, year } =
    -- Base guesses: (year distance from referenceYear) * numDays * numYears
    let
        yearSpace =
            max minYearSpace <| abs (year - referenceYear)
    in
    if String.isEmpty separator then
        yearSpace * 365

    else
        -- Add factor of 4 for separator selection (one of ~4 choices)
        yearSpace * 1460


{-| Given a match token and spatial-specific match data, estimate guesses to crack a spatial match.
-}
spatialGuesses :
    String
    -> { a | graph : String, turns : Int, shiftedCount : Int }
    -> Int
spatialGuesses token { graph, turns, shiftedCount } =
    let
        ( s, d ) =
            if graph == "qwerty" || graph == "dvorak" then
                ( keyboardStartingPositions, keyboardAverageDegree )

            else
                ( keypadStartingPositions, keypadAverageDegree )

        l =
            String.length token
    in
    -- Estimate the number of possible patterns w/ length l or less with possibleTurns turns or less.
    List.range 2 l
        |> List.map
            (\i ->
                let
                    possibleTurns =
                        min turns (i - 1)
                in
                List.range 1 possibleTurns
                    |> List.foldl
                        (\j acc -> acc + (toFloat (nCk (i - 1) (j - 1)) * s * d ^ toFloat j))
                        0
            )
        |> List.sum
        -- Add extra guesses for shifted keys. (% instead of 5, A instead of a.).  Math is similar to extra guesses of l33t substitutions in dictionary matches.
        |> (\i ->
                if shiftedCount == l then
                    -- Double guesses for all caps
                    round <| i * 2

                else
                    let
                        shiftedVariations =
                            calcSubVariations shiftedCount (l - shiftedCount)
                                |> toFloat
                    in
                    round <| i * shiftedVariations
           )


{-| Given a match token and dictionary-specific match data, estimate guesses to crack a dictionary match.
-}
dictionaryGuesses : String -> { a | rank : Int, dictionaryDetails : DictionaryDetails } -> Int
dictionaryGuesses token { rank, dictionaryDetails } =
    let
        baseGuesses =
            rank * uppercaseVariations token
    in
    case dictionaryDetails of
        ReverseDictionary ->
            2 * baseGuesses

        L33tDictionary { sub } ->
            baseGuesses * l33tVariations sub token

        Dictionary ->
            baseGuesses


{-| Calculate number of variations of "l33t" substitution there are in a string, given a `TranslationDict` and that word.
-}
l33tVariations : TranslationDict -> String -> Int
l33tVariations subDict word =
    let
        -- Lower-case match.token before calculating: capitalization shouldn't affect l33t calc.
        token =
            String.toLower word

        accumSubstitutionVariations subbed unsubbed variations =
            let
                -- Num of subbed/unsubbed chars
                ( s, u ) =
                    countSubUnsub subbed unsubbed token
            in
            if s == 0 || u == 0 then
                -- For this sub, password is either fully subbed (444) or fully unsubbed (aaa); treat that as doubling the space (attacker needs to try fully subbed chars in addition to unsubbed.)
                2 * variations

            else
                -- This case is similar to capitalization: with aa44a, u = 3, s = 2, attacker needs to try unsubbed + one sub + two subs.
                variations * calcSubVariations u s
    in
    Dict.foldl accumSubstitutionVariations 1 subDict


{-| Determine if a list of characters is all capitals.
-}
isAllCaps : List Char -> Bool
isAllCaps token =
    not <| List.any Char.isLower token


{-| Determine if a list of characters begins with a capital letter and has no other capitals.
-}
isCapitalized : List Char -> Bool
isCapitalized token =
    case ListX.uncons token of
        Just ( head, tail ) ->
            Char.isUpper head && not (List.any Char.isUpper tail)

        Nothing ->
            False


{-| Determine if a list of characters ends with a capital letter and has no other capitals.
-}
isLastCapitalized : List Char -> Bool
isLastCapitalized token =
    case ListX.unconsLast token of
        Just ( last, init ) ->
            Char.isUpper last && not (List.any Char.isUpper init)

        Nothing ->
            False


{-| Calculate number of variations of uppercase substitution there are in a string.
-}
uppercaseVariations : String -> Int
uppercaseVariations token =
    let
        lToken =
            String.toList token
    in
    if not <| List.any Char.isUpper lToken then
        1

    else if isAllCaps lToken || isCapitalized lToken || isLastCapitalized lToken then
        -- A capitalized word is the most common capitalization scheme, so it only doubles the search space (uncapitalized + capitalized).  Allcaps and end-capitalized are common enough too, underestimate as 2x factor to be safe.
        2

    else
        --Otherwise calculate the number of ways to capitalize U+L uppercase+lowercase letters with U uppercase letters or less.  Or, if there's more uppercase than lower (for e.g. PASSwORD), the number of ways to lowercase U+L letters with L lowercase letters or less.
        let
            ( u, l ) =
                countUpperLower token
        in
        calcSubVariations u l


{-| Given repeat-specific match data, estimate guesses to crack a repeat match.
-}
repeatGuesses : { a | baseGuesses : Int, repeatCount : Int } -> Int
repeatGuesses { baseGuesses, repeatCount } =
    baseGuesses * repeatCount


{-| Estimate number of guesses needed to crack a match.
-}
estimateGuesses : { a | pattern : MatchDetails, token : String } -> String -> Int
estimateGuesses match password =
    let
        len =
            String.length match.token

        minGuesses =
            if len < String.length password then
                if len == 1 then
                    minSubmatchGuessesSingleChar

                else
                    minSubmatchGuessesMultiChar

            else
                1

        guesses =
            case match.pattern of
                DictionaryMatch r ->
                    dictionaryGuesses match.token r

                SpatialMatch r ->
                    spatialGuesses match.token r

                SequenceMatch r ->
                    sequenceGuesses match.token r

                RepeatMatch r ->
                    repeatGuesses r

                RegexMatch _ ->
                    regexGuesses match.token

                DateMatch r ->
                    dateGuesses r

                BruteforceMatch ->
                    bruteforceGuesses match.token
    in
    max guesses minGuesses


{-| Given a token, estimate guesses to crack it by bruteforce.
-}
bruteforceGuesses : String -> Int
bruteforceGuesses token =
    let
        len =
            String.length token

        guesses =
            min BasicsX.maxSafeInteger <| bruteforceCardinality ^ len

        minGuesses =
            -- Small detail: make bruteforce matches at minimum one guess bigger than smallest allowed submatch guesses, such that non-bruteforce submatches over the same [i..j] take precedence.
            if len == 1 then
                minSubmatchGuessesSingleChar + 1

            else
                minSubmatchGuessesMultiChar + 1
    in
    max guesses minGuesses


{-| Takes a sequence of overlapping matches, returns the non-overlapping sequence with minimum guesses. The optimal "minimum guesses" sequence is here defined to be the sequence that minimizes the following function:

       g = l! * Product(m.guesses for m in sequence) + D^(l - 1)

where l is the length of the sequence. The factorial term is the number of ways to order l patterns. The D^(l-1) term is another length penalty, roughly capturing the idea that an attacker will try lower-length sequences first before trying length-l sequences.

For example, consider a sequence that is date-repeat-dictionary.

    - An attacker would need to try other date-repeat-dictionary combinations, hence the product term.
    - An attacker would need to try repeat-date-dictionary, dictionary-repeat-date,..., hence the factorial term.
    - An attacker would also likely try length-1 (dictionary) and length-2 (dictionary-date) sequences before length-3. assuming at minimum D guesses per pattern type, D^(l-1) approximates Sum(D^i for i in [1..l-1].

-}
mostGuessableMatchSequence : Bool -> String -> List Match -> ScoredPassword
mostGuessableMatchSequence excludeAdditive password matches =
    List.map (scoreMatch password) matches
        |> mostGuessableMatchSequenceHelper excludeAdditive password


{-| Actually calculate most guessable sequence, as wrapper simply scores matches and passes them to this.
-}
mostGuessableMatchSequenceHelper : Bool -> String -> List ScoredMatch -> ScoredPassword
mostGuessableMatchSequenceHelper excludeAdditive password matches =
    let
        filterInferiorMatches mByJ =
            -- If two matches have the same i & j, only keep the one with fewer guesses
            ListX.gatherEqualsBy .i mByJ
                |> List.map (\( head, tail ) -> Maybe.withDefault head (ListX.minimumBy .guesses (head :: tail)))
                -- Small detail: for deterministic output, sort each sublist by i.
                |> sortMatches

        matchesByJ =
            -- Partition matches into sublists according to ending index j, keeping only best matches
            ListX.gatherEqualsBy .j matches
                |> List.map (\( head, tail ) -> ( head.j, filterInferiorMatches (head :: tail) ))
                |> Dict.fromList

        n =
            String.length password

        initOptimal : OptimalRecord
        initOptimal =
            { m = Array.repeat n Dict.empty
            , pi = Array.repeat n Dict.empty
            , g = Array.repeat n Dict.empty
            }

        updateWithMatchesAtK : ScoredMatch -> OptimalRecord -> OptimalRecord
        updateWithMatchesAtK m opt =
            if m.i > 0 then
                let
                    matchEnds =
                        Array.get (m.i - 1) opt.m
                            |> Maybe.withDefault Dict.empty
                in
                Dict.foldl
                    (\l _ o ->
                        update excludeAdditive m (l + 1) o
                    )
                    opt
                    matchEnds

            else
                update excludeAdditive m 1 opt

        buildBestMatchSequence : Int -> OptimalRecord -> OptimalRecord
        buildBestMatchSequence k optimal =
            let
                matchesAtK =
                    Dict.get k matchesByJ
                        |> Maybe.withDefault []
            in
            List.foldl updateWithMatchesAtK optimal matchesAtK
                |> bruteforceUpdate excludeAdditive password k
    in
    List.range 0 (n - 1)
        |> List.foldl buildBestMatchSequence initOptimal
        |> unwind n
        |> (\( optimalMatchSequence, guesses ) ->
                { password = password
                , guesses = guesses
                , guessesLog10 = logBase 10 <| toFloat guesses
                , sequence = optimalMatchSequence
                }
           )



-- * Non-exposed below here


{-| Recored returned after scoring a password, including estimated number of guesses to crack and the most guessable sequence of matches.
-}
type alias ScoredPassword =
    { password : String
    , guesses : Int
    , guessesLog10 : Float
    , sequence : List ScoredMatch
    }


{-| `factorial 5` is equivalent to 5!
-}
factorial : Int -> Int
factorial n =
    List.product <| List.range 1 n


{-| Count subbed and unsubbed chars (respectively) in a string.
-}
countSubUnsub : Char -> Char -> String -> ( Int, Int )
countSubUnsub subbed unsubbed word =
    String.foldl
        (\c ( s, u ) ->
            if c == subbed then
                ( s + 1, u )

            else if c == unsubbed then
                ( s, u + 1 )

            else
                ( s, u )
        )
        ( 0, 0 )
        word


{-| Count uppercase and lowercase chars (respectively) in a string.
-}
countUpperLower : String -> ( Int, Int )
countUpperLower word =
    String.foldl
        (\c ( u, l ) ->
            if Char.isUpper c then
                ( u + 1, l )

            else if Char.isLower c then
                ( u, l + 1 )

            else
                ( u, l )
        )
        ( 0, 0 )
        word


{-| Calculate number of variations introduced by having `a` substitutions and `b` unsubbed positions out of `a + b` total possible positions.
-}
calcSubVariations : Int -> Int -> Int
calcSubVariations a b =
    List.range 1 (min a b)
        |> List.foldl (\i acc -> acc + nCk (a + b) i) 0
        |> max 1


{-| "Space" of bruteforce matches, e.g `bruteforceCardinality ^ length`
-}
bruteforceCardinality : Int
bruteforceCardinality =
    10


{-| Minimum guesses at a sequence length before longer sequences will be tried for estimating most guessable sequence.
-}
minGuessesBeforeGrowingSequence : Int
minGuessesBeforeGrowingSequence =
    10000


{-| Minimum guesses to crack a single character.
-}
minSubmatchGuessesSingleChar : Int
minSubmatchGuessesSingleChar =
    10


{-| Minimum guesses to crack 2 or more characters within a match.
-}
minSubmatchGuessesMultiChar : Int
minSubmatchGuessesMultiChar =
    50


{-| Score a match.
-}
scoreMatch : String -> Match -> ScoredMatch
scoreMatch password m =
    let
        guesses =
            estimateGuesses m password
    in
    { pattern = m.pattern
    , token = m.token
    , i = m.i
    , j = m.j
    , guesses = guesses
    , guessesLog10 = logBase 10 <| toFloat guesses
    }


{-| "State" record passed around between various most guessable seqence buildng functions, keeping track of progress.

  - `m` -- m[k][l] holds final match in the best length-l match sequence covering the password prefix up to k, inclusive. If there is no length-l sequence that scores better (fewer guesses) than a shorter match sequence spanning the same prefix, optimal.m[k][l] does not exist.
  - `pi` -- Same structure as `m`; holds the product term Prod(m.guesses for m in sequence). Allows for fast (non-looping) updates to the minimization function.
  - `g` -- Same structure as `m` -- holds the overall metric (guesses).

-}
type alias OptimalRecord =
    { m : Array (Dict Int ScoredMatch)
    , pi : Array (Dict Int Int)
    , g : Array (Dict Int Int)
    }


{-| Make bruteforce match object spanning i to j, inclusive.
-}
makeBruteforceMatch : String -> ( Int, Int ) -> ScoredMatch
makeBruteforceMatch password ( i, j ) =
    let
        token =
            String.slice i (j + 1) password
    in
    { pattern = BruteforceMatch
    , token = token
    , i = i
    , j = j
    }
        |> scoreMatch password


{-| Evaluate bruteforce matches ending at k.
-}
bruteforceUpdate : Bool -> String -> Int -> OptimalRecord -> OptimalRecord
bruteforceUpdate excludeAdditive password k inputOptimal =
    let
        -- See if a single bruteforce match spanning the k-prefix is optimal.
        initOptimal =
            update excludeAdditive (makeBruteforceMatch password ( 0, k )) 1 inputOptimal

        tryBruteforceFrom : Int -> OptimalRecord -> OptimalRecord
        tryBruteforceFrom i optimal =
            -- Generate k bruteforce matches, spanning from (i=1, j=k) up to (i=k, j=k).  See if adding these new matches to any of the sequences in optimal[i-1] leads to new bests.
            let
                bruteforceMatch =
                    makeBruteforceMatch password ( i, k )

                prevMatches =
                    Array.get (i - 1) optimal.m
                        |> Maybe.withDefault Dict.empty

                tryBruteforceAt : Int -> ScoredMatch -> OptimalRecord -> OptimalRecord
                tryBruteforceAt l lastMatch opt =
                    -- Update with the generated bruteforce at the match in question.
                    case lastMatch.pattern of
                        BruteforceMatch ->
                            -- Corner: an optimal sequence will never have two adjacent bruteforce matches.  It is strictly better to have a single bruteforce match spanning the same region: Same contribution to the guess product with a lower length. Thus, safe to skip those cases.
                            opt

                        _ ->
                            -- Try adding m to this length-l sequence.
                            update excludeAdditive bruteforceMatch (l + 1) opt
            in
            Dict.foldl tryBruteforceAt optimal prevMatches
    in
    List.foldl tryBruteforceFrom initOptimal <| List.range 1 k


{-| Considers whether a length-l sequence ending at match m is better (fewer guesses) than previously encountered sequences, updating state if so.
-}
update : Bool -> ScoredMatch -> Int -> OptimalRecord -> OptimalRecord
update excludeAdditive m l optimal =
    let
        -- * Throughout, `m.j` is simply `k`
        pi =
            -- We're considering a length-l sequence ending with match m.  Obtain the product term in the minimization function by multiplying m's guesses by the product of the length-(l-1) sequence ending just before m, at m.i - 1.
            let
                lastProduct =
                    Array.get (m.i - 1) optimal.pi
                        |> Maybe.andThen (Dict.get (l - 1))
                        -- If l == 1 so no products, multiply by 1
                        |> Maybe.withDefault 1
            in
            m.guesses * lastProduct

        g =
            if excludeAdditive then
                factorial l * pi

            else
                factorial l * pi + minGuessesBeforeGrowingSequence ^ (l - 1)

        optimalGuessesAtK =
            Array.get m.j optimal.g
                |> Maybe.withDefault Dict.empty

        betterSequence competingL competingG =
            competingL <= l && competingG <= g
    in
    -- Update state if new best.  First see if any competing sequences covering this prefix, with l or fewer matches, fare better than this sequence.  If so, skip it and return.
    if DictX.any betterSequence optimalGuessesAtK then
        -- Beter sequence already found
        optimal

    else
        -- This sequence might be part of the final optimal sequence.
        let
            newG =
                Dict.insert l g optimalGuessesAtK

            newM =
                Dict.insert l m <| Maybe.withDefault Dict.empty <| Array.get m.j optimal.m

            newPi =
                Dict.insert l pi <| Maybe.withDefault Dict.empty <| Array.get m.j optimal.pi
        in
        { g = Array.set m.j newG optimal.g
        , m = Array.set m.j newM optimal.m
        , pi = Array.set m.j newPi optimal.pi
        }


{-| Steps backwards through optimal.m starting at the end, constructing the final optimal match sequence.
-}
unwind : Int -> OptimalRecord -> ( List ScoredMatch, Int )
unwind n optimal =
    let
        k =
            n - 1

        bestLG =
            -- Find the final best sequence length and score
            Array.get k optimal.g
                |> Maybe.withDefault Dict.empty
                |> Dict.foldl
                    (\candidateL candidateG acc ->
                        case acc of
                            Nothing ->
                                Just ( candidateL, candidateG )

                            Just ( _, g ) ->
                                if candidateG < g then
                                    Just ( candidateL, candidateG )

                                else
                                    acc
                    )
                    Nothing
    in
    case bestLG of
        Nothing ->
            -- Corner: empty password
            ( [], 1 )

        Just ( l, g ) ->
            let
                ( _, _, optimalMatchSequence ) =
                    Array.foldr
                        (\seqDict ( ( currK, currL ), nextK, acc ) ->
                            if currK == nextK then
                                let
                                    m =
                                        Dict.get currL seqDict
                                            |> Maybe.withDefault
                                                { i = -1
                                                , j = -1
                                                , token = ""
                                                , pattern = BruteforceMatch
                                                , guesses = -1
                                                , guessesLog10 = -1
                                                }
                                in
                                ( ( currK - 1, currL - 1 ), m.i - 1, m :: acc )

                            else
                                ( ( currK - 1, currL ), nextK, acc )
                        )
                        ( ( k, l ), k, [] )
                        optimal.m
            in
            ( optimalMatchSequence, g )
