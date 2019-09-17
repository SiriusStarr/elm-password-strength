module ZxcvbnPlus exposing
    ( zxcvbnPlus
    , ZxcvbnPlusResult, Score(..), Feedback, DisplayCrackTimes, NumericCrackTimes
    )

{-| `PasswordStrength.ZxcvbnPlus` is a native Elm implementation of the popular zxcvbn password strength javascript library. If you require exactly identical results to the reference implementation, please use `PasswordStrength.Zxcvbn` instead. More information may be found on its [GitHub](https://github.com/dropbox/zxcvbn). The following is excerpted from the README:

zxcvbn is a password strength estimator inspired by password crackers. Through pattern matching and conservative estimation, it recognizes and weighs 30k common passwords, common names and surnames according to US census data, popular English words from Wikipedia and US television and movies, and other common patterns like dates, repeats ("aaa"), sequences ("abcd"), keyboard patterns ("qwertyuiop"), and l33t speak.

Consider using zxcvbn as an algorithmic alternative to password composition policy — it is more secure, flexible, and usable when sites require a minimal complexity score in place of annoying rules like "passwords must contain three of {lower, upper, numbers, symbols}".

  - **More secure:** policies often fail both ways, allowing weak passwords ("P@ssword1") and disallowing strong passwords.
  - **More flexible:** zxcvbn allows many password styles to flourish so long as it detects sufficient complexity — passphrases are rated highly given enough uncommon words, keyboard patterns are ranked based on length and number of turns, and capitalization adds more complexity when it's unpredictaBle.
  - **More usable:** zxcvbn is designed to power simple, rule-free interfaces that give instant feedback. In addition to strength estimation, zxcvbn includes minimal, targeted verbal feedback that can help guide users towards less guessable passwords.

For further detail and motivation, please refer to the USENIX Security '16 [paper and presentation](https://www.usenix.org/conference/usenixsecurity16/technical-sessions/presentation/wheeler).


## Usage Notes

The results returned by this package differ somewhat from the reference (javascript) implementation. If you require exactly identical results, consider using `Zxcvbn` instead.

The computational performance of this library is quite good, with essentially instantaneous results for normal length passwords and barely perceptible delays even with ~100-character passwords.

It should be noted that the zxcvbn library is not "light" by any means, given the large dictionaries of common words it requires. Sizes for an extremely simple example application follow:

  - Compiled size -- 3.69 MB
  - Minified size -- 1.85 MB
  - Gzipped size -- 694 KB


## Differences from `Zxcvbn`

In general, this package differs from the reference implementation in being more "Elm"-y. For example, custom types are used in place of strings in return values, regexes are avoided in favor of parsers, etc. Unlike the reference implementation, it also will not erroneously flag invalid dates, e.g. February 31st.


## Known Issues

With _exceedingly_ difficult to crack passwords, the algorithm may report bruteforce matches as "more guessable" than actual matches in the password. This is because the number of possible guesses for bruteforce caps at approximately `10^308` guesses. Since this is truly absurdly large, it should never be problematic in normal use.


## Functions

This package exposes exactly one function, `zxcvbnPlus`, which requires no configuration of any kind and may be simply called with a password (and possibly-empty list of additional patterns).

@docs zxcvbnPlus


## Result Types

The sole function in this package returns one relatively complex record containing the full results of the password strength check.

@docs ZxcvbnPlusResult, Score, Feedback, DisplayCrackTimes, NumericCrackTimes

-}

import Internal.ZxcvbnPlus.Matching exposing (omnimatch)
import Internal.ZxcvbnPlus.Scoring exposing (isAllCaps, isCapitalized, isLastCapitalized, mostGuessableMatchSequence)
import List.Extra as ListX
import ZxcvbnPlus.MatchTypes exposing (DateDetails(..), Dictionary(..), DictionaryDetails(..), MatchDetails(..), ScoredMatch)


{-| `zxcvbnPlus` takes two arguments, a `List String` of user patterns and a `String` password, and returns a result record of type `ZxcvbnResult`.

The user patterns argument (which may be an empty list) is an array of strings that `zxcvbnPlus` will treat as an extra dictionary. This can be whatever list of strings you like but is meant for user inputs from other fields of the form, like name and email. That way a password that includes a user's personal information can be heavily penalized. This list is also good for site-specific vocabulary -- Acme Brick Co. might want to include `["acme", "brick", "acmebrick", etc]`. This list is ordered in increasing rank, which the first element of the list being the highest rank (most common). Entries with higher ranks are more strongly penalized. However, unless a truly massive list is used, the ordering is unlikely to be relevant. User patterns are not case-sensitive.

-}
zxcvbnPlus : List String -> String -> ZxcvbnPlusResult
zxcvbnPlus userPatterns password =
    let
        matches =
            omnimatch userPatterns password

        result =
            mostGuessableMatchSequence False password matches

        ( numericCrackTimes, displayCrackTimes, score ) =
            estimateAttackTimes result.guesses

        feedback =
            getFeedback score result.sequence
    in
    ZxcvbnPlusResult
        score
        feedback
        displayCrackTimes
        numericCrackTimes
        result.guesses
        (logBase 10 <| toFloat result.guesses)
        result.password
        result.sequence


{-| `ZxcvbnPlusResult` is returned by `zxcvbnPlus` and holds the results of the password strength check. It has the following fields:

  - `score` -- The overall score of the password (useful for displaying to the user, e.g. as a strength bar).
  - `feedback` -- A record containing user-friendly feedback to help choose better passwords; is empty if the score is sufficiently high.
  - `crackTimesDisplay` -- A record with user-friendly display strings for the crack time estimations, e.g. "less than a second", "3 hours", "centuries", etc.
  - `crackTimesSeconds` -- A record containing back-of-the-envelope crack time estimations, in seconds, based on a few scenarios.
  - `guesses` -- The estimated guesses needed to crack the password.
  - `guessesLog10` -- The order of magnitude of the estimated number of guesses to crack the password.
  - `password` -- The password that was checked.
  - `sequence` -- The list of patterns that zxcvbn based the guess calculation on. Rather complicated and typically unnecessary, but the contained types are exposed in `ZxcvbnPlus.MatchTypes` if they are needed.

-}
type alias ZxcvbnPlusResult =
    { score : Score
    , feedback : Feedback
    , crackTimesDisplay : DisplayCrackTimes
    , crackTimesSeconds : NumericCrackTimes
    , guesses : Int
    , guessesLog10 : Float
    , password : String
    , matchSequence : List ScoredMatch
    }


{-| The overall score for a password's quality, e.g. for display to the user as a growing bar.

  - `TooGuessable` -- Risky password: "too guessable"
  - `VeryGuessable` -- Modest protection from throttled online attacks: "very guessable"
  - `SomewhatGuessable` -- Modest protection from unthrottled online attacks: "somewhat guessable"
  - `SafellyUnguessable` -- Modest protection from offline attacks: "safely unguessable" assuming a salted, slow hash function like bcrypt, scrypt, PBKDF2, argon, etc
  - `VeryUnguessable` -- Strong protection from offline attacks under same scenario: "very unguessable"

-}
type Score
    = TooGuessable
    | VeryGuessable
    | SomewhatGuessable
    | SafelyUnguessable
    | VeryUnguessable


{-| Contains feedback inteded to be displayed to the user trying to create a password.

  - `warning`: Explains what's wrong, e.g. "This is a top-10 common password". Will be `Nothing` if no applicable warnings are found.
  - `suggestion`: A possibly-empty list of suggestions to help choose a less guessable password, e.g. "Add another word or two".

-}
type alias Feedback =
    { warning : Maybe String
    , suggestions : List String
    }


{-| Crack times for password in seconds for various cases.

  - `onlineThrottling100PerHour` -- Online attack on a service that ratelimits password auth attempts.
  - `onlineNoThrottling10PerSecond` -- Online attack on a service that doesn't ratelimit, or where an attacker has outsmarted ratelimiting.
  - `offlineSlowHashing1e4PerSecond` -- Offline attack. assumes multiple attackers, proper user-unique salting, and a slow hash function w/ moderate work factor, such as bcrypt, scrypt, PBKDF2.
  - `offlineFastHashing1e10PerSecond` -- Offline attack with user-unique salting but a fast hash function like SHA-1, SHA-256 or MD5. A wide range of reasonable numbers anywhere from one billion - one trillion guesses per second, depending on number of cores and machines. Ballparking at 10B/sec.

-}
type alias NumericCrackTimes =
    { onlineThrottling100PerHour : Float
    , onlineNoThrottling10PerSecond : Float
    , offlineSlowHashing1e4PerSecond : Float
    , offlineFastHashing1e10PerSecond : Float
    }


{-| Crack times for password in user-friendly format (e.g. "less than a second", "10 years", etc.) for various cases.

  - `onlineThrottling100PerHour` -- Online attack on a service that ratelimits password auth attempts.
  - `onlineNoThrottling10PerSecond` -- Online attack on a service that doesn't ratelimit, or where an attacker has outsmarted ratelimiting.
  - `offlineSlowHashing1e4PerSecond` -- Offline attack. assumes multiple attackers, proper user-unique salting, and a slow hash function w/ moderate work factor, such as bcrypt, scrypt, PBKDF2.
  - `offlineFastHashing1e10PerSecond` -- Offline attack with user-unique salting but a fast hash function like SHA-1, SHA-256 or MD5. A wide range of reasonable numbers anywhere from one billion - one trillion guesses per second, depending on number of cores and machines. Ballparking at 10B/sec.

-}
type alias DisplayCrackTimes =
    { onlineThrottling100PerHour : String
    , onlineNoThrottling10PerSecond : String
    , offlineSlowHashing1e4PerSecond : String
    , offlineFastHashing1e10PerSecond : String
    }



-- * Non-exposed below here


{-| Takes a `Score` and a list of `ScoredMatch` and returns warnings/suggestions for the user with help on how to improve the password, if it requires it.
-}
getFeedback : Score -> List ScoredMatch -> Feedback
getFeedback score sequence =
    if List.isEmpty sequence then
        -- Starting feedback
        defaultFeedback

    else
        case score of
            -- No feedback if score is good or great.
            VeryUnguessable ->
                Feedback Nothing []

            SafelyUnguessable ->
                Feedback Nothing []

            _ ->
                -- Tie feedback to the longest match for longer sequences
                let
                    longestMatch =
                        ListX.maximumBy (\m -> String.length m.token) sequence

                    { warning, suggestions } =
                        case longestMatch of
                            Nothing ->
                                Feedback Nothing []

                            Just m ->
                                getMatchFeedback (List.length sequence == 1) m
                in
                Feedback
                    warning
                    ("Add another word or two.  Uncommon words are better." :: suggestions)


{-| Feedback provided in the absence of matches.
-}
defaultFeedback : Feedback
defaultFeedback =
    { warning = Nothing
    , suggestions = [ "Use a few words, avoid common phrases.", "No need for symbols, digits, or uppercase letters." ]
    }


{-| Provides feedback tailored to a specific match.
-}
getMatchFeedback : Bool -> ScoredMatch -> Feedback
getMatchFeedback isSoleMatch match =
    case match.pattern of
        DictionaryMatch data ->
            getDictionaryMatchFeedback isSoleMatch data match

        KeyAdjacencyMatch { turns } ->
            let
                warning =
                    if turns == 1 then
                        "Straight rows of keys are easy to guess."

                    else
                        "Short keyboard patterns are easy to guess."
            in
            Feedback
                (Just warning)
                [ "If you must use keyboard patterns, use a longer pattern with more turns." ]

        RepeatMatch { baseToken } ->
            let
                warning =
                    if String.length baseToken == 1 then
                        "Repeats like \"aaa\" are easy to guess."

                    else
                        "Repeats like \"abcabcabc\" are only slightly harder to guess than \"abc\"."
            in
            Feedback
                (Just warning)
                [ "Avoid repeated words and characters." ]

        OrdinalSequenceMatch _ ->
            Feedback
                (Just "Sequences like abc or 6543 are easy to guess.")
                [ "Avoid sequences." ]

        DateMatch { dateDetails } ->
            case dateDetails of
                RecentYear ->
                    Feedback
                        (Just "Recent years are easy to guess.")
                        [ "Avoid recent years.", "Avoid years that are associated with you." ]

                FullDate _ ->
                    Feedback
                        (Just "Dates are often easy to guess.")
                        [ "Avoid dates and years that are associated with you." ]

        BruteforceMatch ->
            Feedback Nothing []


{-| Provide feedback specifically for dictionary matches.
-}
getDictionaryMatchFeedback :
    Bool
    ->
        { dictionaryDetails : DictionaryDetails
        , dictionary : Dictionary
        , rank : Int
        , matchedWord : String
        }
    -> ScoredMatch
    -> Feedback
getDictionaryMatchFeedback isSoleMatch { dictionaryDetails, dictionary, rank } { guesses, token } =
    let
        warning =
            case dictionary of
                CommonPassword ->
                    if isSoleMatch && dictionaryDetails == NormalDictionary then
                        if rank <= 10 then
                            Just "This is a top-10 common password."

                        else if rank <= 100 then
                            Just "This is a top-100 common password."

                        else
                            Just "This is a very common password."

                    else if guesses <= 10000 then
                        Just "This is similar to a commonly used password."

                    else
                        Nothing

                EnglishWord _ ->
                    if isSoleMatch then
                        Just "A word by itself is easy to guess."

                    else
                        Nothing

                Name _ ->
                    if isSoleMatch then
                        Just "Names and surnames by themselves are easy to guess."

                    else
                        Just "Common names and surnames are easy to guess."

                UserPattern ->
                    Just "Words or phrases associated with the site or with your user information are very easy to guess."

        suggestion1 =
            let
                lToken =
                    String.toList token
            in
            if isCapitalized lToken then
                [ "Capitalization doesn't help very much." ]

            else if isLastCapitalized lToken then
                [ "Capitalizing the last letter doesn't help very much. " ]

            else if isAllCaps lToken && String.toLower token /= token then
                [ "All-uppercase is almost as easy to guess as all-lowercase." ]

            else
                []

        suggestion2 =
            case dictionaryDetails of
                ReverseDictionary ->
                    if String.length token >= 4 then
                        [ "Reversed words aren't much harder to guess." ]

                    else
                        []

                L33tDictionary _ ->
                    [ "Predictable substitutions like '@' instead of 'a' don't help very much." ]

                _ ->
                    []
    in
    Feedback warning <| suggestion1 ++ suggestion2


{-| Given an estimated number of guesses for a password, determine crack times and a score.
-}
estimateAttackTimes : Int -> ( NumericCrackTimes, DisplayCrackTimes, Score )
estimateAttackTimes guesses =
    let
        fGuesses =
            toFloat guesses

        numericCrackTimes =
            NumericCrackTimes
                (fGuesses / 0.027777778)
                (fGuesses / 10)
                (fGuesses / 10000)
                (fGuesses / 10000000000)

        displayCrackTimes =
            DisplayCrackTimes
                (displayTime numericCrackTimes.onlineThrottling100PerHour)
                (displayTime numericCrackTimes.onlineNoThrottling10PerSecond)
                (displayTime numericCrackTimes.offlineSlowHashing1e4PerSecond)
                (displayTime numericCrackTimes.offlineFastHashing1e10PerSecond)
    in
    ( numericCrackTimes, displayCrackTimes, guessesToScore guesses )


{-| Map number of guesses to a `Score`.
-}
guessesToScore : Int -> Score
guessesToScore guesses =
    let
        delta =
            5
    in
    if guesses < 1000 + delta then
        TooGuessable

    else if guesses < 1000000 + delta then
        VeryGuessable

    else if guesses < 100000000 + delta then
        SomewhatGuessable

    else if guesses < 10000000000 + delta then
        SafelyUnguessable

    else
        VeryUnguessable


{-| Convert a time in seconds into a human-readable time, e.g. "less than a second" or "10 years".
-}
displayTime : Float -> String
displayTime seconds =
    let
        ( num, base ) =
            if seconds < 60 then
                ( round seconds, "second" )

            else if seconds < 3600 then
                ( round <| seconds / 60, "minute" )

            else if seconds < 86400 then
                ( round <| seconds / 3600, "hour" )

            else if seconds < 2592000 then
                ( round <| seconds / 86400, "day" )

            else if seconds < 31556952 then
                ( round <| seconds / 2592000, "month" )

            else
                ( round <| seconds / 31556952, "year" )
    in
    if seconds < 1 then
        "less than a second"

    else if seconds >= 3214080000 then
        "centuries"

    else if num /= 1 then
        String.fromInt num ++ " " ++ base ++ "s"

    else
        "1 " ++ base
