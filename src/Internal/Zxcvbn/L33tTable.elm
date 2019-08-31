module Internal.Zxcvbn.L33tTable exposing (SubstitutionDict, l33tTable)

{-| This l33t substitution table is imported from <https://github.com/dropbox/zxcvbn/blob/master/src/matching.coffee>
-}

import Dict exposing (Dict)


{-| General type for a l33t substitution dictionary for legibility in type annotations.
-}
type alias SubstitutionDict =
    Dict Char (List Char)


{-| Dictionary of possible "l33t" character substitutions with characters for keys and lists of characters for values.
-}
l33tTable : SubstitutionDict
l33tTable =
    Dict.fromList
        [ ( 'a', [ '4', '@' ] )
        , ( 'b', [ '8' ] )
        , ( 'c', [ '(', '{', '[', '<' ] )
        , ( 'e', [ '3' ] )
        , ( 'g', [ '6', '9' ] )
        , ( 'i', [ '1', '!', '|' ] )
        , ( 'l', [ '1', '|', '7' ] )
        , ( 'o', [ '0' ] )
        , ( 's', [ '$', '5' ] )
        , ( 't', [ '+', '7' ] )
        , ( 'x', [ '%' ] )
        , ( 'z', [ '2' ] )
        ]