module Internal.Rumkin exposing (CommonPasswordList(..), FrequencyList(..), getCharsetSize)

{-| Contains what is needed for `TestRumkin` but shouldn't be exposed.
-}

import Array exposing (Array)
import Set exposing (Set)


{-| Opaque type to ensure frequency lists only come from the parser.
-}
type FrequencyList
    = FrequencyList (Array Float)


{-| Opaque type to ensure common password lists only come from the parser.
-}
type CommonPasswordList
    = CommonPasswordList (Set String)


{-| Given a password, return its sequence space (not unicode aware).
-}
getCharsetSize : String -> Int
getCharsetSize password =
    String.foldl updateSeqRecord (SequenceRecord False False False False False False False 0) password
        |> .seqSpace


{-| Given a character, updated sequence space as necessary.
-}
updateSeqRecord : Char -> SequenceRecord -> SequenceRecord
updateSeqRecord c r =
    let
        code =
            Char.toCode c
    in
    if not r.lowercaseLatin && code >= 97 && code <= 122 then
        -- * a: 97, z: 122
        { r | lowercaseLatin = True, seqSpace = r.seqSpace + 26 }

    else if not r.uppercaseLatin && code >= 65 && code <= 90 then
        -- * A: 65, Z: 90
        { r | uppercaseLatin = True, seqSpace = r.seqSpace + 26 }

    else if not r.digits && code >= 48 && code <= 57 then
        -- * 0: 48, 9: 57
        { r | digits = True, seqSpace = r.seqSpace + 10 }

    else if not r.shiftedNumberKeys && Set.member c shiftedNumberKeys then
        { r | shiftedNumberKeys = True, seqSpace = r.seqSpace + 10 }

    else if not r.keyboardPunctuation && Set.member c keyboardPunctuation then
        -- ! The sequence space in the source code is wrong (20), but the website has the correct 22
        { r | keyboardPunctuation = True, seqSpace = r.seqSpace + 22 }

    else if not r.space && c == ' ' then
        { r | space = True, seqSpace = r.seqSpace + 1 }

    else if not r.miscAscii && (code < 32 || code > 126) then
        -- * ' ': 32, '~': 126
        { r | miscAscii = True, seqSpace = r.seqSpace + 160 }

    else
        r


{-| Record for tracking sequence space through a string fold.
-}
type alias SequenceRecord =
    { lowercaseLatin : Bool -- * a-z
    , uppercaseLatin : Bool -- * A-Z
    , digits : Bool -- * 0-9
    , shiftedNumberKeys : Bool -- * !@#$%^&*()
    , keyboardPunctuation : Bool -- * `~-_=+[{]}\\|;:'\",<.>/?
    , space : Bool -- * ' '
    , miscAscii : Bool -- * All other ascii
    , seqSpace : Int
    }


shiftedNumberKeys : Set Char
shiftedNumberKeys =
    Set.fromList [ '!', '@', '#', '$', '%', '^', '&', '*', '(', ')' ]


keyboardPunctuation : Set Char
keyboardPunctuation =
    Set.fromList [ '`', '~', '-', '_', '=', '+', '[', '{', ']', '}', '\\', '|', ';', ':', '\'', '"', ',', '<', '.', '>', '/', '?' ]
