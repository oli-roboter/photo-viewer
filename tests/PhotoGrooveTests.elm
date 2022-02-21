module PhotoGrooveTests exposing (..)

import Expect exposing (Expectation)
import Json.Decode exposing (decodeString)
import PhotoGroove
import Fuzz exposing (Fuzzer, int, list, string)
import Test exposing (..)


decoderTest : Test
decoderTest =
    test "title defaults to (untitled)" <|
        \_ -> 
            -- "{\"url\": \"fruits.com\", \"size\": 5}"
            """{"url": "fruits.com", "size": 5}""" --triple quote notation is the same as above
                |> decodeString PhotoGroove.photoDecoder
                |> Result.map (\photo -> photo.title)
                |> Expect.equal (Ok "(untitled)")
        