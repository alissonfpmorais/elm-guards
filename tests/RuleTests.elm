module RuleTests exposing (suite)

import Expect
import Rule
import Test exposing (Test, describe, test)


suite : Test
suite =
    describe "Testing rules"
        [ intSuite
        , floatSuite
        , stringSuite
        , listSuite
        ]


intSuite : Test
intSuite =
    describe "Integer rules"
        [ test "Odd number against Rule.even must be false" <|
            \_ ->
                Expect.equal False (Rule.even 1)
        , test "Even number against Rule.even must be true" <|
            \_ ->
                Expect.equal True (Rule.even -2)
        , test "Odd number against Rule.odd must be true" <|
            \_ ->
                Expect.equal True (Rule.odd -1)
        , test "Even number against Rule.odd must be false" <|
            \_ ->
                Expect.equal False (Rule.odd 2)
        ]


floatSuite : Test
floatSuite =
    describe "Float rules"
        [ test "Numbers greater than 0 against Rule.positive must be true" <|
            \_ ->
                Expect.equal True (Rule.positive 1)
        , test "0 against Rule.positive must be false" <|
            \_ ->
                Expect.equal False (Rule.positive 0)
        , test "Numbers lower than 0 against Rule.positive must be false" <|
            \_ ->
                Expect.equal False (Rule.positive -1)
        , test "Numbers greater than 0 against Rule.negative must be false" <|
            \_ ->
                Expect.equal False (Rule.negative 1)
        , test "0 against Rule.negative must be false" <|
            \_ ->
                Expect.equal False (Rule.negative 0)
        , test "Numbers lower than 0 against Rule.negative must be true" <|
            \_ ->
                Expect.equal True (Rule.negative -1)
        , test "Numbers greater than 0 against Rule.equal must be false" <|
            \_ ->
                Expect.equal False (Rule.equal 0 1)
        , test "Number equal to 0 against Rule.equal must be true" <|
            \_ ->
                Expect.equal True (Rule.equal 0 0)
        , test "Numbers lower than 0 against Rule.equal must be false" <|
            \_ ->
                Expect.equal False (Rule.equal 0 -1)
        , test "Numbers greater than 0 against Rule.different must be true" <|
            \_ ->
                Expect.equal True (Rule.different 0 1)
        , test "Number equal to 0 against Rule.different must be false" <|
            \_ ->
                Expect.equal False (Rule.different 0 0)
        , test "Numbers lower than 0 against Rule.different must be true" <|
            \_ ->
                Expect.equal True (Rule.different 0 -1)
        , test "Numbers greater than 0 against Rule.greaterThan must be true" <|
            \_ ->
                Expect.equal True (Rule.greaterThan 0 1)
        , test "Number equal to 0 against Rule.greaterThan must be false" <|
            \_ ->
                Expect.equal False (Rule.greaterThan 0 0)
        , test "Numbers lower than 0 against Rule.greaterThan must be false" <|
            \_ ->
                Expect.equal False (Rule.greaterThan 0 -1)
        , test "Numbers greater than 0 against Rule.lowerThan must be false" <|
            \_ ->
                Expect.equal False (Rule.lowerThan 0 1)
        , test "Number equal to 0 against Rule.lowerThan must be false" <|
            \_ ->
                Expect.equal False (Rule.lowerThan 0 0)
        , test "Numbers lower than 0 against Rule.lowerThan must be true" <|
            \_ ->
                Expect.equal True (Rule.lowerThan 0 -1)
        ]


stringSuite : Test
stringSuite =
    describe "String rules"
        [ test "String containing 'rule' against Rule.contains must be true" <|
            \_ ->
                let
                    pattern =
                        "rule"

                    sentence =
                        "One Ring to rule them all, One Ring to find them, One Ring to bring them all, and in the darkness bind them."
                in
                Expect.equal True (Rule.contains pattern sentence)
        , test "String not containing 'rule' against Rule.contains must be false" <|
            \_ ->
                let
                    pattern =
                        "rule"

                    sentence =
                        "It is a curious thing, Harry, but perhaps those who are best suited to power are those who never sought it."
                in
                Expect.equal False (Rule.contains pattern sentence)
        , test "String matching '([a-zA-Z ])+ padawan\\.' against Rule.match must be true" <|
            \_ ->
                let
                    pattern =
                        "([a-zA-Z ])+ padawan\\."

                    sentence =
                        "PATIENCE YOU MUST HAVE my young padawan."
                in
                Expect.equal True (Rule.match pattern sentence)
        , test "String not matching 'DeLorean' against Rule.match must be false" <|
            \_ ->
                let
                    pattern =
                        "DeLorean"

                    sentence =
                        "Wait a minute, Doc. Ah... Are you telling me that you built a time machine... out of a DeLorean?"
                in
                Expect.equal False (Rule.match pattern sentence)
        , test "Empty string against Rule.exactly 0 must be true" <|
            \_ ->
                Expect.equal True (Rule.exactly 0 "")
        , test "String 'Paint the fence.  Up…down.  Up!  Down!' against Rule.exactly 38 must be true" <|
            \_ ->
                let
                    sentence =
                        "Paint the fence.  Up…down.  Up!  Down!"
                in
                Expect.equal True (Rule.exactly 38 sentence)
        , test "Empty string against Rule.exactly 1 must be false" <|
            \_ ->
                Expect.equal False (Rule.exactly 1 "")
        , test "String 'Ohh, what's really going to bake your noodle later on is, would you still have broken it if I hadn't said anything?' against Rule.exactly 0 must be false" <|
            \_ ->
                let
                    sentence =
                        "Ohh, what's really going to bake your noodle later on is, would you still have broken it if I hadn't said anything?"
                in
                Expect.equal False (Rule.exactly 0 sentence)
        , test "Empty string against Rule.longerThan -1 must be true" <|
            \_ ->
                Expect.equal True (Rule.longerThan -1 "")
        , test "Empty string against Rule.longerThan 0 must be false" <|
            \_ ->
                Expect.equal False (Rule.longerThan 0 "")
        , test "Empty string against Rule.longerThan 1 must be false" <|
            \_ ->
                Expect.equal False (Rule.longerThan 1 "")
        , test "String longer than 0 chars against Rule.longerThan 0 must be true" <|
            \_ ->
                let
                    sentence =
                        "Hasta la vista, baby!"
                in
                Expect.equal True (Rule.longerThan 0 sentence)
        , test "String with 15 chars against Rule.longerThan 15 must be false" <|
            \_ ->
                let
                    sentence =
                        "Why so serious?"
                in
                Expect.equal False (Rule.longerThan 15 sentence)
        , test "String shorter than 32 chars against Rule.longerThan 32 must be false" <|
            \_ ->
                let
                    sentence =
                        "No vegan diet, NO VEGAN POWERS!"
                in
                Expect.equal False (Rule.longerThan 32 sentence)
        , test "Empty string against Rule.shorterThan -1 must be false" <|
            \_ ->
                Expect.equal False (Rule.shorterThan -1 "")
        , test "Empty string against Rule.shorterThan 0 must be false" <|
            \_ ->
                Expect.equal False (Rule.shorterThan 0 "")
        , test "Empty string against Rule.shorterThan 1 must be true" <|
            \_ ->
                Expect.equal True (Rule.shorterThan 1 "")
        , test "String longer than 0 chars against Rule.shorterThan 0 must be false" <|
            \_ ->
                let
                    sentence =
                        "What we do in life, echoes in eternity"
                in
                Expect.equal False (Rule.shorterThan 0 sentence)
        , test "String with 31 chars against Rule.shorterThan 31 must be false" <|
            \_ ->
                let
                    sentence =
                        "Then we will fight in the shade"
                in
                Expect.equal False (Rule.shorterThan 31 sentence)
        , test "String shorter than 141 chars against Rule.shorterThan 141 must be false" <|
            \_ ->
                let
                    sentence =
                        "An idea is like a virus. Resilient. Highly contagious. And even the smallest seed of an idea can grow. It can grow to define or destroy you."
                in
                Expect.equal True (Rule.shorterThan 141 sentence)
        ]


listSuite : Test
listSuite =
    describe "List rules"
        [ test "List containing value against Rule.oneOf must be true" <|
            \_ ->
                Expect.equal True (Rule.oneOf [ -1, 0, 1 ] -1)
        , test "List not containing value against Rule.oneOf must be false" <|
            \_ ->
                Expect.equal False (Rule.oneOf [ -1, 0, 1 ] 2)
        , test "List not containing value against Rule.noneOf must be true" <|
            \_ ->
                Expect.equal True (Rule.noneOf [ -1, 0, 1 ] 2)
        , test "List containing value against Rule.noneOf must be false" <|
            \_ ->
                Expect.equal False (Rule.noneOf [ -1, 0, 1 ] 1)
        ]
