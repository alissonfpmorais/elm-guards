module GuardTests exposing (suite)

import Expect
import Guard
import Rule
import Test exposing (Test, describe, test)


suite : Test
suite =
    let
        guard =
            Guard.new ""
                [ Guard.constraint "match error" (Rule.match "elm-[a-z]+") ]
                |> Guard.validate
    in
    describe "Guard tests"
        [ test "Get input without changes must return exactly same previous value" <|
            \_ ->
                guard
                    |> Guard.getInput
                    |> Expect.equal ""
        , test "Get input with changes must return exactly changed value" <|
            \_ ->
                guard
                    |> Guard.updateInput "elm-grd"
                    |> Guard.getInput
                    |> Expect.equal "elm-grd"
        , test "When checking if invalid input is valid the guard must raise false" <|
            \_ ->
                guard
                    |> Guard.isValid
                    |> Expect.equal False
        , test "When checking if invalid input is invalid the guard must raise true" <|
            \_ ->
                guard
                    |> Guard.hasErrors
                    |> Expect.equal True
        , test "Invalid input for guard must list error" <|
            \_ ->
                guard
                    |> Guard.listErrors
                    |> Expect.equal [ "match error" ]
        , test "When checking if valid input is valid the guard must raise true" <|
            \_ ->
                guard
                    |> Guard.updateInput "elm-grd"
                    |> Guard.validate
                    |> Guard.isValid
                    |> Expect.equal True
        , test "When checking if valid input is invalid the guard must raise false" <|
            \_ ->
                guard
                    |> Guard.updateInput "elm-grd"
                    |> Guard.validate
                    |> Guard.hasErrors
                    |> Expect.equal False
        , test "Valid input for guard must list no errors" <|
            \_ ->
                guard
                    |> Guard.updateInput "elm-grd"
                    |> Guard.validate
                    |> Guard.listErrors
                    |> Expect.equal []
        , test "Invalid input with new constraint must list errors" <|
            \_ ->
                guard
                    |> Guard.addConstraints [ Guard.constraint "longer than error" (Rule.longerThan 7) ]
                    |> Guard.validate
                    |> Guard.listErrors
                    |> Expect.equal [ "longer than error", "match error" ]
        , test "Partially valid input with new constraint must list errors" <|
            \_ ->
                guard
                    |> Guard.updateInput "elm-grd"
                    |> Guard.addConstraints [ Guard.constraint "longer than error" (Rule.longerThan 7) ]
                    |> Guard.validate
                    |> Guard.listErrors
                    |> Expect.equal [ "longer than error" ]
        , test "Valid input with new constraint must list no errors" <|
            \_ ->
                guard
                    |> Guard.updateInput "elm-guards"
                    |> Guard.addConstraints [ Guard.constraint "longer than error" (Rule.longerThan 7) ]
                    |> Guard.validate
                    |> Guard.listErrors
                    |> Expect.equal []
        ]
