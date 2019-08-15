module Rule exposing (contains, different, equal, even, exactly, greaterThan, longerThan, lowerThan, match, negative, noneOf, odd, oneOf, positive, shorterThan)

import Regex exposing (Regex)



-- Int


even : Int -> Bool
even number =
    modBy 2 number == 0


odd : Int -> Bool
odd number =
    not (even number)



-- Float


positive : Float -> Bool
positive number =
    number > 0


negative : Float -> Bool
negative number =
    number < 0


equal : Float -> Float -> Bool
equal base number =
    number == base


different : Float -> Float -> Bool
different base number =
    not (number == base)


greaterThan : Float -> Float -> Bool
greaterThan base number =
    number > base


lowerThan : Float -> Float -> Bool
lowerThan base number =
    number < base



-- String


contains : String -> String -> Bool
contains pattern text =
    let
        regex =
            Maybe.withDefault Regex.never <|
                Regex.fromString pattern
    in
    Regex.contains regex text


match : String -> String -> Bool
match pattern text =
    contains ("^" ++ pattern ++ "$") text


exactly : Int -> String -> Bool
exactly base text =
    String.length text == base


longerThan : Int -> String -> Bool
longerThan base text =
    String.length text > base


shorterThan : Int -> String -> Bool
shorterThan base text =
    String.length text < base



-- List


oneOf : List k -> k -> Bool
oneOf list value =
    List.any
        (\i -> value == i)
        list


noneOf : List k -> k -> Bool
noneOf list value =
    not (oneOf list value)
