module Guard exposing (Guard, addConstraints, constraint, getInput, hasErrors, isValid, listErrors, new, updateInput, validate)


type alias Guard e k =
    { input : Input k
    , constraints : Constraints e k
    }


type Input k
    = Input k


type alias Constraints e k =
    { none : List (Constraint e k)
    , valid : List (Constraint e k)
    , invalid : List (Constraint e k)
    }


type alias Constraint e k =
    { rule : k -> Bool
    , error : e
    }



-- Builders


new : k -> List (Constraint e k) -> Guard e k
new value constraints =
    { input = Input value
    , constraints = initConstraints constraints
    }


constraint : e -> (k -> Bool) -> Constraint e k
constraint error rule =
    { rule = rule, error = error }


updateInput : k -> Guard e k -> Guard e k
updateInput value { constraints } =
    { input = Input value
    , constraints = resetConstraints constraints
    }


addConstraints : List (Constraint e k) -> Guard e k -> Guard e k
addConstraints newConstraints ({ constraints } as guard) =
    { guard
        | constraints = { constraints | none = constraints.none ++ newConstraints }
    }


initConstraints : List (Constraint e k) -> Constraints e k
initConstraints none =
    { none = none
    , valid = []
    , invalid = []
    }


resetConstraints : Constraints e k -> Constraints e k
resetConstraints constraints =
    initConstraints
        (constraints.none
            ++ constraints.valid
            ++ constraints.invalid
        )


ruleConstraints : Constraints e k -> Constraints e k
ruleConstraints constraints =
    { constraints | none = [] }



-- Runners


validate : Guard e k -> Guard e k
validate guard =
    let
        value : k
        value =
            getInput guard

        rules : List (Constraint e k)
        rules =
            guard.constraints.none

        constraints : Constraints e k
        constraints =
            List.foldl
                (ruleRunner value)
                (ruleConstraints guard.constraints)
                rules
    in
    { guard | constraints = constraints }


ruleRunner : k -> Constraint e k -> Constraints e k -> Constraints e k
ruleRunner value c constraints =
    if c.rule value then
        { constraints | valid = c :: constraints.valid }

    else
        { constraints | invalid = c :: constraints.invalid }



-- Error


isValid : Guard e k -> Bool
isValid { constraints } =
    List.isEmpty (constraints.invalid ++ constraints.none)


hasErrors : Guard e k -> Bool
hasErrors { constraints } =
    not (List.isEmpty constraints.invalid)


listErrors : Guard e k -> List e
listErrors { constraints } =
    let
        toError : Constraint e k -> e
        toError constraint_ =
            constraint_.error
    in
    List.map
        toError
        constraints.invalid



-- Utils


getInput : Guard e k -> k
getInput { input } =
    case input of
        Input k ->
            k
