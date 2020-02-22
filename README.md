# elm-guards


### Usage

To create a new guard, pass the data to validate and a list of contraints:
```elm
guard : Guard String Float
guard =
  Guard.new
    4.5
    [ Guard.constraint "Number is not positive" Rule.positive
    , Guard.constraint "Number different from 5" (Rule.equal 5)
    ]

```

Validate and get a list of errors, use:
```elm
validate : Guard String Float -> Guard String Float
validate guard =
  guard
    |> Guard.validate
    |> Guard.listErrors

-- ["Number different from 5"] : List String
```

What about update the input value?
```elm
update : Float -> Guard String Float -> Guard String Float
update number guard =
  guard
    |> Guard.updateInput number
```
