## Module Test.Spec.Assertions.Aff

#### `expectError`

``` purescript
expectError :: forall r t. Aff r t -> Aff r Unit
```

#### `expectErrorProp`

``` purescript
expectErrorProp :: forall eff prop. (Testable prop) => prop -> Aff (console :: CONSOLE, random :: RANDOM, err :: EXCEPTION | eff) Unit
```


