## Module Test.Spec.Reporter

#### `Entry`

``` purescript
data Entry t
  = Describe (Array Name)
  | It Name t
  | Pending Name
```

##### Instances
``` purescript
instance eqEntry :: Eq (Entry Result)
instance showEntry :: Show (Entry Result)
```

#### `Reporter`

``` purescript
type Reporter e = Array (Group (Aff e Unit)) -> Aff e Unit
```

#### `collapse`

``` purescript
collapse :: forall t. Array (Group t) -> Array (Entry t)
```


