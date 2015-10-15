## Module Test.Spec

#### `Name`

``` purescript
type Name = String
```

#### `Result`

``` purescript
data Result
  = Success
  | Failure Error
```

##### Instances
``` purescript
instance showResult :: Show Result
instance eqResult :: Eq Result
instance showGroup :: Show (Group Result)
instance eqGroup :: Eq (Group Result)
```

#### `Group`

``` purescript
data Group t
  = Describe Name (Array (Group t))
  | It Name t
  | Pending Name
```

##### Instances
``` purescript
instance showGroup :: Show (Group Result)
instance eqGroup :: Eq (Group Result)
```

#### `Spec`

``` purescript
type Spec r t = StateT (Array (Group (Aff r Unit))) (Aff r) t
```

#### `describe`

``` purescript
describe :: forall r. String -> Spec r Unit -> Spec r Unit
```

#### `pending`

``` purescript
pending :: forall r. String -> Spec r Unit
```

#### `it`

``` purescript
it :: forall r. String -> Aff r Unit -> Spec r Unit
```

#### `collect`

``` purescript
collect :: forall r. Spec r Unit -> Aff r (Array (Group (Aff r Unit)))
```

#### `await`

``` purescript
await :: forall r. Array (Group (Aff r Unit)) -> Aff r (Array (Group Result))
```


