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
Show Result
Eq Result
```

#### `Group`

``` purescript
data Group
  = Describe Name (Array Group)
  | It Name Result
  | Pending Name
```

##### Instances
``` purescript
Show Group
Eq Group
```

#### `Spec`

``` purescript
type Spec r t = StateT (Array Group) (Aff r) t
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
collect :: forall r. Spec r Unit -> Aff r (Array Group)
```

#### `prop`

``` purescript
prop :: forall r prop. (Testable prop) => String -> prop -> Spec (console :: CONSOLE, random :: RANDOM, err :: EXCEPTION | r) Unit
```


