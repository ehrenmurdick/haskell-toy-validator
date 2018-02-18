Toy validation library

```hs
import           Validator

data User = User
  { name    :: Maybe String
  , email   :: Maybe String
  , address :: Maybe String
  } deriving (Show)

ehren =
  User (Just "ehren") (Just "my-email-is-too-long@gmail.com") (Just "123 Street rd")

invalidUser = User Nothing (Just "email") Nothing

validateUser :: User -> [String]
validateUser =
  validate
    [ (presence "email" email)
    , (size "email is too long" email (> 10))
    , (presence "name" name)
    , (size "name is too short" name (< 10))
    , (presence "address" address)
    ]
```

Provides the following:

```hs
validateUser invalidUser
-- ["name cannot be blank","address cannot be blank"]

validateUser ehren
-- ["email is too long","name is too short"]
```
