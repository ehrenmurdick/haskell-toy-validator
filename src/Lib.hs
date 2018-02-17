module Lib where

import           Validator

data User = User
  { name    :: Maybe String
  , email   :: Maybe String
  , address :: Maybe String
  } deriving (Show)

ehren =
  User (Just "ehren") (Just "ehren.murdick@gmail.com") (Just "123 Street rd")

invalidUser = User Nothing (Just "email") Nothing

validateUser :: User -> Validator [String] User
validateUser =
  validate
    [ (presence "email" email)
    , (presence "name" name)
    , (presence "address" address)
    ]
