{-|
Module      : Person.Internal.Parse
Description : Provides all the tools neceesary to parse an Unparsed Person into a Parsed Person.

This module provides:

- The `Person` type, which represents a person with all fields parsed and in
  a consistent, correct state.
- Parsed field types like `FirstName`, `LastName`, `SSN`, `MaritalStatus`,
  and `USPhoneNumber`.
- Functions to parse and convert an `Unparsed.Person` into a `Parsed.Person`,
  while collecting all pares errors.

== When to use this module

- This module is only to be used inside "Person.External.Parse"

== When __not__ to use this module

- Do not use this module anywhere else. There are hlint rules preventing this

== Usage example

@
import qualified Person.External.Unparsed as Unparsed
import qualified Person.Internal.Parse as Internal
import qualified Data.Text as T

parsePerson :: Unparsed.Person -> Either [Internal.ParseError] Internal.Person
parsePerson uvp =
  Internal.parseToEither personParser
  where
    personParser :: Internal.PParse Internal.Person
    personParser = Internal.Person
      <$> Internal.parseField "First Name" (Unparsed.firstName uvp) Internal.parseFirstName
      <*> Internal.parseField "Last Name" (Unparsed.lastName uvp) Internal.parseLastName
      <*> Internal.parseField "Marital Status" (Unparsed.maritalStatus uvp) Internal.parseMaritalStatus
      <*> Internal.parseField "Phone Number" (Unparsed.phoneNumber uvp) Internal.parsePhoneNumber
      <*> Internal.parseField "SSN" (Unparsed.ssn uvp) Internal.parseSSN
@
-}

module Person.Internal.Parse where

import qualified Data.List.NonEmpty as List
import qualified Data.NonEmptyText as NET
import qualified Data.Text as T

-- | Represents a person with all fields parsed.
data Person = Person
  { firstName     :: FirstName
  , lastName      :: LastName
  , maritalStatus :: MaritalStatus
  , phoneNumber   :: USPhoneNumber
  , ssn           :: SSN
  } deriving (Show, Eq)

-- | Parsed non-empty first name.
newtype FirstName = FirstName NET.NonEmptyText deriving (Show, Eq)
-- | Parsed non-empty last name.
newtype LastName = LastName NET.NonEmptyText deriving (Show, Eq)

-- | Parsed marital status.
data MaritalStatus
  = Single
  | Married
  | Divorced
  | Widowed
  deriving (Show, Eq)

-- | Parsed social security number (9-digit number).
newtype SSN = SSN NET.NonEmptyText deriving (Show, Eq)
-- | Parsed US phone number (10-digit number).
newtype USPhoneNumber = USPhoneNumber NET.NonEmptyText deriving (Show, Eq)
-- | Represents a parse error with a message.
newtype ParseError = ParseError NET.NonEmptyText deriving (Show, Eq)

data Parser e v = Failure e | Success v deriving (Show)

instance Functor (Parser e) where
  fmap f (Success v) = Success (f v)
  fmap _ (Failure e) = Failure e

instance Semigroup e => Applicative (Parser e) where
  pure = Success
  Success f <*> Success v = Success (f v)
  Failure e1 <*> Failure e2 = Failure (e1 <> e2)
  Failure e1 <*> _          = Failure e1
  _          <*> Failure e2 = Failure e2

type PParser a = Parser (List.NonEmpty (ParseError a))

parseToEither :: Parser e a -> Either e a
parseToEither (Failure e) = Left e
parseToEither (Success v) = Right v

parseField :: T.Text -> Maybe T.Text -> (T.Text -> Either ParseError b) -> PParser b
parseField fieldName Nothing _ = Failure $ List.listToNonEmpty [ParseError $ T.pack (fieldName ++ " is missing")]
parseField _ (Just t) f = case f t of
  Left err -> Failure [err]
  Right val -> Success val

-- | https://www.kalzumeus.com/2010/06/17/falsehoods-programmers-believe-about-names
parseFirstName :: T.Text -> Either ParseError FirstName
parseFirstName s = undefined

parseLastName :: T.Text -> Either ParseError LastName
parseLastName s = undefined

{-|
This can be improved by adhering to https://en.wikipedia.org/wiki/Social_Security_number#Valid_SSNs

Some special numbers are never allocated:

- Numbers with all zeros in any digit group (000-##-####, ###-00-####,
- ###-##-0000).  Numbers with 666 or 900–999 (Individual Taxpayer Identification
- Number) in the
  first digit group.
-}
parseSSN :: T.Text -> Either ParseError SSN
parseSSN s = undefined

parseMaritalStatus :: T.Text -> Either ParseError MaritalStatus
parseMaritalStatus s = undefined

-- | Might be able to glean some info from
-- https://en.wikipedia.org/wiki/North_American_Numbering_Plan to improve this
parsePhoneNumber :: T.Text -> Either ParseError USPhoneNumber
parsePhoneNumber s = undefined
