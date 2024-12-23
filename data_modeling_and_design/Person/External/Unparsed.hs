{-|
Module      : Person.Unparsed
Description : Represents a Person in the Editing state with unparsed fields.

This module provides the `Unparsed.Person` type, which represents a person
during the data entry or editing process. All fields are optional (`Maybe` type)
and may contain invalid or incomplete data. The module exposes the
`Unparsed.Person` data constructor for flexible construction and modification
during editing.

== When to use this module

Use "Person.Unparsed" when you need to capture or manipulate user input before
parsing. Ideal for forms, data import processes, or any scenario where
data is progressively entered and may be incomplete or invalid.

== When __not__ to use this module

Do not use this module once the data has been parsed. For parsed data, use
`Person.External.Parse` to ensure all fields are correct and safe for further
processing.

== Importing Note

- This module exports the `Person` constructor. No special import
  considerations are needed.
- Import qualified to avoid name clashes and to read clearly, ie
  `Unparsed.firstName`, `Unparsed.Person`

== Usage example

@
import qualified Person.External.Unparsed as Unparsed

-- Constructing an UnparsedPerson with partial data
unparsedPerson :: Unparsed.Person
unparsedPerson = Unparsed.Person
  { Unparsed.firstName     = Just \"John\"
  , Unparsed.lastName      = Nothing
  , Unparsed.sSN           = Just \"123456789\"
  , Unparsed.maritalStatus = Nothing
  , Unparsed.phoneNumber   = Just \"5558675309\"
  }
@

-}

module Person.External.Unparsed (Person(..)) where

import qualified Data.Text as T

-- | Represents an unparsed Person. All fields are optional (`Maybe` type) and may
-- contain invalid or incomplete data.
data Person = Person
  { firstName     :: Maybe T.Text
  , lastName      :: Maybe T.Text
  , ssn           :: Maybe T.Text
  , maritalStatus :: Maybe T.Text
  , phoneNumber   :: Maybe T.Text
  }
