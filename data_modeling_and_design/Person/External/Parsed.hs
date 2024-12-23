{-|
Module      : Person.External.Parsed
Description : Provides a parser to parse an Unparse Person into a Parsed Person.

== When to use this module

- Use this module after you have collected user input and wish to parse it
  before further processing.
- Ideal for ensuring data integrity before storage, computation, or business
  logic execution.

== When __not__ to use this module

- Do not use this module for initial data entry or editing phases where data may
  be incomplete.
- For data entry, use "Person.External.Unparsed".

== Special Import Requirements

- Import qualified to avoid name clashes and to read clearly, ie
  `Parsed.person`

== Usage example

@
import qualified Person.External.Unparsed as Unparsed
import qualified Person.External.Parsed as Parsed

-- Assume 'partialPerson' is an Unparsed.Person from user input
partialPerson :: Unparsed.Person
partialPerson = Unparsed.Person
  { Unparsed.firstName     = Just \"John\"
  , Unparsed.lastName      = Just \"Doe\"
  , Unparsed.ssn           = Just \"123456789\"
  , Unparsed.maritalStatus = Just \"Single\"
  , Unparsed.phoneNumber   = Just \"5558675305\"
  }

-- Parse the person
case Parsed.person partialPerson of
  Right parsedPerson -> -- proceed with parsed data
    putStrLn \"Person parsed successfully.\"
  Left errors -> -- handle parse errors
    print errors
@

-}
{-# LANGUAGE OverloadedStrings #-}

module Person.External.Parsed
  ( person
  ) where

import qualified Person.External.Unparsed as Unparsed
import qualified Person.Internal.Parse as Internal

-- | Parses an 'Unparsed.Person' and returns either a 'Parsed.Person' or a list
-- of 'ParseError's.
person :: Unparsed.Person -> Either [Internal.ParseError] Internal.Person
person uvp =
  Internal.parseToEither parse
  where
    parse :: Internal.PParse Internal.Person
    parse = Internal.Person
      <$> Internal.parseField "First Name" (Unparsed.firstName uvp) Internal.parseFirstName
      <*> Internal.parseField "Last Name" (Unparsed.lastName uvp) Internal.parseLastName
      <*> Internal.parseField "Marital Status" (Unparsed.maritalStatus uvp) Internal.parseMaritalStatus
      <*> Internal.parseField "Phone Number" (Unparsed.phoneNumber uvp) Internal.parsePhoneNumber
      <*> Internal.parseField "SSN" (Unparsed.ssn uvp) Internal.parseSSN
