module SyntacticClass
  ( -- * Class
    Syntactic (..),

    -- ** Helpers
    toText,
    maybeFromText,

    -- ** Laws
    properties,

    -- * Format wrappers
    InFixedBinary (..),
    InIso8601 (..),
  )
where

import SyntacticClass.Class
import SyntacticClass.Formats.InFixedBinary
import SyntacticClass.Formats.InIso8601
import SyntacticClass.Instances.LazyText ()
import SyntacticClass.Instances.Text ()
import SyntacticClass.Instances.Uuid ()
