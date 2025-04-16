module SyntacticClass
  ( -- * Class
    Syntactic (..),

    -- ** Helpers
    toText,
    toString,
    maybeFromText,

    -- ** Laws
    properties,

    -- * Format wrappers
    InFixedBinary (..),
    InIso8601 (..),
  )
where

import SyntacticClass.Core
import SyntacticClass.Formats.InFixedBinary
import SyntacticClass.Formats.InIso8601
import SyntacticClass.Instances.Int ()
import SyntacticClass.Instances.Int16 ()
import SyntacticClass.Instances.Int32 ()
import SyntacticClass.Instances.Int64 ()
import SyntacticClass.Instances.Int8 ()
import SyntacticClass.Instances.LazyText ()
import SyntacticClass.Instances.String ()
import SyntacticClass.Instances.Text ()
import SyntacticClass.Instances.Uuid ()
import SyntacticClass.Instances.Word ()
import SyntacticClass.Instances.Word16 ()
import SyntacticClass.Instances.Word32 ()
import SyntacticClass.Instances.Word64 ()
import SyntacticClass.Instances.Word8 ()
