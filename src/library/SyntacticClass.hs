module SyntacticClass
  ( -- * Class
    Syntactic (..),

    -- ** Helpers
    toText,
    maybeFromText,

    -- ** Laws
    syntacticProperties,

    -- * Wrappers
    InIso8601 (..),
  )
where

import SyntacticClass.Class
import SyntacticClass.Instances.InIso8601
import SyntacticClass.Instances.LazyText ()
import SyntacticClass.Instances.Text ()
import SyntacticClass.Instances.Uuid ()
