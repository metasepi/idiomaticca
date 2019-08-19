-- | Library to change ATS type into better one with usage.
module Language.Idiomaticca.TypeUsage
  ( interpretTypeUsage
  ) where

import qualified Language.ATS as A
import qualified Language.Idiomaticca.ATSUtils as A

-- | Change ATS type into better one with usage.
interpretTypeUsage :: A.AAts -> A.AAts
interpretTypeUsage = id -- xxx Should be implemented
