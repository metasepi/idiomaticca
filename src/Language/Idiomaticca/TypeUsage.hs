-- | Library to change ATS type into better one with usage.
module Language.Idiomaticca.TypeUsage
  ( interpretTypeUsage
  ) where

import qualified Control.Monad.State as St
import qualified Language.ATS as A
import qualified Language.Idiomaticca.ATSUtils as A

-- | State to find better type
data BtEnv = BtEnv { btEnvFuns :: [(String, A.AFunc)] }

-- | Find better type on ATS
findBtAts :: A.AAts -> St.State BtEnv ()
findBtAts = undefined

-- | Change ATS type into better one with usage.
interpretTypeUsage :: A.AAts -> A.AAts
interpretTypeUsage = id -- xxx Should be implemented
