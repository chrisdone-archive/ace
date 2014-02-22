-- |

module ACE.Pretty
  (module ACE.Types.Pretty
  ,human)
  where

import ACE.Types.Pretty

import Data.Default

-- | Human-readable pretty printing settings.
human :: PrettySettings
human = def { prettyShowPrecedence = False }
