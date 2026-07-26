module Error.Error where

import GHC.Stack.Types

quick_error::HasCallStack=>String->Int->a
quick_error string int=error (string++": error "++show int)