module Error.Error where

quick_error::String->Int->a
quick_error string int=error (string++": error "++show int)