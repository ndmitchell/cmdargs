
import Control.Monad
import System.Cmd
import System.Exit

main = do
    r <- system "runhaskell Main --test"
    when (r /= ExitSuccess) $ error "failed"
