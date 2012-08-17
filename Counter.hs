module Counter where

import Control.Concurrent.MVar

newtype Counter = Counter (MVar Integer)

new :: IO Counter
new = do
  mv <- newMVar 0
  return (Counter mv)

take :: Counter -> IO Integer
take (Counter mv) = modifyMVar mv $ \n -> return (n+1, n)



