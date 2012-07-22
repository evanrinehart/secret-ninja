module GameTime where

import Data.Time

{-
1 hour = 1 game day would be cool
but i would also like to keep the rate flexible

for example i would like to switch to
1 day = 1 game day for 1 exactly 1 day
then return to normal
and not have any drift

1 hour = 1 game day = 3600 seconds
1 day = 1 game day = 86400 seconds

game time units:
1 game day consists of 24 game hours
each game hour is 60 game minutes
we only need game minutes
thats 1440 game minutes in a game day


-}




