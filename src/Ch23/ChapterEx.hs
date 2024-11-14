module Ch23.ChapterEx where

import Control.Lens.Combinators
import Control.Monad.Trans.State hiding (get, modify, put)

get :: State s s
get = state $ \s -> (s, s)

--- >>> runState get "curryIsAmaze"
-- ("curryIsAmaze","curryIsAmaze")

put :: s -> State s ()
put s = state $ const ((), s)

--- >>> runState (put "blah") "woot"
-- ((),"blah")

exec :: State s a -> s -> s
exec (StateT sa) = snd . runIdentity . sa

--- >>> exec get "scooby papu"
-- "scooby papu"

--- >>> exec (put "wilma") "daphne"
-- "wilma"

eval :: State s a -> s -> a
eval (StateT sa) = fst . runIdentity . sa

--- >>> eval get "bunnicula"
-- "bunnicula"

--- >>> eval get "stake a bunny"
-- "stake a bunny"

modify :: (s -> s) -> State s ()
modify f = state $ \s -> ((), f s)

sampleModify :: State Integer ()
sampleModify = modify (+ 1)

--- >>> runState sampleModify 0
-- ((),1)

--- >>> runState (sampleModify >> sampleModify) 0
-- ((),2)
