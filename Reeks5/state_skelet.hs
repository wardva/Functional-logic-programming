import Control.Monad(liftM,ap)

--State
data State s a = State (s-> (a,s))

instance Functor (State s) where
        fmap = liftM

instance Applicative (State s) where
        pure  = return
        (<*>) = ap


instance Monad (State s)  where
  return a = State (\s -> (a,s))
  m >>= k = State (\s ->
                      let (x,s') = (run m s)
                      in (run (k x) s'))

-- execute the state full computation
run (State f) s = f s

-- get the state
get :: State s s
get = State $ \s -> (s, s)

-- set the state
put :: s -> State s ()
put s = State $ \s -> ((), s)

-- state-change using a function
modify :: (s -> s) -> State s ()
modify f = State $ \s -> ((), f s)

--
-- Monadic Stack Machine
--
type Stack = [Int]

push :: Int -> State Stack ()
push x = State (\s -> ((), x:s))

pop :: State Stack Int
pop = State (\(x:xs) -> (x, xs))

add, mult :: State Stack ()
add  = State (\(x:y:xs) -> ((), (x+y):xs))
mult = State (\(x:y:xs) -> ((), (x*y):xs))

-- prog1 :: State Stack Int
-- prog1 = do {
--   push 20;
--   push 1;
--   add;
--   push 2;
--   mult;
--   pop;
-- }

prog1 :: State Stack Int
prog1 = push 20 >> push 1 >> add >> push 2 >> mult >> pop

test = run prog1 []
