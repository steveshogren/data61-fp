{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE RebindableSyntax #-}

module Course.State where

import Course.Core
import qualified Prelude as P
import Course.Optional
import Course.List
import Course.Functor
import Course.Applicative
import Course.Monad
import Data.Char as C
import qualified Data.Set as S

-- $setup
-- >>> import Test.QuickCheck.Function
-- >>> import Data.List(nub)
-- >>> import Test.QuickCheck
-- >>> import qualified Prelude as P(fmap)
-- >>> import Course.Core
-- >>> import Course.List
-- >>> instance Arbitrary a => Arbitrary (List a) where arbitrary = P.fmap listh arbitrary

-- A `State` is a function from a state value `s` to (a produced value `a`, and a resulting state `s`).
newtype State s a =
  State {
    runState :: s -> (a, s)
  }

-- | Run the `State` seeded with `s` and retrieve the resulting state.
--
-- prop> \(Fun _ f) s -> exec (State f) s == snd (runState (State f) s)
exec :: State s a -> s -> s
exec (State f) = snd . f

-- | Run the `State` seeded with `s` and retrieve the resulting value.
--
-- prop> \(Fun _ f) s -> eval (State f) s == fst (runState (State f) s)
eval :: State s a -> s -> a
eval (State f) = fst . f

-- | A `State` where the state also distributes into the produced value.
--
-- >>> runState get 0
-- (0,0)
get :: State s s
get = State (\s -> (s,s))

-- | A `State` where the resulting state is seeded with the given value.
--
-- >>> runState (put 1) 0
-- ((),1)
put :: s -> State s ()
put s = State (\_ -> ((),s))

-- | Implement the `Functor` instance for `State s`.
--
-- >>> runState ((+1) <$> State (\s -> (9, s * 2))) 3
-- (10,6)
instance Functor (State s) where
  (<$>) :: (a -> b) -> State s a -> State s b
  (<$>) f (State fa) = State (\s -> let (out,s2) = (fa s)
                                    in (f out, s2))

-- | Implement the `Applicative` instance for `State s`.
--
-- >>> runState (pure 2) 0
-- (2,0)
--
-- >>> runState (pure (+1) <*> pure 0) 0
-- (1,0)
--
-- >>> import qualified Prelude as P
-- >>> runState (State (\s -> ((+3), s P.++ ["apple"])) <*> State (\s -> (7, s P.++ ["banana"]))) []
-- (10,["apple","banana"])

-- this really challenged my expectations. The State is the NEW State that the
-- State function needs to perform its work. This is effectively an abstraction
-- over the function inside a fold or recursive loop. It needs the new State
-- before it can produce a result. In this abstraction, we execute the func
-- State func FIRST, get its new State and func, then pass just the new State to
-- the data State func. E.g. perhaps we need to get a func that needs a database
-- connection. It would execute, collect the data to make the func, then the
-- database connection is passed along to the second State that uses it to query
-- for some other data from the database. After it has finished, the resulting
-- func and data are combined to produce the "result" of the whole func, and the
-- db connection is returned along with it.
instance Applicative (State s) where
  pure :: a -> State s a
  pure a = State (\s -> (a,s))
  (<*>) :: State s (a -> b) -> State s a -> State s b
  (<*>) (State f) (State a) =
    State (\s ->
             let (f1, s1) = f s
                 (a1, s2) = a s1
             in (f1 a1, s2))

-- | Implement the `Bind` instance for `State s`.
--
-- >>> runState ((const $ put 2) =<< put 1) 0
-- ((),2)
--
-- >>> let modify f = State (\s -> ((), f s)) in runState (modify (+1) >>= \() -> modify (*2)) 7
-- ((),16)
instance Monad (State s) where
  (=<<) :: (a -> State s b) -> State s a -> State s b
  (=<<) f2 (State f1) =
      State (\s -> let (a1, s1) = f1 s
                   in  runState (f2 a1) s1)

-- | Find the first element in a `List` that satisfies a given predicate.
-- It is possible that no element is found, hence an `Optional` result.
-- However, while performing the search, we sequence some `Monad` effect through.
--
-- Note the similarity of the type signature to List#find
-- where the effect appears in every return position:
--   find ::  (a ->   Bool) -> List a ->    Optional a
--   findM :: (a -> f Bool) -> List a -> f (Optional a)
--
-- >>> let p x = (\s -> (const $ pure (x == 'c')) =<< put (1+s)) =<< get in runState (findM p $ listh ['a'..'h']) 0
-- (Full 'c',3)
--
-- >>> let p x = (\s -> (const $ pure (x == 'i')) =<< put (1+s)) =<< get in runState (findM p $ listh ['a'..'h']) 0
-- (Empty,8)
findM :: Monad f => (a -> f Bool) -> List a -> f (Optional a)
findM _ Nil = pure Empty
findM f (a:.as) =
  let result = f a
  in (\s -> if s then pure (Full a) else findM f as) =<< result


-- | Find the first element in a `List` that repeats.
-- It is possible that no element repeats, hence an `Optional` result.
--
-- /Tip:/ Use `findM` and `State` with a @Data.Set#Set@.
--
-- prop> \xs -> case firstRepeat xs of Empty -> let xs' = hlist xs in nub xs' == xs'; Full x -> length (filter (== x) xs) > 1
-- prop> \xs -> case firstRepeat xs of Empty -> True; Full x -> let (l, (rx :. rs)) = span (/= x) xs in let (l2, r2) = span (/= x) rs in let l3 = hlist (l ++ (rx :. Nil) ++ l2) in nub l3 == l3


-- We work backwards, starting by building an empty set, using 'get' to retrieve
-- the State and pass it to the helper, then retrieving the bool value, passing
-- it along to the second inner helper, then putting back on the new State.
-- Since we need to check for membership before inserting the current element,
-- we need the second inner helper
firstRepeat :: Ord a => List a -> Optional a
firstRepeat l =
  let pred elm = get
                  >>= (\set ->
                          let found = S.member elm set
                          in get >>= (\s -> put (S.insert elm s))
                                 >>= (const (pure found)))
  in fst $ runState (findM pred l) S.empty

-- | Remove all duplicate elements in a `List`.
-- /Tip:/ Use `filtering` and `State` with a @Data.Set#Set@.
--
-- prop> \xs -> firstRepeat (distinct xs) == Empty
--
-- prop> \xs -> distinct xs == distinct (flatMap (\x -> x :. x :. Nil) xs)

-- the pattern of binding a value can be done with repeated new nested lambdas
-- for each new binding
distinct :: Ord a => List a -> List a
distinct l =
  let pred elm = get >>= (\set ->
                            (pure $ S.member elm set)
                             >>= (\member ->
                                    get >>= (\s -> put (S.insert elm s))
                                          >>= (const $ pure $ not $ member)))
  in fst $ runState (filtering pred l) S.empty

-- | A happy number is a positive integer, where the sum of the square of its digits eventually reaches 1 after repetition.
-- In contrast, a sad number (not a happy number) is where the sum of the square of its digits never reaches 1
-- because it results in a recurring sequence.
--
-- /Tip:/ Use `firstRepeat` with `produce`.
--
-- /Tip:/ Use `join` to write a @square@ function.
--
-- /Tip:/ Use library functions: @Optional#contains@, @Data.Char#digitToInt@.
--
-- >>> isHappy 4
-- False
--
-- >>> isHappy 7
-- True
--
-- >>> isHappy 42
-- False
--
-- >>> isHappy 44
-- True

isHappy :: Integer -> Bool
isHappy num =
  let square = join (*)
      squares = toInteger . sum . map (square . C.digitToInt) . show'
  in contains 1 . firstRepeat . produce squares $ num

-- It is clear here what they meant by the instructions:

--   λ> happyView 4
--     [4,16,37,58,89,145,42,20,4,16]
--   λ> happyView 7
--     [7,49,97,130,10,1,1,1,1,1]
happyView num =
  let square = join (*)
      squares = toInteger . sum . map (square . C.digitToInt) . show'
  in take 10 . produce squares $ num
