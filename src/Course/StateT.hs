{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE OverloadedStrings #-}

module Course.StateT where

import Course.Core
import Course.ExactlyOne
import Course.Optional
import Course.List
import Course.Functor
import Course.Applicative
import Course.Monad
import Course.State
import qualified Data.Set as S
import qualified Prelude as P

-- $setup
-- >>> import Test.QuickCheck
-- >>> import qualified Prelude as P(fmap)
-- >>> instance Arbitrary a => Arbitrary (List a) where arbitrary = P.fmap listh arbitrary

-- | A `StateT` is a function from a state value `s` to a functor f of (a produced value `a`, and a resulting state `s`).
newtype StateT s f a = StateT {runStateT :: s -> f (a, s)}

-- | Implement the `Functor` instance for @StateT s f@ given a @Functor f@.
--
-- >>> runStateT ((+1) <$> (pure 2) :: StateT Int List Int) 0
-- [(3,0)]

-- State and StateT are like thunks that need to be evaluated to
-- build the "next" thunk that performs work on the result

-- * Take the function that is inside a StateT (which we'll call k)
-- * Construct a new StateT, which first runs k, and then fmaps into the f (a, s) returned by k.
-- We use first to fmap over the a instead of the (a, s)
instance Functor f => Functor (StateT s f) where
  (<$>) :: (a -> b) -> StateT s f a -> StateT s f b
  (<$>) ab (StateT s_f_a) = StateT (\s -> let f_a_s2 = (s_f_a s)
                                          in (\(a, s2) -> (ab a, s2)) <$> f_a_s2)

-- | Implement the `Applicative` instance for @StateT s f@ given a @Monad f@.
--
-- >>> runStateT (pure 2) 0
-- (2,0)
--
-- >>> runStateT ((pure 2) :: StateT Int List Int) 0
-- [(2,0)]
--
-- >>> runStateT (pure (+2) <*> ((pure 2) :: StateT Int List Int)) 0
-- [(4,0)]
--
-- >>> import qualified Prelude as P
-- >>> runStateT (StateT (\s -> Full ((+2), s P.++ [1])) <*> (StateT (\s -> Full (2, s P.++ [2])))) [0]
-- Full (4,[0,1,2])
--
-- >>> runStateT (StateT (\s -> ((+2), s P.++ [1]) :. ((+3), s P.++ [1]) :. Nil) <*> (StateT (\s -> (2, s P.++ [2]) :. Nil))) [0]
-- [(4,[0,1,2]),(5,[0,1,2])]
instance Monad f => Applicative (StateT s f) where
  pure :: a -> StateT s f a
  pure a = StateT (\s -> pure (a,s))
  (<*>) :: StateT s f (a -> b) -> StateT s f a -> StateT s f b
  (<*>) (StateT s_f_ab) s_f_a =
    StateT (\s -> let f_ab_s2 = (s_f_ab s)
                  in f_ab_s2 >>= (\(ab,s2) -> runStateT (ab <$> s_f_a) s2))

-- | Implement the `Monad` instance for @StateT s f@ given a @Monad f@.
-- Make sure the state value is passed through in `bind`.
--
-- >>> runStateT ((const $ putT 2) =<< putT 1) 0
-- ((),2)
--
-- >>> let modify f = StateT (\s -> pure ((), f s)) in runStateT (modify (+1) >>= \() -> modify (*2)) 7
-- ((),16)
instance Monad f => Monad (StateT s f) where
  (=<<) :: (a -> StateT s f b) -> StateT s f a -> StateT s f b
  (=<<) a_sfb (StateT sfa) =
    StateT (\s -> let fa = sfa s
                  in fa >>= (\(a,s2) -> runStateT (a_sfb a) s2))

-- | A `State'` is `StateT` specialised to the `ExactlyOne` functor.
type State' s a =
  StateT s ExactlyOne a

-- | Provide a constructor for `State'` values
--
-- >>> runStateT (state' $ runState $ put 1) 0
-- ExactlyOne  ((),1)
state' :: (s -> (a, s)) -> State' s a
state' f = StateT (\s -> ExactlyOne (f s))

-- | Provide an unwrapper for `State'` values.
--
-- >>> runState' (state' $ runState $ put 1) 0
-- ((),1)
runState' :: State' s a -> s -> (a, s)
runState' (StateT st) s =
  runExactlyOne $ st s

-- | Run the `StateT` seeded with `s` and retrieve the resulting state.
execT :: Functor f => StateT s f a -> s -> f s
execT (StateT sfa) s = snd <$> (sfa s)

-- | Run the `State` seeded with `s` and retrieve the resulting state.
exec' :: State' s a -> s -> s
exec' (StateT sa) s = snd $ runExactlyOne $ sa s

-- | Run the `StateT` seeded with `s` and retrieve the resulting value.
evalT :: Functor f => StateT s f a -> s -> f a
evalT (StateT sfa) s = fst <$> sfa s

-- | Run the `State` seeded with `s` and retrieve the resulting value.
eval' :: State' s a -> s -> a
eval' (StateT sa) s = fst $ runExactlyOne $ sa s

-- | A `StateT` where the state also distributes into the produced value.
--
-- >>> (runStateT (getT :: StateT Int List Int) 3)
-- [(3,3)]
getT :: Applicative f => StateT s f s
getT = StateT (\s -> pure (s,s))

-- | A `StateT` where the resulting state is seeded with the given value.
--
-- >>> runStateT (putT 2) 0
-- ((),2)
--
-- >>> runStateT (putT 2 :: StateT Int List ()) 0
-- [((),2)]
putT :: Applicative f => s -> StateT s f ()
putT s = StateT (\_ -> pure ((),s) )

-- | Remove all duplicate elements in a `List`.
--
-- /Tip:/ Use `filtering` and `State'` with a @Data.Set#Set@.
--
-- prop> \xs -> distinct' xs == distinct' (flatMap (\x -> x :. x :. Nil) xs)

-- filtering :: Applicative f => (a -> f Bool) -> List a -> f (List a)

distinctPred' :: (Ord a) => a -> StateT (S.Set a) ExactlyOne Bool
distinctPred' x = getT >>= (\set -> let inSet = S.member x set
                                    in  (putT $ S.insert x set) >>= (const $ pure $ not inSet))

distinctPred'' :: (Ord a) => a -> StateT (S.Set a) ExactlyOne Bool
distinctPred'' x = StateT (\set -> let inSet = S.member x set
                                   in  ExactlyOne ((not inSet), (S.insert x set)))

distinct' :: (Ord a, Num a) => List a -> List a
distinct' l =
  fst $ runState' (filtering distinctPred'' l) S.empty

-- | Remove all duplicate elements in a `List`.
-- However, if you see a value greater than `100` in the list,
-- abort the computation by producing `Empty`.
--
-- /Tip:/ Use `filtering` and `StateT` over `Optional` with a @Data.Set#Set@.
--
-- >>> distinctF $ listh [1,2,3,2,1]
-- Full [1,2,3]
--
-- >>> distinctF $ listh [1,2,3,2,1,101]
-- Empty

-- filtering :: Applicative f => (a -> f Bool) -> List a -> f (List a)

distinctFPred :: (Ord a, Num a) => a -> StateT (S.Set a) Optional Bool
distinctFPred x = getT >>= (\set -> let inSet = S.member x set
                                       in (putT $ S.insert x set) >>=
                                          (\_ -> if x > 100 then StateT (\_ -> Empty) else (pure $ not inSet)) )

distinctFPred' :: (Ord a, Num a) => a -> StateT (S.Set a) Optional Bool
distinctFPred' x = StateT (\set -> let nextState = (not $ S.member x set, S.insert x set)
                                   in if x > 100 then Empty else (Full nextState))

distinctF :: (Ord a, Num a) => List a -> Optional (List a)
distinctF l =
  let x = runStateT (filtering distinctFPred' l) S.empty
  in fst <$> x


-- | An `OptionalT` is a functor of an `Optional` value.
data OptionalT f a =
  OptionalT {
    runOptionalT ::
      f (Optional a)
  }

-- | Implement the `Functor` instance for `OptionalT f` given a Functor f.
--
-- >>> runOptionalT $ (+1) <$> OptionalT (Full 1 :. Empty :. Nil)
-- [Full 2,Empty]
fmap :: Functor f => (a -> b) -> f a -> f b
fmap = (<$>)

instance Functor f => Functor (OptionalT f) where
  (<$>) :: (a -> b) -> (OptionalT f a) -> (OptionalT f b)
  (<$>) f (OptionalT fx) =
    OptionalT ((fmap . fmap $ f) fx)

-- | Implement the `Applicative` instance for `OptionalT f` given a Applicative f.
--
-- >>> runOptionalT $ OptionalT (Full (+1) :. Full (+2) :. Nil) <*> OptionalT (Full 1 :. Empty :. Nil)
-- [Full 2,Empty,Full 3,Empty]

ap' :: Applicative f => f (a -> b) -> f a -> f b
ap' = (<*>)

instance Applicative f => Applicative (OptionalT f) where
  pure :: a -> OptionalT f a
  pure x = OptionalT (pure $ Full x)
  (<*>) :: OptionalT f (a -> b) -> OptionalT f a -> OptionalT f b
  (<*>) (OptionalT f_o_ab) (OptionalT f_o_a) =
    OptionalT (lift2 ap' f_o_ab f_o_a)

-- | Implement the `Monad` instance for `OptionalT f` given a Monad f.
--
-- >>> runOptionalT $ (\a -> OptionalT (Full (a+1) :. Full (a+2) :. Nil)) =<< OptionalT (Full 1 :. Empty :. Nil)
-- [Full 2,Full 3,Empty]
instance Monad f => Monad (OptionalT f) where
  (=<<) :: (a -> OptionalT f b) -> OptionalT f a -> OptionalT f b
  (=<<) a_fb (OptionalT f_o_a) =
    OptionalT ((\o -> case o of
                       Empty -> pure Empty
                       Full a -> runOptionalT (a_fb a)) =<< f_o_a)

-- | A `Logger` is a pair of a list of log values (`[l]`) and an arbitrary value (`a`).
data Logger l a =
  Logger (List l) a
  deriving (Eq, Show)

-- | Implement the `Functor` instance for `Logger
--
-- >>> (+3) <$> Logger (listh [1,2]) 3
-- Logger [1,2] 6
instance Functor (Logger l) where
  (<$>) :: (a -> b) -> Logger l a -> Logger l b
  (<$>) f (Logger l a )=
    Logger l (f a)

-- | Implement the `Applicative` instance for `Logger`.
--
-- >>> pure "table" :: Logger Int P.String
-- Logger [] "table"
--
-- >>> Logger (listh [1,2]) (+7) <*> Logger (listh [3,4]) 3
-- Logger [1,2,3,4] 10
instance Applicative (Logger l) where
  pure :: a -> Logger l a
  pure a =
    Logger Nil a
  (<*>) :: Logger l (a->b) -> Logger l a -> Logger l b
  (<*>) (Logger l1 f) (Logger l2 a) =
    Logger (l1 ++ l2) (f a)

-- | Implement the `Monad` instance for `Logger`.
-- The `bind` implementation must append log values to maintain associativity.
--
-- >>> (\a -> Logger (listh [4,5]) (a+3)) =<< Logger (listh [1,2]) 3
-- Logger [1,2,4,5] 6
instance Monad (Logger l) where
  (=<<) :: (a -> (Logger l b)) -> (Logger l a) -> (Logger l b)
  (=<<) f (Logger l1 a) =
    let (Logger l2 b) = f a
    in Logger (l1 ++ l2) b

-- | A utility function for producing a `Logger` with one log value.
--
-- >>> log1 1 2
-- Logger [1] 2
log1 :: l -> a -> Logger l a
log1 l a =
  Logger (l :. Nil) a

-- | Remove all duplicate integers from a list. Produce a log as you go.
-- If there is an element above 100, then abort the entire computation and produce no result.
-- However, always keep a log. If you abort the computation, produce a log with the value,
-- "aborting > 100: " followed by the value that caused it.
-- If you see an even number, produce a log message, "even number: " followed by the even number.
-- Other numbers produce no log message.
--
-- /Tip:/ Use `filtering` and `StateT` over (`OptionalT` over `Logger` with a @Data.Set#Set@).
--
-- >>> distinctG $ listh [1,2,3,2,6]
-- Logger ["even number: 2","even number: 2","even number: 6"] (Full [1,2,3,6])
--
-- >>> distinctG $ listh [1,2,3,2,6,106]
-- Logger ["even number: 2","even number: 2","even number: 6","aborting > 100: 106"] Empty

distinctGPred :: (Integral a, Show a) => a -> StateT (S.Set a) (OptionalT (Logger Chars)) Bool
distinctGPred a =
  StateT (\set -> OptionalT (
                               if a > 100 then
                                 log1 (fromString ("aborting > 100: " P.++ show a)) Empty
                               else
                                 let nextState = (Full (a `S.notMember` set, a `S.insert` set))
                                 in (if even a then
                                       log1 (fromString ("even number: " P.++ show a)) nextState
                                      else pure nextState)))

-- filtering :: Applicative f => (a -> f Bool) -> List a -> f (List a)

distinctG :: (Integral a, Show a) => List a -> Logger Chars (Optional (List a))
distinctG l =
   runOptionalT $ (evalT (filtering distinctGPred l) S.empty)

-- distinctFPred :: (Ord a, Num a) => a -> StateT (S.Set a) Optional Bool
-- distinctFPred x = getT >>= (\set -> let inSet = S.member x set
--                                        in (putT $ S.insert x set) >>=
--                                           (\_ -> if x > 100 then StateT (\_ -> Empty) else (pure $ not inSet)) )
-- distinctF :: (Ord a, Num a) => List a -> Optional (List a)
-- distinctF l =
--   let x = runStateT (filtering distinctFPred l) S.empty
--   in fst <$> x
