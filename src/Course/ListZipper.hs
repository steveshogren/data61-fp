{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Course.ListZipper where

import Course.Core
import Course.List
import Course.Optional
import Course.Functor
import Course.Applicative
import Course.Extend
import Course.Comonad
import Course.Traversable
import Course.StateT as ST
import qualified Prelude as P

-- $setup
-- >>> import Test.QuickCheck
-- >>> import Data.Maybe(maybe)
-- >>> import Course.Core
-- >>> import qualified Prelude as P
-- >>> let optional e _ Empty = e; optional _ f (Full a) = f a
-- >>> instance Arbitrary a => Arbitrary (Optional a) where arbitrary = P.fmap (maybe Empty Full) arbitrary
-- >>> instance Arbitrary a => Arbitrary (List a) where arbitrary = P.fmap (P.foldr (:.) Nil :: ([a] -> List a)) arbitrary
-- >>> instance Arbitrary a => Arbitrary (ListZipper a) where arbitrary = do l <- arbitrary; x <- arbitrary; r <- arbitrary; P.return (ListZipper l x r)

-- A `ListZipper` is a focussed position, with a list of values to the left and to the right.
--
-- For example, taking the list [0,1,2,3,4,5,6], the moving focus to the third position, the zipper looks like:
-- ListZipper [2,1,0] 3 [4,5,6]
--
-- Supposing then we move left on this zipper:
-- ListZipper [1,0] 2 [3,4,5,6]
--
-- then suppose we add 17 to the focus of this zipper:
-- ListZipper [1,0] 19 [3,4,5,6]
data ListZipper a =
  ListZipper (List a) a (List a)
  deriving Eq

lefts :: ListZipper a -> List a
lefts (ListZipper l _ _) = l

rights :: ListZipper a -> List a
rights (ListZipper _ _ r) = r

-- A `MaybeListZipper` is a data structure that allows us to "fail" zipper operations.
-- e.g. Moving left when there are no values to the left.
--
-- We then overload operations polymorphically to operate on both `ListZipper` and `MaybeListZipper`
-- using the `ListZipper'` type-class below.
data MaybeListZipper a =
  IsZ (ListZipper a)
  | IsNotZ
  deriving Eq

-- | Implement the `Functor` instance for `ListZipper`.
--
-- >>> (+1) <$> (zipper [3,2,1] 4 [5,6,7])
-- [4,3,2] >5< [6,7,8]
instance Functor ListZipper where
  (<$>) :: (a -> b) -> ListZipper a -> ListZipper b
  (<$>) a_b (ListZipper l a r) = (ListZipper (a_b <$> l) (a_b a) (a_b <$> r))

-- | Implement the `Functor` instance for `MaybeListZipper`.
--
-- >>> (+1) <$> (IsZ (zipper [3,2,1] 4 [5,6,7]))
-- [4,3,2] >5< [6,7,8]
instance Functor MaybeListZipper where
  (<$>) :: (a -> b) -> MaybeListZipper a -> MaybeListZipper b
  (<$>) _ IsNotZ = IsNotZ
  (<$>) a_b (IsZ lz) = IsZ (a_b <$> lz)

-- | Convert the given zipper back to a list.
--
-- >>> toList <$> toOptional (fromList Nil)
-- Empty
--
-- >>> toList (ListZipper Nil 1 (2:.3:.4:.Nil))
-- [1,2,3,4]
--
-- >>> toList (ListZipper (3:.2:.1:.Nil) 4 (5:.6:.7:.Nil))
-- [1,2,3,4,5,6,7]
toList :: ListZipper a -> List a
toList (ListZipper l a r) = (reverse l) ++ (a:.Nil) ++ r

-- | Convert the given (maybe) zipper back to a list.
toListZ :: MaybeListZipper a -> List a
toListZ IsNotZ = Nil
toListZ (IsZ z) = toList z

-- | Create a `MaybeListZipper` positioning the focus at the head.
--
-- ->>> fromList (1 :. 2 :. 3 :. Nil)
-- [] >1< [2,3]
--
-- >>> fromList Nil
-- ><
--
-- prop> \xs -> xs == toListZ (fromList xs)
fromList :: List a -> MaybeListZipper a
fromList Nil = IsNotZ
fromList (x:.xs) = IsZ (ListZipper Nil x xs)

-- | Retrieve the `ListZipper` from the `MaybeListZipper` if there is one.
--
-- prop> \xs -> isEmpty xs == (toOptional (fromList xs) == Empty)
--
-- prop> \z -> toOptional (fromOptional z) == z
toOptional :: MaybeListZipper a -> Optional (ListZipper a)
toOptional IsNotZ = Empty
toOptional (IsZ lz) = Full lz

zipper :: [a] -> a -> [a] -> ListZipper a
zipper l x r =
  ListZipper (listh l) x (listh r)

fromOptional :: Optional (ListZipper a) -> MaybeListZipper a
fromOptional Empty = IsNotZ
fromOptional (Full z) = IsZ z

asZipper :: (ListZipper a -> ListZipper a) -> MaybeListZipper a -> MaybeListZipper a
asZipper f =
  asMaybeZipper (IsZ . f)

(>$>):: (ListZipper a -> ListZipper a) -> MaybeListZipper a -> MaybeListZipper a
(>$>) =
  asZipper

asMaybeZipper :: (ListZipper a -> MaybeListZipper a) -> MaybeListZipper a -> MaybeListZipper a
asMaybeZipper _ IsNotZ =
  IsNotZ
asMaybeZipper f (IsZ z) =
  f z

(-<<) :: (ListZipper a -> MaybeListZipper a) -> MaybeListZipper a -> MaybeListZipper a
(-<<) =
  asMaybeZipper

-- | Update the focus of the zipper with the given function on the current focus.
--
-- >>> withFocus (+1) (zipper [] 0 [1])
-- [] >1< [1]
--
-- >>> withFocus (+1) (zipper [1,0] 2 [3,4])
-- [1,0] >3< [3,4]
withFocus :: (a -> a) -> ListZipper a -> ListZipper a
withFocus f (ListZipper l x r) =
  ListZipper l (f x) r

-- | Set the focus of the zipper to the given value.
-- /Tip:/ Use `withFocus`.
--
-- >>> setFocus 1 (zipper [] 0 [1])
-- [] >1< [1]
--
-- >>> setFocus 1 (zipper [1,0] 2 [3,4])
-- [1,0] >1< [3,4]
setFocus :: a -> ListZipper a -> ListZipper a
setFocus a lz = withFocus (const a) lz

-- A flipped infix alias for `setFocus`. This allows:
--
-- z .= "abc" -- sets the focus on the zipper z to the value "abc".
(.=) :: ListZipper a -> a -> ListZipper a
(.=) =
  flip setFocus

-- | Returns whether there are values to the left of focus.
--
-- >>> hasLeft (zipper [1,0] 2 [3,4])
-- True
--
-- >>> hasLeft (zipper [] 0 [1,2])
-- False
hasLeft :: ListZipper a -> Bool
hasLeft (ListZipper l _ _) = not $ isEmpty l

-- | Returns whether there are values to the right of focus.
--
-- >>> hasRight (zipper [1,0] 2 [3,4])
-- True
--
-- >>> hasRight (zipper [1,0] 2 [])
-- False
hasRight :: ListZipper a -> Bool
hasRight (ListZipper _ _ r) = not $ isEmpty r

-- | Seek to the left for a location matching a predicate, starting from the
-- current one.
--
-- /Tip:/ Use `break`
--
-- prop> \xs p -> findLeft (const p) -<< fromList xs == IsNotZ
--
-- >>> findLeft (== 1) (zipper [2, 1] 3 [4, 5])
-- [] >1< [2,3,4,5]
--
-- >>> findLeft (== 6) (zipper [2, 1] 3 [4, 5])
-- ><
--
-- >>> findLeft (== 1) (zipper [2, 1] 1 [4, 5])
-- [] >1< [2,1,4,5]
--
-- >>> findLeft (== 1) (zipper [1, 2, 1] 3 [4, 5])
-- [2,1] >1< [3,4,5]
--
-- >>> findLeft (== 1) (zipper [3, 4, 1, 5] 9 [2, 7])
-- [5] >1< [4,3,9,2,7]


findLeft :: (a -> Bool) -> ListZipper a -> MaybeListZipper a
findLeft pred (ListZipper l m r) =
  let (pop, keep) = break pred l
  in case keep of
       Nil -> IsNotZ
       (mid:.left) -> IsZ $ (ListZipper left mid ((reverse pop)++(m:.r)))

-- | Seek to the right for a location matching a predicate, starting from the
-- current one.
--
-- /Tip:/ Use `break`
--
-- prop> \xs -> findRight (const False) -<< fromList xs == IsNotZ
--
-- >>> findRight (== 5) (zipper [2, 1] 3 [4, 5])
-- [4,3,2,1] >5< []
--
-- >>> findRight (== 6) (zipper [2, 1] 3 [4, 5])
-- ><
--
-- >>> findRight (== 1) (zipper [2, 3] 1 [4, 5, 1])
-- [5,4,1,2,3] >1< []
--
-- >>> findRight (== 1) (zipper [2, 3] 1 [1, 4, 5, 1])
-- [1,2,3] >1< [4,5,1]
findRight :: (a -> Bool) -> ListZipper a -> MaybeListZipper a
findRight pred (ListZipper l m r) =
  let (pop, keep) = break pred r
  in case keep of
       Nil -> IsNotZ
       (mid:.right) -> IsZ $ (ListZipper ((reverse (m:.pop))++l) mid right)

-- | Move the zipper left, or if there are no elements to the left, go to the far right.
--
-- >>> moveLeftLoop (zipper [3,2,1] 4 [5,6,7])
-- [2,1] >3< [4,5,6,7]
--
-- >>> moveLeftLoop (zipper [] 1 [2,3,4])
-- [3,2,1] >4< []
moveLeftLoop :: ListZipper a -> ListZipper a
moveLeftLoop lz@(ListZipper Nil _ Nil) = lz
moveLeftLoop (ListZipper (newMid:.ls) m r) = (ListZipper ls newMid (m:.r))
moveLeftLoop (ListZipper Nil m r) =
  let (newMid:.newLeft) = reverse (m:.r)
  in (ListZipper newLeft newMid Nil)

-- | Move the zipper right, or if there are no elements to the right, go to the far left.
--
-- >>> moveRightLoop (zipper [3,2,1] 4 [5,6,7])
-- [4,3,2,1] >5< [6,7]
--
-- >>> moveRightLoop (zipper [3,2,1] 4 [])
-- [] >1< [2,3,4]
moveRightLoop :: ListZipper a -> ListZipper a
moveRightLoop lz@(ListZipper Nil _ Nil) = lz
moveRightLoop (ListZipper l m (newMid:.r)) = (ListZipper (m:.l) newMid r)
moveRightLoop (ListZipper l m Nil) =
  let (newMid:.newRight) = reverse (m:.l)
  in (ListZipper Nil newMid newRight)

-- | Move the zipper one position to the left.
--
-- >>> moveLeft (zipper [3,2,1] 4 [5,6,7])
-- [2,1] >3< [4,5,6,7]
--
-- >>> moveLeft (zipper [] 1 [2,3,4])
-- ><
moveLeft :: ListZipper a -> MaybeListZipper a
moveLeft (ListZipper Nil _ _) = IsNotZ
moveLeft (ListZipper (newMid:.ls) m r) = IsZ (ListZipper ls newMid (m:.r))

-- | Move the zipper one position to the right.
--
-- >>> moveRight (zipper [3,2,1] 4 [5,6,7])
-- [4,3,2,1] >5< [6,7]
--
-- >>> moveRight (zipper [3,2,1] 4 [])
-- ><
moveRight :: ListZipper a -> MaybeListZipper a
moveRight (ListZipper _ _ Nil) = IsNotZ
moveRight (ListZipper l m (newMid:.r)) = IsZ (ListZipper (m:.l) newMid r)

-- | Swap the current focus with the value to the left of focus.
--
-- >>> swapLeft (zipper [3,2,1] 4 [5,6,7])
-- [4,2,1] >3< [5,6,7]
--
-- >>> swapLeft (zipper [] 1 [2,3,4])
-- ><
swapLeft :: ListZipper a -> MaybeListZipper a
swapLeft (ListZipper Nil _ _) = IsNotZ
swapLeft (ListZipper (newMid:.ls) m r) = IsZ (ListZipper (m:.ls) newMid r)

-- | Swap the current focus with the value to the right of focus.
--
-- >>> swapRight (zipper [3,2,1] 4 [5,6,7])
-- [3,2,1] >5< [4,6,7]
--
-- >>> swapRight (zipper [3,2,1] 4 [])
-- ><
swapRight :: ListZipper a -> MaybeListZipper a
swapRight (ListZipper _ _ Nil) = IsNotZ
swapRight (ListZipper l m (newMid:.r)) = IsZ (ListZipper l newMid (m:.r))

-- | Drop all values to the left of the focus.
--
-- >>> dropLefts (zipper [3,2,1] 4 [5,6,7])
-- [] >4< [5,6,7]
--
-- >>> dropLefts (zipper [] 1 [2,3,4])
-- [] >1< [2,3,4]
--
-- prop> \l x r -> dropLefts (zipper l x r) == zipper [] x r
dropLefts :: ListZipper a -> ListZipper a
dropLefts (ListZipper _ m r) = ListZipper Nil m r

-- | Drop all values to the right of the focus.
--
-- >>> dropRights (zipper [3,2,1] 4 [5,6,7])
-- [3,2,1] >4< []
--
-- >>> dropRights (zipper [3,2,1] 4 [])
-- [3,2,1] >4< []
--
-- prop> \l x r -> dropRights (zipper l x r) == zipper l x []
dropRights :: ListZipper a -> ListZipper a
dropRights (ListZipper l m _) = ListZipper l m Nil

-- | Move the focus left the given number of positions. If the value is negative, move right instead.
--
-- >>> moveLeftN 2 (zipper [2,1,0] 3 [4,5,6])
-- [0] >1< [2,3,4,5,6]
--
-- >>> moveLeftN (-1) $ zipper [2,1,0] 3 [4,5,6]
-- [3,2,1,0] >4< [5,6]

-- moveLeft (ListZipper Nil _ _) = IsNotZ
-- moveLeft (ListZipper (newMid:.ls) m r) = IsZ (ListZipper ls newMid (m:.r))

moveLeftN :: Int -> ListZipper a -> MaybeListZipper a
moveLeftN 0 l = IsZ l
moveLeftN x lz@(ListZipper Nil _ _ )
  | x > 0 = IsNotZ
  | x < 0 = moveRightN ((-1) * x) lz
moveLeftN x lz@(ListZipper (l:.ls) m r)
  | x > 0 = moveLeftN (x - 1) (ListZipper ls l (m:.r))
  | x < 0 = moveRightN ((-1) * x) lz
moveLeftN _ _ = IsNotZ

  -- | x > 0 = ((moveLeftN (x-1)) <$> (moveLeft l))
  -- | x < 0 = sequenceA $ (moveLeftN (x+1)) <$> (moveRight l)

-- | Move the focus right the given number of positions. If the value is negative, move left instead.
--
-- >>> moveRightN 1 (zipper [2,1,0] 3 [4,5,6])
-- [3,2,1,0] >4< [5,6]
--
-- >>> moveRightN (-1) $ zipper [2,1,0] 3 [4,5,6]
-- [1,0] >2< [3,4,5,6]
moveRightN :: Int -> ListZipper a -> MaybeListZipper a
moveRightN 0 l = IsZ l
moveRightN x lz@(ListZipper _ _ Nil)
  | x > 0 = IsNotZ
  | x < 0 = moveLeftN ((-1) * x) lz
moveRightN x lz@(ListZipper l m (r:.rs))
  | x > 0 = moveRightN (x - 1) (ListZipper (m:.l) r rs)
  | x < 0 = moveLeftN ((-1) * x) lz
moveRightN _ _ = IsNotZ

-- | Move the focus left the given number of positions. If the value is negative, move right instead.
-- If the focus cannot be moved, the given number of times, return the value by which it can be moved instead.
--
-- >>> moveLeftN' 4 (zipper [3,2,1] 4 [5,6,7])
-- Left 3
--
-- >>> moveLeftN' 1 (zipper [3,2,1] 4 [5,6,7])
-- Right [2,1] >3< [4,5,6,7]
--
-- >>> moveLeftN' 0 (zipper [3,2,1] 4 [5,6,7])
-- Right [3,2,1] >4< [5,6,7]
--
-- >>> moveLeftN' (-2) (zipper [3,2,1] 4 [5,6,7])
-- Right [5,4,3,2,1] >6< [7]
--
-- >>> moveLeftN' (-4) (zipper [3,2,1] 4 [5,6,7])
-- Left 3
--
-- >>> moveLeftN' 4 (zipper [3,2,1] 4 [5,6,7,8,9])
-- Left 3
--
-- >>> moveLeftN' (-4) (zipper [5,4,3,2,1] 6 [7,8,9])
-- Left 3

moveLeftHelper :: (Ord a, Num a) => ListZipper b -> a -> a -> Either a (ListZipper b)
moveLeftHelper lz 0 _ = Right lz
moveLeftHelper lz remaining made =
  case moveLeft lz of
    IsNotZ -> Left made
    IsZ lz' -> moveLeftHelper lz' (remaining - 1) (made + 1)

moveRightHelper :: (Ord a, Num a) => ListZipper b -> a -> a -> Either a (ListZipper b)
moveRightHelper lz 0 _ = Right lz
moveRightHelper lz remaining made =
  case moveRight lz of
    IsNotZ -> Left made
    IsZ lz' -> moveRightHelper lz' (remaining - 1) (made + 1)

moveLeftN' :: Int -> ListZipper a -> Either Int (ListZipper a)
moveLeftN' 0 l = Right l
moveLeftN' x l
  | x > 0 = moveLeftHelper l x 0
  | x < 0 = moveRightN' (x*(-1)) l
moveLeftN' _ _ = Left 0

-- | Move the focus right the given number of positions. If the value is negative, move left instead.
-- If the focus cannot be moved, the given number of times, return the value by which it can be moved instead.
--
-- >>> moveRightN' 4 (zipper [3,2,1] 4 [5,6,7])
-- Left 3
--
-- >>> moveRightN' 1 (zipper [3,2,1] 4 [5,6,7])
-- Right [4,3,2,1] >5< [6,7]
--
-- >>> moveRightN' 0 (zipper [3,2,1] 4 [5,6,7])
-- Right [3,2,1] >4< [5,6,7]
--
-- >>> moveRightN' (-2) (zipper [3,2,1] 4 [5,6,7])
-- Right [1] >2< [3,4,5,6,7]
--
-- >>> moveRightN' (-4) (zipper [3,2,1] 4 [5,6,7])
-- Left 3
moveRightN' :: Int -> ListZipper a -> Either Int (ListZipper a)
moveRightN' 0 l = Right l
moveRightN' x l
  | x > 0 = moveRightHelper l x 0
  | x < 0 = moveLeftN' (x*(-1)) l
moveRightN' _ _ = Left 0

-- | Move the focus to the given absolute position in the zipper. Traverse the zipper only to the extent required.
--
-- >>> nth 1 (zipper [3,2,1] 4 [5,6,7])
-- [1] >2< [3,4,5,6,7]
--
-- >>> nth 5 (zipper [3,2,1] 4 [5,6,7])
-- [5,4,3,2,1] >6< [7]
--
-- >>> nth 8 (zipper [3,2,1] 4 [5,6,7])
-- ><
nth :: Int -> ListZipper a -> MaybeListZipper a
nth idx lz@(ListZipper l _ _) =
  let currentIdx = length l
      moves = idx - currentIdx
  in moveRightN moves lz

-- | Return the absolute position of the current focus in the zipper.
--
-- >>> index (zipper [3,2,1] 4 [5,6,7])
-- 3
--
-- prop> \i z z' -> optional True (\z' -> index z' == i) (toOptional (nth i z))
index :: ListZipper a -> Int
index (ListZipper l _ _) = length l

-- | Move the focus to the end of the zipper.
--
-- >>> end (zipper [3,2,1] 4 [5,6,7])
-- [6,5,4,3,2,1] >7< []
--
-- prop> \lz -> toList lz == toList (end lz)
--
-- prop> \lz -> rights (end lz) == Nil
end :: ListZipper a -> ListZipper a
end lz@(ListZipper _ _ r) =
  case moveRightN (length r) lz of
    IsNotZ -> error ""
    IsZ lz' -> lz'

-- | Move the focus to the start of the zipper.
--
-- >>> start (zipper [3,2,1] 4 [5,6,7])
-- [] >1< [2,3,4,5,6,7]
--
-- prop> \lz -> toList lz == toList (start lz)
--
-- prop> \lz -> lefts (start lz) == Nil
start :: ListZipper a -> ListZipper a
start lz@(ListZipper l _ _) =
  case moveLeftN (length l) lz of
    IsNotZ -> error ""
    IsZ lz' -> lz'

-- | Delete the current focus and pull the left values to take the empty position.
--
-- >>> deletePullLeft (zipper [3,2,1] 4 [5,6,7])
-- [2,1] >3< [5,6,7]
--
-- >>> deletePullLeft (zipper [] 1 [2,3,4])
-- ><
deletePullLeft :: ListZipper a -> MaybeListZipper a
deletePullLeft (ListZipper Nil _ _) = IsNotZ
deletePullLeft (ListZipper (newMid:.l) _ r) = IsZ (ListZipper l newMid r)

-- | Delete the current focus and pull the right values to take the empty position.
--
-- >>> deletePullRight (zipper [3,2,1] 4 [5,6,7])
-- [3,2,1] >5< [6,7]
--
-- >>> deletePullRight (zipper [3,2,1] 4 [])
-- ><
deletePullRight :: ListZipper a -> MaybeListZipper a
deletePullRight (ListZipper _ _ Nil) = IsNotZ
deletePullRight (ListZipper l _ (newMid:.r)) = IsZ (ListZipper l newMid r)

-- | Insert at the current focus and push the left values to make way for the new position.
--
-- >>> insertPushLeft 15 (zipper [3,2,1] 4 [5,6,7])
-- [4,3,2,1] >15< [5,6,7]
--
-- >>> insertPushLeft 15 (zipper [] 1 [2,3,4])
-- [1] >15< [2,3,4]
--
-- prop> \i z -> optional False (==z) (toOptional (deletePullLeft (insertPushLeft i z)))
insertPushLeft :: a -> ListZipper a -> ListZipper a
insertPushLeft newMid (ListZipper l m r) = ListZipper (m:.l) newMid r

-- | Insert at the current focus and push the right values to make way for the new position.
--
-- >>> insertPushRight 15 (zipper [3,2,1] 4 [5,6,7])
-- [3,2,1] >15< [4,5,6,7]
--
-- >>> insertPushRight 15 (zipper [3,2,1] 4 [])
-- [3,2,1] >15< [4]
--
-- prop> \i z -> optional False (==z) (toOptional (deletePullRight (insertPushRight i z)))
insertPushRight :: a -> ListZipper a -> ListZipper a
insertPushRight newMid (ListZipper l m r) = ListZipper l newMid (m:.r)

-- | Implement the `Applicative` instance for `ListZipper`.
-- `pure` produces an infinite list zipper (to both left and right).
-- (<*>) zips functions with values by function application.
--
-- prop> \n -> all . (==) <*> take n . lefts . pure
--
-- prop> \n -> all . (==) <*> take n . rights . pure
--
-- >>> zipper [(+2), (+10)] (*2) [(*3), (4*), (5+)] <*> zipper [3,2,1] 4 [5,6,7]
-- [5,12] >8< [15,24,12]

instance Applicative ListZipper where
  pure :: a -> ListZipper a
  pure a = ListZipper (repeat a) a (repeat a)
  (<*>) :: ListZipper (a -> b) -> ListZipper a -> ListZipper b
  (<*>) (ListZipper fl fm fr) (ListZipper l m r) =
    ListZipper (zipWith (\f a -> f a) fl l)
               (fm m)
               (zipWith (\f a -> f a) fr r)

-- | Implement the `Applicative` instance for `MaybeListZipper`.
--
-- /Tip:/ Use @pure@ for `ListZipper`.
-- /Tip:/ Use `<*>` for `ListZipper`.
--
-- prop> \z n -> let is (IsZ z) = z in all . (==) <*> take n . lefts . is . pure
--
-- prop> \z n -> let is (IsZ z) = z in all . (==) <*> take n . rights . is . pure
--
-- >>> IsZ (zipper [(+2), (+10)] (*2) [(*3), (4*), (5+)]) <*> IsZ (zipper [3,2,1] 4 [5,6,7])
-- [5,12] >8< [15,24,12]
--
-- >>> IsNotZ <*> IsZ (zipper [3,2,1] 4 [5,6,7])
-- ><
--
-- >>> IsZ (zipper [(+2), (+10)] (*2) [(*3), (4*), (5+)]) <*> IsNotZ
-- ><
--
-- >>> IsNotZ <*> IsNotZ
-- ><
instance Applicative MaybeListZipper where
  pure :: a -> MaybeListZipper a
  pure a = IsZ (pure a)
  (<*>) :: MaybeListZipper (a -> b) -> MaybeListZipper a -> MaybeListZipper b
  (<*>) IsNotZ _ = IsNotZ
  (<*>) _ IsNotZ = IsNotZ
  (<*>) (IsZ lzf) (IsZ lza) = IsZ $ lzf <*> lza

-- | Implement the `Extend` instance for `ListZipper`.
-- This implementation "visits" every possible zipper value derivable from a given zipper (i.e. all zippers to the left and right).
--
-- /Tip:/ Use @List#unfoldr@.
-- unfoldr :: (a -> Optional (b, a)) -> a -> List b
--
-- >>> id <<= (zipper [2,1] 3 [4,5])
-- [[1] >2< [3,4,5],[] >1< [2,3,4,5]] >[2,1] >3< [4,5]< [[3,2,1] >4< [5],[4,3,2,1] >5< []]
instance Extend ListZipper where
  (<<=) :: (ListZipper a -> b) -> ListZipper a -> ListZipper b
  (<<=) f lz =
    ListZipper
     (unfoldr (\l -> (\l' -> (f l',l')) <$> (toOptional $ moveLeft l)) lz)
     (f lz)
     (unfoldr (\l -> (\l' -> (f l',l')) <$> (toOptional $ moveRight l)) lz)

-- | Implement the `Extend` instance for `MaybeListZipper`.
-- This instance will use the `Extend` instance for `ListZipper`.
--
--
-- id <<= IsNotZ
-- ><
--
-- >>> id <<= (IsZ (zipper [2,1] 3 [4,5]))
-- [[1] >2< [3,4,5],[] >1< [2,3,4,5]] >[2,1] >3< [4,5]< [[3,2,1] >4< [5],[4,3,2,1] >5< []]
instance Extend MaybeListZipper where
  (<<=) :: (MaybeListZipper a -> b) -> MaybeListZipper a -> MaybeListZipper b
  (<<=) _ IsNotZ = IsNotZ
  (<<=) f (IsZ lz) = IsZ ((\lz' -> f (IsZ lz')) <<= lz)

-- | Implement the `Comonad` instance for `ListZipper`.
-- This implementation returns the current focus of the zipper.
--
-- >>> copure (zipper [2,1] 3 [4,5])
-- 3
instance Comonad ListZipper where
  copure :: ListZipper a -> a
  copure (ListZipper _ m _) = m

-- | Implement the `Traversable` instance for `ListZipper`.
-- This implementation traverses a zipper while running some `Applicative` effect through the zipper.
-- An effectful zipper is returned.
--
-- >>> traverse id (zipper [Full 1, Full 2, Full 3] (Full 4) [Full 5, Full 6, Full 7])
-- Full [1,2,3] >4< [5,6,7]
--
-- >>> traverse id (zipper [Full 1, Full 2, Full 3] (Full 4) [Empty, Full 6, Full 7])
-- Empty
instance Traversable ListZipper where
  traverse :: Applicative f => (a -> f b) -> ListZipper a -> f (ListZipper b)
  traverse a_fb (ListZipper l m r) =
    ListZipper <$> (traverse a_fb l) <*> (a_fb m) <*> (traverse a_fb r)

-- | Implement the `Traversable` instance for `MaybeListZipper`.
--
-- /Tip:/ Use `traverse` for `ListZipper`.
--
-- >>> traverse id IsNotZ
-- ><
--
-- >>> traverse id (IsZ (zipper [Full 1, Full 2, Full 3] (Full 4) [Full 5, Full 6, Full 7]))
-- Full [1,2,3] >4< [5,6,7]
instance Traversable MaybeListZipper where
  traverse :: Applicative f => (a -> f b) -> MaybeListZipper a -> f (MaybeListZipper b)
  traverse _ IsNotZ = pure IsNotZ
  traverse a_fb (IsZ lz) =
    IsZ <$> traverse a_fb lz

instance Show a => Show (ListZipper a) where
  show (ListZipper l x r) =
    stringconcat [show l, " >", show x, "< ", show r]

instance Show a => Show (MaybeListZipper a) where
  show (IsZ z) = show z
  show IsNotZ = "><"
