{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE TupleSections #-}

module Course.State where

import Course.Core
import qualified Prelude as P
import Course.Optional
import Course.List
import Course.Functor
import Course.Applicative
import Course.Monad
import qualified Data.Set as S
import qualified Course.List as S

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
    runState ::
      s
      -> (a, s)
  }

-- | Run the `State` seeded with `s` and retrieve the resulting state.
--
-- prop> \(Fun _ f) s -> exec (State f) s == snd (runState (State f) s)
exec ::
  State s a
  -> s
  -> s
exec state = snd . runState state

-- | Run the `State` seeded with `s` and retrieve the resulting value.
--
-- prop> \(Fun _ f) s -> eval (State f) s == fst (runState (State f) s)
eval ::
  State s a
  -> s
  -> a
eval state = fst . runState state

-- | A `State` where the state also distributes into the produced value.
--
-- >>> runState get 0
-- (0,0)
get ::
  State s s
get = State (\s -> (s, s))

-- | A `State` where the resulting state is seeded with the given value.
--
-- >>> runState (put 1) 0
-- ((),1)
put ::
  s
  -> State s ()
put s = State (P.const ((), s))

-- | Implement the `Functor` instance for `State s`.
--
-- >>> runState ((+1) <$> State (\s -> (9, s * 2))) 3
-- (10,6)
instance Functor (State s) where
  (<$>) ::
    (a -> b)
    -> State s a
    -> State s b
  (<$>) f s1 = State (\x -> (f $ eval s1 x, exec s1 x))

-- | Implement the `Applicative` instance for `State s`.
--
-- >>> runState (pure 2) 0
-- (2,0)
--
-- >>> runState (pure (+1) <*> pure 0) 0
-- (1,0)
--
-- >>> runState (State (\s -> ((+3), s ++ ("apple":.Nil))) <*> State (\s -> (7, s ++ ("banana":.Nil)))) Nil
-- (10,["apple","banana"])
instance Applicative (State s) where
  pure ::
    a
    -> State s a
  pure x = State (x,)
  (<*>) ::
    State s (a -> b)
    -> State s a
    -> State s b
  (<*>) sf sa = State (\x -> (eval sf x $ eval sa x, exec sa $ exec sf x))

-- | Implement the `Monad` instance for `State s`.
--
-- >>> runState ((const $ put 2) =<< put 1) 0
-- ((),2)
--
-- >>> let modify f = State (\s -> ((), f s)) in runState (modify (+1) >>= \() -> modify (*2)) 7
-- ((),16)
--
-- >>> runState ((\a -> State (\s -> (a + s, 10 + s))) =<< State (\s -> (s * 2, 4 + s))) 2
-- (10,16)
instance Monad (State s) where
  (=<<) ::
    (a -> State s b)
    -> State s a
    -> State s b
  (=<<) f sa = State (\x -> (eval (f $ eval sa x) (exec sa x), exec (f $ eval sa x) (exec sa x)))

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
findM ::
  Monad f =>
  (a -> f Bool)
  -> List a
  -> f (Optional a)
findM _ Nil = pure Empty
findM p (x :. xs) = (\b -> if b then pure (Full x) else findM p xs) =<< p x

-- | Find the first element in a `List` that repeats.
-- It is possible that no element repeats, hence an `Optional` result.
--
-- /Tip:/ Use `findM` and `State` with a @Data.Set#Set@.
--
-- >>> firstRepeat $ 1 :. 2 :. 0 :. 9 :. 2 :. 1 :. Nil
-- Full 2
--
-- prop> \xs -> case firstRepeat xs of Empty -> let xs' = hlist xs in nub xs' == xs'; Full x -> length (filter (== x) xs) > 1
-- prop> \xs -> case firstRepeat xs of Empty -> True; Full x -> let (l, (rx :. rs)) = span (/= x) xs in let (l2, r2) = span (/= x) rs in let l3 = hlist (l ++ (rx :. Nil) ++ l2) in nub l3 == l3
firstRepeat ::
  Ord a =>
  List a
  -> Optional a
firstRepeat list = fst $ runState (findM p list) S.empty
    where p x = (\s -> const (pure (S.member x s)) =<< put (S.insert x s)) =<< get
    -- 更直接的写法是下面这种，不过我还是采取了更Moanidc的方式（因为它看起来更复杂
    -- where p x = State (\s -> (S.member x s, S.insert x s))

-- | Remove all duplicate elements in a `List`.
-- /Tip:/ Use `filtering` and `State` with a @Data.Set#Set@.
--
-- prop> \xs -> firstRepeat (distinct xs) == Empty
--
-- prop> \xs -> distinct xs == distinct (flatMap (\x -> x :. x :. Nil) xs)
-- distinct的结果等于列表第一个元素x，加上从剩余元素xs中去掉x再求distinct的结果
-- 从xs中去掉xs就用了filtering，这种写法其实没有必要用state和Set了，属于强行使用 :)
distinct ::
  Ord a =>
  List a
  -> List a
distinct Nil = Nil
distinct (x:.xs) = x :. distinct (remove x xs)
    where remove x xs = fst $ runState (filtering p xs) (S.insert x S.empty)
          p x = (\s -> const (pure (S.notMember x s)) =<< put (S.insert x s)) =<< get

-- (ﾉ*･ω･)ﾉ
-- **Monad原教旨主义者的写法**（虽然是我写的，但每次回头看这个都不知道该怎么理解）
-- distinct list = fst $ runState (filtering p list) S.empty
--     where p x = (\s -> const (pure (S.notMember x s)) =<< put (S.insert x s)) =<< get

-- <(ˉ^ˉ)>
-- **正常人的写法** （好好写不要装逼）
-- distinct list = fst $ runState (filtering p list) S.empty
--     where p x = State (\s -> (S.null s, S.insert x s))

-- w(ﾟДﾟ)w
-- 注意以上两种实现均不正确，根据自己对上面State的<*>的实现，利用定义展开，这里filtering相当于把每个State用<*>进行组合，A <*> B <*> C <*> D ....  
-- 因为<*>是左结合的，A <*> B <*> C <*> D 等价于 ((A <*> B) <*> C) <*> D, 所以输入的s被分别传递给了((A <*> B) <*> C) 和 D
-- 在((A <*> B) <*> C)中s又被分别传递给了(A <*> B)和C，最后的效果相当于s被分别传递给了ABCD，所以这里的S.empty实际被分别作用在了filering中的每个State上
-- 向Set里插入元素其实是无效的，最后的效果是S.notMember x s中的s永远是空集，起不到任何判断的作用。


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
isHappy ::
  Integer
  -> Bool
isHappy x = contains 1 (firstRepeat (produce sumOfDigitSquare x))

sumOfDigitSquare ::
  Integer
  -> Integer
sumOfDigitSquare x = toInteger (sum (map square $ digitSeq x))
  where square = join (*)
        digitSeq = map digitToInt . show'

-- 注意digitToInt 和sum 都产生的是Int 所以需要用toInteger转换为Integer
-- 由于使用了我们自己实现的List, show要用show'