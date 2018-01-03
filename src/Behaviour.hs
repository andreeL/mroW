module Behaviour
  ( Behaviour(..)
  , bScan
  , bScanSplit
  , bEval
  ) where

import Control.Applicative (Applicative(..))
import qualified Control.Category as C (Category(..))
import Control.Arrow (Arrow(..))

newtype Behaviour i o = Behaviour { getBehaviour :: i -> (o, Behaviour i o) }

instance Functor (Behaviour i) where
  fmap f b = Behaviour $ \i ->
    let (o, b') = getBehaviour b i
     in (f o, fmap f b')

instance Applicative (Behaviour i) where
  pure o =  Behaviour $ const (o, pure o)
  b1 <*> b2 = Behaviour $ \i ->
    let (o1, b1') = getBehaviour b1 i
        (o2, b2') = getBehaviour b2 i
     in (o1 o2, b1' <*> b2')

instance C.Category Behaviour where
  id = Behaviour $ \i -> (i, C.id)
  bc . ab = Behaviour $ \i ->
    let (b, ab') = getBehaviour ab i
        (o, bc') = getBehaviour bc b
     in (o, bc' C.. ab')

instance Arrow Behaviour where
  arr f = Behaviour $ \i -> (f i, arr f)
  first b = Behaviour $ \(i1, i2) ->
    let (o, b') = getBehaviour b i1
     in ((o, i2), first b')

bScan :: (o -> i -> o) -> o -> Behaviour i o
bScan f acc = Behaviour $ \i ->
  let next = f acc i
   in (next, bScan f next)

bScanSplit :: (a -> i -> (o, a)) -> a -> Behaviour i o
bScanSplit f acc = Behaviour $ \i ->
  let (o, acc') = f acc i
   in (o, bScanSplit f acc')
   
bEval :: Behaviour i o -> [i] -> [o]
bEval _ [] = []
bEval b (x:xs) = let (o, b') = getBehaviour b x
                  in o:bEval b' xs