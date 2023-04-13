module Composite.Opaleye.RecToFields where

import Composite.Record (pattern (:*:), Rec((:&), RNil), (:->)(Val))
import Data.Functor.Identity (Identity(Identity))
import Data.Kind (Type)
import Data.Profunctor.Product.Default (Default)
import Opaleye (ToFields, toFields)

class RecToFields (haskells :: [Type]) (fields :: [Type]) where
  recToFields :: Rec Identity haskells -> Rec Identity fields

instance RecToFields '[] '[] where
  recToFields RNil = RNil

instance (Default ToFields h f, RecToFields haskells fields) => RecToFields (s :-> h ': haskells) (s :-> f ': fields) where
  recToFields hs' = let Identity (Val h) :& hs = hs' in toFields h :*: recToFields hs
