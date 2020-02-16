module Difflist2 where

import qualified Prelude

orb :: Prelude.Bool -> Prelude.Bool -> Prelude.Bool
orb b1 b2 =
  case b1 of {
   Prelude.True -> Prelude.True;
   Prelude.False -> b2}

negb :: Prelude.Bool -> Prelude.Bool
negb b =
  case b of {
   Prelude.True -> Prelude.False;
   Prelude.False -> Prelude.True}

map :: (a1 -> a2) -> (([]) a1) -> ([]) a2
map f l =
  case l of {
   [] -> [];
   (:) a t -> (:) (f a) (map f t)}

fold_left :: (a1 -> a2 -> a1) -> (([]) a2) -> a1 -> a1
fold_left f l a0 =
  case l of {
   [] -> a0;
   (:) b t -> fold_left f t (f a0 b)}

filter :: (a1 -> Prelude.Bool) -> (([]) a1) -> ([]) a1
filter f l =
  case l of {
   [] -> [];
   (:) x l0 ->
    case f x of {
     Prelude.True -> (:) x (filter f l0);
     Prelude.False -> filter f l0}}

elem :: (a1 -> a1 -> Prelude.Bool) -> (([]) a1) -> a1 -> Prelude.Bool
elem eqb l a =
  fold_left orb (map (eqb a) l) Prelude.False

intersection :: (a1 -> a1 -> Prelude.Bool) -> (([]) a1) -> (([]) a1) -> ([])
                a1
intersection eqb l1 l2 =
  filter (elem eqb l2) l1

not_elem :: (a1 -> a1 -> Prelude.Bool) -> (([]) a1) -> a1 -> Prelude.Bool
not_elem eqb l a =
  negb (elem eqb l a)

sub :: (a1 -> a1 -> Prelude.Bool) -> (([]) a1) -> (([]) a1) -> ([]) a1
sub eqb l1 l2 =
  filter (not_elem eqb l2) l1

