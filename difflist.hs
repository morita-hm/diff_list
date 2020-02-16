module Difflist where

import qualified Prelude

elem :: (a1 -> a1 -> Prelude.Bool) -> (([]) a1) -> a1 -> Prelude.Bool
elem eqb l a =
  case l of {
   [] -> Prelude.False;
   (:) x xs ->
    case eqb x a of {
     Prelude.True -> Prelude.True;
     Prelude.False -> elem eqb xs a}}

rcons :: (([]) a1) -> a1 -> ([]) a1
rcons l a =
  case l of {
   [] -> (:) a [];
   (:) x xs -> (:) x (rcons xs a)}

union :: (a1 -> a1 -> Prelude.Bool) -> (([]) a1) -> (([]) a1) -> ([]) a1
union eqb l1 l2 =
  case l2 of {
   [] -> l1;
   (:) x xs ->
    case elem eqb l1 x of {
     Prelude.True -> union eqb l1 xs;
     Prelude.False -> union eqb (rcons l1 x) xs}}

intersection :: (a1 -> a1 -> Prelude.Bool) -> (([]) a1) -> (([]) a1) -> ([])
                a1
intersection eqb l1 l2 =
  case l1 of {
   [] -> [];
   (:) x xs ->
    case elem eqb l2 x of {
     Prelude.True -> (:) x (intersection eqb xs l2);
     Prelude.False -> intersection eqb xs l2}}

sub :: (a1 -> a1 -> Prelude.Bool) -> (([]) a1) -> (([]) a1) -> ([]) a1
sub eqb l1 l2 =
  case l1 of {
   [] -> [];
   (:) x xs ->
    case elem eqb l2 x of {
     Prelude.True -> sub eqb xs l2;
     Prelude.False -> (:) x (sub eqb xs l2)}}

