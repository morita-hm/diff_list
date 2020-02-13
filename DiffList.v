
Require Import List.
Open Scope list_scope.

Section BooleanValuedFunction.
  
  Fixpoint elem {A : Type} (eqb : A -> A -> bool) (l : list A) (a : A) :=
    match l with
    | nil => false
    | x :: xs => if eqb x a then true else elem eqb xs a
    end.

  Definition not_elem {A : Type} (eqb : A -> A -> bool) (l : list A) (a : A) :=
    negb (elem eqb l a).

  Compute elem Nat.eqb (1 :: 2 :: 3 :: nil) 1.
  Compute elem Nat.eqb (1 :: 2 :: 3 :: nil) 5.

  Fixpoint rcons {A : Type} (l : list A) (a : A) :=
    match l with
    | nil => a :: nil
    | x :: xs => x :: rcons xs a
    end.

  (* l1 ∪ l2 *)
  Fixpoint union {A : Type} (eqb : A -> A -> bool) (l1 l2 : list A) :=
    match l2 with
    | x :: xs => if elem eqb l1 x then union eqb l1 xs else union eqb (rcons l1 x) xs
    | nil => l1
    end.

  Compute union Nat.eqb (1 :: 2 :: 3 :: 4 :: nil) (1 :: 2 :: 4 :: 8 :: nil).

  (* l1 \ l2 *)
  Fixpoint sub {A : Type} (eqb : A -> A -> bool) (l1 l2 : list A) :=
    match l1 with
    | x :: xs => if elem eqb l2 x then sub eqb xs l2 else x :: sub eqb xs l2
    | nil => nil
    end.

  Goal forall (A : Type) (a : A) (la lb : list A) (eqb : A -> A -> bool),
      elem eqb lb a = false -> sub eqb (a :: la) lb = a :: sub eqb la lb.
  Proof.
    intros A a la lb eqb H.
    unfold sub.
    now rewrite H.
  Qed.
  
  Compute sub Nat.eqb (1 :: 2 :: 3 :: 4 :: 5 :: 6 :: nil) (2 :: 4 :: 5 :: nil).
End BooleanValuedFunction.
  
Require Import Extraction.
Extraction Language Haskell.
Extract Inductive list => "([])" ["[]" "(:)"].
Extract Inductive bool => "Prelude.Bool" ["Prelude.True" "Prelude.False"].
Extraction "difflist.hs" union sub.
