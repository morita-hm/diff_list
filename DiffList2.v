
Require Import List.
Open Scope list_scope.

Section BooleanValuedFunction.
  
  (* a ∈ l *)
  Definition elem {A : Type} (eqb : A -> A -> bool) (l : list A) (a : A) :=
    fold_left orb (map (eqb a) l) false.

  Goal forall (l : list bool), fold_left orb (true :: l) false = true.
  Proof.
    induction l.
    - reflexivity.
    - simpl in IHl.
      simpl.
      now rewrite IHl.
  Qed.

  Check filter.
  (* l1 ∩ l2 *)
  Fixpoint intersection {A : Type} (eqb : A -> A -> bool) (l1 l2 : list A) :=
     filter (elem eqb l2) l1.
  
  Compute intersection Nat.eqb (1 :: 2 :: 3 :: 4 :: 5 :: 6 :: nil) (3 :: 6 :: 9 :: nil).

  Definition not_elem {A : Type} (eqb : A -> A -> bool) (l : list A) (a : A) :=
    negb (elem eqb l a).

  (* l1 \ l2 *)
  Fixpoint sub {A : Type} (eqb : A -> A -> bool) (l1 l2 : list A) :=
    filter (not_elem eqb l2) l1.
  
  Compute sub Nat.eqb (1 :: 2 :: 3 :: 4 :: 5 :: 6 :: nil) (2 :: 4 :: 5 :: nil).
End BooleanValuedFunction.
  
Require Import Extraction.
Extraction Language Haskell.
Extract Inductive list => "([])" ["[]" "(:)"].
Extract Inductive bool => "Prelude.Bool" ["Prelude.True" "Prelude.False"].
Extraction "difflist2.hs" intersection sub.
