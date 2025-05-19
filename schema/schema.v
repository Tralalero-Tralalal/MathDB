Require Import Proj.Cabs.

Definition Vec (A : Type) : nat -> Type :=
  fix vec n := match n return Type with
               | O   => unit
               | S n => prod A (vec n)
               end.

(*
Record row := {
  first_name: list Cabs.integer
  last_name: list Cabs.integer
  age: Cabs.integer
}.


Definition table : list row := nil. 

Definition create_row (f_name : list Cabs.integer) (l_name : list Cabs.integer) (a : Cabs.integer) : row := {|
  first_name := f_name;
  last_name := l_name;
  age := a
|}.

Definition add_row_to_table (r : row) := (r :: table)
*)
