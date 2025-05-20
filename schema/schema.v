Require Import Proj.Cabs.

Definition Vec (A : Type) : nat -> Type :=
  fix vec n := match n return Type with
               | O   => unit
               | S n => prod A (vec n)
               end.

Definition id_size := 4.
Definition username_size := 32.
Definition email_size := 255.

Definition id_offset := 0.
Definition username_offset := id_offset + id_size.
Definition email_offset := username_offset + username_size.

Definition row_size := id_size + username_size + email_size.
