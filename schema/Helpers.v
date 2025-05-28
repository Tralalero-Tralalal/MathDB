From Stdlib Require Import String.
From Stdlib Require Import Ascii.
From Stdlib Require Import List.
From Stdlib Require Import Arith.
From Stdlib Require Import NArith.
From Stdlib Require Import ZArith.
Import List.ListNotations.

Definition get_third {A : Type} {B : Type} {C : Type} (t : A * B * C) : C :=
  match t with
  | (_, _, c) => c
  end.

Definition get_second {A : Type} {B : Type} {C : Type} (t : A * B * C) : B :=
  match t with
  | (_, b, _) => b
  end.

Definition get_first {A : Type} {B : Type} {C : Type} (t : A * B * C) : A :=
  match t with
  | (a, _, _) => a
  end.

Fixpoint map {A : Type} {B : Type} (l : list A) (func : A -> B) : list B :=
    match l with
      | nil => nil
      | cons a t => cons (func a) (map t func)
    end.

Fixpoint mapi_aux {A B : Type} (i : nat) (l : list A) (f : nat -> A -> B) : list B :=
  match l with
  | nil => nil
  | cons a t => cons (f i a) (mapi_aux (S i) t f)
  end.

Definition mapi {A B : Type} (l : list A) (f : nat -> A -> B) : list B :=
  mapi_aux 0 l f.

Definition update_nth {A : Type} (lst : list A) (idx : nat) (new_val : A) : list A :=
  mapi lst (fun n x => if Nat.eqb n idx then new_val else x).

Fixpoint add_n_elems_to_list {A : Type} (n : nat) (a : A) (ls : list A) : list A :=
  match n with
  | 0 => ls
  | S n' => add_n_elems_to_list n' a (ls ++ [a])
  end.

Definition make_list_of {A : Type} (n : nat) (a : A) : list A :=
  add_n_elems_to_list n a [].


Definition make_list (lst : list ascii) (len : nat) : list ascii :=
  let padding_len := Nat.max 0 (len - length lst) in
  let padding := make_list_of padding_len Ascii.zero in 
  lst ++ padding.

Fixpoint list_blit {A} (dst src : list A) (offset : nat) : list A :=
  let fix aux (i : nat) (d : list A) : list A :=
    match d with
    | nil => nil
    | hd :: tl =>
      if andb (Nat.leb offset i) (Nat.ltb i (offset + length src))
      then nth_default hd src (i - offset) :: aux (S i) tl
      else hd :: aux (S i) tl
    end
  in
  aux 0 dst.

Fixpoint list_sub {A} (lst : list A) (offset len : nat) : list A :=
  match lst, offset with
  | _, 0 =>
    (match lst, len with
     | _, 0 => []
     | [], _ => []
     | hd :: tl, len' => hd :: list_sub tl 0 (pred len') end)
  | [], _ => []
  | _ :: tl, n' => list_sub tl (pred n') len
  end.

