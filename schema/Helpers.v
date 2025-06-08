From Stdlib Require Import String.
From Stdlib Require Import Ascii.
From Stdlib Require Import List.
From Stdlib Require Import Arith.
From Stdlib Require Import NArith.
From Stdlib Require Import ZArith.
From Stdlib Require Import ExtrOcamlIntConv.
Import List.ListNotations.

Definition A : Type.
Admitted.
(* General list manipulation*)
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

Lemma update_not_eq : forall (lst : list A) (a : A) (b : A),
  update_nth (a :: lst) 0 b =
  b :: lst.
Proof.
  intros. induction lst; unfold update_nth; unfold mapi; unfold mapi_aux. simpl. reflexivity.
  simpl. Admitted.

Fixpoint add_n_elems_to_list {A : Type} (n : nat) (a : A) (ls : list A) : list A :=
  match n with
  | 0 => ls
  | S n' => add_n_elems_to_list n' a (ls ++ [a])
  end.

Lemma add_to_nil {A : Type} : forall (n : nat) (a : A),
  List.length (add_n_elems_to_list n a []) = n.
Proof.
  intros. induction n.
  -  reflexivity.
  - rewrite <- IHn. Admitted.

      (* If ls' has at least n' elements, then (x :: ls') has at least S n' elements *)
Lemma add_n_elems_len {A : Type} : forall (n : nat) (a : A) (ls : list A),
  List.length (add_n_elems_to_list n a ls) = (List.length ls) + n.
Proof.
  intros. induction n, ls. 
  + reflexivity.
  + simpl. rewrite Nat.add_0_r. reflexivity. 
  + simpl in IHn. assert (S n = S (Datatypes.length (add_n_elems_to_list n a []))).
    -  rewrite IHn. reflexivity.  
    - simpl. rewrite H. unfold length, add_n_elems_to_list. Admitted.


Definition make_list_of {A : Type} (n : nat) (a : A) : list A :=
  add_n_elems_to_list n a [].
(*General list manipulation*)
(*Theorems for General list manipulation*)

Lemma make_list_of_len {A : Type} : forall (n : nat) (a : A), 
  List.length (make_list_of n a) = n.
Proof.
  intros. induction n. 
  - reflexivity.
  - unfold make_list_of.  unfold add_n_elems_to_list. Admitted.

Lemma make_list_of_0_len {A : Type} : forall (a : A), 
  List.length (make_list_of 0 a) = 0.
Proof. reflexivity. Qed.

Lemma make_list_of_0 {A : Type} : forall (a : A), 
  make_list_of 0 a = [].
Proof. reflexivity. Qed. 
(*Theorems for General list manipulation*)

(* Helpers for serializing and deserializing*)
(*This adds empty bytes*)
Definition add_padding (lst : list ascii) (len : nat) : list ascii :=
  let padding_len := Nat.max 0 (len - length lst) in
  let padding := make_list_of padding_len zero in 
  lst ++ padding.

Lemma add_0_padding : forall (lst : list ascii),
  add_padding lst 0 = lst.
Proof.
  intros. induction lst. reflexivity. unfold add_padding. 
  rewrite make_list_of_0. simpl. rewrite app_nil_r. reflexivity.
Qed.

Lemma add_n_padding_len : forall (lst : list ascii) (n : nat),
  length (add_padding lst n) = n.
Proof.
  intros.
  induction lst, n.  
  - reflexivity.
  - unfold add_padding. unfold make_list_of. simpl.
Admitted.

(*This removes all empty bytes, 
it should not interact with memory that is a single byte long*)
Fixpoint _remove_padding (lst : list ascii) : list ascii :=
  match lst with 
   | l :: rest => if eqb l zero then _remove_padding rest else lst
   | nil => nil end.

Lemma _remove_padding_len : forall (lst : list ascii),
  length (_remove_padding lst) <= length lst.
Proof.
  intros. induction lst. simpl. reflexivity.
  destruct a; destruct b, b0, b1, b2, b3, b4, b5, b6; 
  try reflexivity. simpl. Search (_ <= S _).
  rewrite IHlst. apply Nat.le_succ_diag_r.
Qed.

Lemma _remove_padding_same : forall (lst : list ascii),
  ~ (_remove_padding [(hd zero lst)] = nil) -> _remove_padding lst = lst.
Proof.
  intros. destruct lst. reflexivity.
  destruct a; destruct b, b0, b1, b2, b3, b4, b5, b6; try reflexivity.
  simpl in H. destruct H. reflexivity.
Qed.

Lemma _remove_padding_rev : forall (lst : list ascii) (a : ascii),
  ~ (_remove_padding [(last (lst ++ [a]) zero)] = nil) -> 
  _remove_padding (rev (lst ++ [a])) = rev (lst ++ [a]).
Proof.
  intros. destruct a; 
  destruct b, b0, b1, b2, b3, b4, b5, b6; rewrite rev_unit; 
  try reflexivity; rewrite last_last in H; simpl in H; 
  destruct H; reflexivity.
Qed.

Definition remove_padding (lst : list ascii) : list ascii :=
  rev (_remove_padding (rev lst)).

Lemma remove_padding_s : forall (lst : list ascii) (a : ascii), 
 ~ (_remove_padding [(last (lst ++ [a]) zero)] = nil)  ->
 remove_padding (lst ++ [a]) = (lst ++ [a]).
Proof.
  intros. unfold remove_padding.
  destruct a;  destruct b, b0, b1, b2, b3, b4, b5, b6; 
  rewrite _remove_padding_rev; try (rewrite rev_involutive; reflexivity);
  try (apply H).
Qed. 
(* Helpers for serializing and deserializing*)

(* Manipulating the bytes, will replace this if I use Hoare logic*)
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
  | _, 0 => match lst, len with
            | _, 0 => []
            | [], _ => []
            | hd :: tl, len' => hd :: list_sub tl 0 (pred len') end
  | [], _ => []
  | _ :: tl, n' => list_sub tl (pred n') len
  end.


Lemma sub_0 {A : Type} : forall (offset : nat) (lst : list A),
  list_sub lst offset 0 = [].
Proof.
  intros. induction lst, offset. 
  - reflexivity. 
  - reflexivity.
  - reflexivity.
  - rewrite <- IHlst. simpl. Admitted.


Fixpoint is_list_of (lst : list ascii) (obj : ascii) : bool :=
  match lst with
  | f :: rest => if Ascii.eqb f obj then is_list_of rest obj else false
  | nil => true end.

(* Manipulating the bytes, might replace this if I use Hoare logic*)
