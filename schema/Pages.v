From Stdlib Require Import String.
From Stdlib Require Import Ascii.
From Stdlib Require Import List.

Definition id_size := 1.
Definition name_size := 32. (*Cans be 32 ascii*)

Parameter pages_type : Type.

Definition byte := ascii.
Definition bytes := list ascii.

Record table := {
  num_rows : ascii; (*int8*)
  pages : pages_type
}.

Record row := {
  id : ascii; (*int8*)
  name : string
}.

Definition deserialize_row (b : list ascii) : row :=
  match b with
  | id :: name => {| id := id; name := string_of_list_ascii name |}
  | nil => {| id := "0"%char; name := "" |}  (* or handle differently if needed *)
  end.

Definition serialize_row (r : row) : list ascii :=
  r.(id) :: list_ascii_of_string r.(name).

Theorem serializing_inv : forall (r : row) (s : list ascii), s <> nil -> deserialize_row (serialize_row r) = r 
  /\ serialize_row(deserialize_row s) = s.
Proof.
split. simpl. rewrite string_of_list_ascii_of_string. destruct r. simpl. reflexivity.
induction s. exfalso. apply H. reflexivity. unfold serialize_row. simpl. rewrite list_ascii_of_string_of_list_ascii.
reflexivity.
Qed.

Require Import ExtrOcamlBasic.
Require Import ExtrOcamlString.
Extract Constant pages_type => "Bytes.t option array".
