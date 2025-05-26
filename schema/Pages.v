From Stdlib Require Import String.
From Stdlib Require Import Ascii.
From Stdlib Require Import List.
From Stdlib Require Import Arith.
From Stdlib Require Import NArith.
From Stdlib Require Import ZArith.

Parameter pages_type : Type.

Definition byte := ascii.
Definition bytes := list ascii.

Record table := {
  num_rows : ascii; (*int8*)
  pages : list (option (list ascii)) 
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
Extract Inductive positive => int
[ "(fun p-> let two = Int.add Int.one Int.one in
    Int.add Int.one (Int.mul two p))"
  "(fun p->
    let two = Int.add Int.one Int.one in Int.mul two p)" "Int.one" ]
  "(fun f2p1 f2p f1 p -> let two = Int.add Int.one Int.one in
    if p <= Int.one then f1 () else if Int.rem p two = Int.zero then
    f2p (Int.div p two) else f2p1 (Int.div p two))".

Extract Inductive N => int [ "Int.zero" "" ]
"(fun f0 fp n -> if n=Int.zero then f0 () else fp n)".

Extract Inductive Z => int [ "0" "" "(~-)" ]
"(fun f0 fp fn z -> if z=0 then f0 () else if z>0 then fp z else fn (-z))".
