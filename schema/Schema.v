From Stdlib Require Import String.
From Stdlib Require Import Ascii.
From Stdlib Require Import List.
From Stdlib Require Import Arith.
From Stdlib Require Import NArith.
From Stdlib Require Import ZArith.
From Stdlib Require Import ExtrOcamlIntConv.

From SimpleIO Require Import SimpleIO.
From SimpleIO Require Import IO_Unix.

Import List.ListNotations.

From Schema Require Import Helpers.
From Schema Require Import Records.

Definition byte := ascii.
Definition _bytes := list ascii.

Definition page := _bytes.

Definition id_size := 1.

Definition name_size := 32. (* String can be 32 ascii asciis long *)

Definition id_offset := 0.

Definition name_offset := id_offset + id_size.

Definition row_size := id_size + name_size.

Definition page_size := 4096.

Definition table_max_pages := 100.

Definition rows_per_page := page_size / row_size.

Definition table_max_rows := rows_per_page * table_max_pages.

Parameter _get_page : pager -> int -> pager * page.

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

Definition get_page := _get_page.

(*Finds the place in memory to do an operation*)
Definition row_slot (tbl : table) (row_num : nat) : pager * page * nat :=
  (*Finds which page it should be at*)
  let page_num := row_num / rows_per_page in
    (*How many rows is the row I'm looking for offset by?*)
    let row_offset := row_num mod rows_per_page in
    (*How many bytes is the byte I'm looking for offset by?*)
    let byte_offset := row_offset * row_size in
    let pager_and_page := get_page (_pager tbl) (int_of_nat page_num) in
    (fst pager_and_page, snd pager_and_page, byte_offset).

(* Definition for the Full_error exception (using option type to simulate) *)
Definition Full_error (msg : string) : option table := None.

(* This executes an insert operation*)
Definition execute_insert (tbl : table) (r : row) : option table :=
  let num_of_rows := nat_of_int tbl.(num_rows) in
  if table_max_rows <? num_of_rows then (*Checks if there are too many rows*)
    Full_error "inflation made me too full"
  else
    let serialized := make_list (serialize_row r) row_size in (*serialize the inputted row into a list of ascii that has a preallocated size*)
    let full := row_slot tbl num_of_rows in (*Find the page to put it in, if there is none make a new one*) 
    let current_pager := get_first full in 
    let page := get_second full in 
    let offset := get_third full in 
    let updated_page := list_blit page serialized offset in
 (*The new page then replaces the current page, which makes a new list called new_pages*)
    let page_num := num_of_rows / rows_per_page in
    let new_pages := update_nth (pages current_pager) (int_of_nat page_num) (Some updated_page) in
    (*The updated table is returned*)
    let updated_pager := {| file_descriptor := (file_descriptor current_pager); 
      file_length := (file_length current_pager); pages := new_pages|} in 
    let updated_table := 
      {| num_rows := int_of_nat (Nat.succ num_of_rows); _pager := updated_pager |} in
    Some updated_table.

(*This prints all rows*)
Fixpoint get_rows (tbl : table) (ls : list row) (i : nat) : list row :=
  match i with
  | 0 => rev ls
  | S i' =>
    let full := row_slot tbl i' in (*Find the page to put it in, if there is none make a new one*) 
    let page := get_second full in 
    let offset := get_third full in
    let row_bytes := list_sub page offset row_size in 
    let row := deserialize_row row_bytes in get_rows tbl (ls ++ [row]) i' end. 

Definition execute_select (tbl : table) :=
  get_rows tbl [] (nat_of_int (num_rows tbl)).


