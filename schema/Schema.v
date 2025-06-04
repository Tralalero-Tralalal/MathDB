From Stdlib Require Import String.
From Stdlib Require Import Ascii.
From Stdlib Require Import List.
From Stdlib Require Import Arith.
From Stdlib Require Import NArith.
From Stdlib Require Import ZArith.
From Stdlib Require Import ExtrOcamlIntConv.

From SimpleIO Require Import SimpleIO.
From SimpleIO Require Import IO_Unix.

Require Import Schema.Helpers.

Import List.ListNotations.


Definition id_size := 4.
Definition name_size := 32.

Definition id_offset := 0.
Definition name_offset := id_offset + id_size.

Definition row_size := id_size + name_size.

Definition page_size := 4096.
Definition table_max_pages := 100.
Definition rows_per_page := page_size / row_size.
Definition table_max_rows := rows_per_page * table_max_pages.

Definition pages_ := list (option (list ascii)).

Record table := {
  num_rows : ascii;
  pages : pages_
}.

Record row := {
  id : ascii;
  name : string;
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


(*Checks if there is memory for the row to be allocated to
if there isn't, allocate the memory and return the new_pages*)
Definition allocate_memory (page_num : nat) (tbl : table) (row_num : nat) : pages_ :=
  let pages := pages tbl in
    match List.nth_default None pages page_num with
    | Some p => pages
    | None =>
        let new_page := make_list_of page_size zero in
        let new_pages := update_nth pages page_num (Some new_page) in
        new_pages end.

Definition get_page (pages : pages_) (page_num : nat) :=
  let page := List.nth_error pages page_num in
  match page with 
    | Some p => p
    | None => None end.

Definition get_offset (row_num : nat) :=
  let row_offset := row_num mod rows_per_page in
  let byte_offset := row_offset * row_size in
  byte_offset.

Definition memory_alloc_error (msg : string) : option table := None.

Definition execute_insert (tbl : table) (r : row) :=
  (*Gets num of rows in tbl*)
  let num_rows := (nat_of_ascii (num_rows tbl)) in
  (* Gets the page num to insert row*)
  let page_num := num_rows / rows_per_page in
  (*if the num_rows is more than the max rows, return none*)
    if table_max_rows <? num_rows then 
      None else
      (*Serialize rows*)
      let serialized := serialize_row r in
      (* Allocate the memory to the pages if there isn't a place for the insert*)
        let alloc_pages := allocate_memory page_num tbl num_rows in
        (* Get the page that will have the row inserted *)
        let page := get_page alloc_pages page_num in
        match page with
          | Some p =>
          (* Get the location where the row should be inserted *)
          let offset := get_offset num_rows in
          (* Update the page that we got *)
          let updated_page := list_blit p serialized offset in
          (* put the updated page into the pages to make new_pages *)
          let new_pages := update_nth alloc_pages page_num (Some updated_page) in
          Some {|
            num_rows := ascii_of_nat (num_rows + 1);
            pages := new_pages
          |}
          (*If the page is not found, then return an error*)
          | None => memory_alloc_error "failed to alloc memory" end.

(*This prints all rows*)
Fixpoint get_rows (pages : pages_) (ls : list row) (i : nat) : list row :=
  match i with
  | 0 => rev ls
  | S i' =>
    let offset := get_offset i' in
    let page := get_page pages i' in 
    match page with
    | Some p => 
    let row_bytes := list_sub p offset row_size in 
    let row := deserialize_row row_bytes in get_rows pages (ls ++ [row]) i'  
    | None =>
      [] end 
  end.

Definition execute_select (tbl : table) :=
  get_rows (pages tbl) [] (nat_of_ascii (num_rows tbl)).


Definition new_tbl :=
  let emp_pages := make_list_of 100 None in 
  let pre_alloc_page := make_list_of 4069 zero in 
  let new_pages := update_nth emp_pages 0 (Some pre_alloc_page) in
{| num_rows := zero; pages :=  new_pages|}.

