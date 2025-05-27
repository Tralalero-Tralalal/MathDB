From Stdlib Require Import String.
From Stdlib Require Import Ascii.
From Stdlib Require Import List.
From Stdlib Require Import Arith.
From Stdlib Require Import NArith.
From Stdlib Require Import ZArith.
Import List.ListNotations.

Definition byte := ascii.
Definition _bytes := list ascii.

Definition page := _bytes.
Definition pages_type := list (option page).

Definition id_size := 1.
Definition name_size := 32. (* String can be 32 ascii asciis long *)

Definition id_offset := 0.
Definition username_offset := id_offset + id_size.

Definition row_size := id_size + name_size.

Definition page_size := 4096.
Definition table_max_pages := 100.
Definition rows_per_page := page_size / row_size.
Definition table_max_rows := rows_per_page * table_max_pages.

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

Definition new_tbl :=
  let emp_pages := make_list_of 100 None in 
  let pre_alloc_page := make_list_of 4069 zero in 
  let new_pages := update_nth emp_pages 0 (Some pre_alloc_page) in
{| num_rows := zero; pages :=  new_pages|}.

(*Finds the place in memory to do an operation*)
Definition row_slot (tbl : table) (row_num : nat) : table * page * nat :=
  (*Finds which page it should be at*)
  let page_num := row_num / rows_per_page in
  (*It finds element in pages at index page num*)
  let page_option := nth_default None tbl.(pages) page_num in
  match page_option with
  | Some p => (*If there is some page, it returns it*)
    (*How many rows is the row I'm looking for offset by?*)
    let row_offset := row_num mod rows_per_page in
    (*How many bytes is the byte I'm looking for offset by?*)
    let byte_offset := row_offset * row_size in (tbl, p, byte_offset)
  | None => (*IF there is no page, it creates one with arbitrary bytes*)
    let new_page := repeat Ascii.zero page_size in
    (*How many rows is the row I'm looking for offset by?*)
    let row_offset := row_num mod rows_per_page in
    (*How many bytes is the byte I'm looking for offset by?*)
    let byte_offset := row_offset * row_size in
    (*Adds this to the pages to make new pages*)
    let new_pages := update_nth tbl.(pages) page_num (Some new_page) in
    (*Makes a new table with the pages and returns it along with page*)
    let new_table := {| num_rows := (tbl.(num_rows)); pages := new_pages |} in
    (new_table, new_page, byte_offset)
  end.


Definition get_third (t : table * page * nat) : nat :=
  match t with
  | (_, _, c) => c
  end.

Definition get_second (t : table * page * nat) : page :=
  match t with
  | (_, b, _) => b
  end.

Definition get_first (t : table * page * nat) : table :=
  match t with
  | (a, _, _) => a
  end.

(* Definition for the Full_error exception (using option type to simulate) *)
Definition Full_error (msg : string) : option table := None.

(* This executes an insert operation*)
Definition execute_insert (tbl : table) (r : row) : option table :=
  let num_of_rows := Ascii.nat_of_ascii tbl.(num_rows) in
  if table_max_rows <? num_of_rows then (*Checks if there are too many rows*)
    Full_error "inflation made me too full"
  else
    let serialized := make_list (serialize_row r) row_size in (*serialize the inputted row into a list of ascii that has a preallocated size*)
    let full := row_slot tbl num_of_rows in (*Find the page to put it in, if there is none make a new one*) 
    let current_table := get_first full in 
    let page := get_second full in 
    let offset := get_third full in 
    let updated_page := list_blit page serialized offset in
 (*The new page then replaces the current page, which makes a new list called new_pages*)
    let page_num := num_of_rows / rows_per_page in
    let new_pages := update_nth current_table.(pages) page_num (Some updated_page) in
    (*The updated table is returned*)
    let updated_table := {| num_rows := Ascii.ascii_of_nat (Nat.succ num_of_rows); pages := new_pages |} in
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
  get_rows tbl [] (Ascii.nat_of_ascii (num_rows tbl)).
