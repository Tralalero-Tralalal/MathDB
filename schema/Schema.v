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
From Schema Require Import B_Tree.

Parameter _get_page : pager -> int -> pager * page.

Definition deserialize_row (b : list ascii) : row :=
  {| id := hd zero b; 
    name := string_of_list_ascii (remove_padding (list_sub b name_offset name_size));
    email := string_of_list_ascii (remove_padding (list_sub b email_offset email_size));
  |}.

Definition serialize_row (r : row) : list ascii :=
  let value := add_padding (list_ascii_of_string r.(name)) name_size
    ++ add_padding (list_ascii_of_string r.(email)) email_size in
  r.(id) :: value.

Compute (serialize_row {|
  id := one;
  name := "Ahmad";
  email := "Ahmad@potus.com";
|}
).

Compute (deserialize_row ["001"%char; "A"%char; "h"%char; "m"%char; "a"%char; "d"%char;
        "000"%char; "000"%char; "000"%char; "000"%char; "000"%char;
        "000"%char; "000"%char; "000"%char; "000"%char; "000"%char;
        "000"%char; "000"%char; "000"%char; "000"%char; "000"%char;
        "000"%char; "000"%char; "000"%char; "000"%char; "000"%char;
        "000"%char; "000"%char; "000"%char; "000"%char; "000"%char;
        "000"%char; "000"%char; "A"%char; "h"%char; "m"%char; "a"%char;
        "d"%char; "@"%char; "p"%char; "o"%char; "t"%char; "u"%char; "s"%char;
        "."%char; "c"%char; "o"%char; "m"%char; "000"%char; "000"%char;
        "000"%char; "000"%char; "000"%char; "000"%char; "000"%char;
        "000"%char; "000"%char; "000"%char; "000"%char; "000"%char;
        "000"%char; "000"%char; "000"%char; "000"%char; "000"%char]
).

Theorem serializing_inv : forall (r : row) (s : list ascii), s <> nil -> deserialize_row (serialize_row r) = r 
  /\ serialize_row(deserialize_row s) = s.
Proof.
Admitted.

Definition get_page := _get_page.

Definition cursor_value (cursor : cursor) :=
  let page_num := page_num cursor in
    (*Gets the page, making sure to allocate memory if there isn't any*)
    let (pager, page) := get_page (_pager (_table cursor)) page_num in
    (pager, page, leaf_node_value page (nat_of_int (cell_num cursor))).


Definition table_start (tbl : table) := 
  let root_node := snd (get_page (_pager tbl) (root_page_num tbl)) in
    let num_cells := nat_of_int (leaf_node_num_cells root_node) in
      let end_of_table := num_cells =? 0 in
    {| 
  _table := tbl;
  page_num := root_page_num tbl;
  cell_num := int_of_nat 0; 
  eot := end_of_table
|}.

Definition table_end (tbl : table) :=
  let root_node := snd (get_page (_pager tbl) (root_page_num tbl)) in
    let num_cells := nat_of_int (leaf_node_num_cells root_node) in
      let end_of_table := num_cells =? 0 in
    {|
  _table := tbl;
  page_num := root_page_num tbl;
  cell_num := int_of_nat num_cells;
  eot := true
|}.

Definition cursor_advance (cursor : cursor) :=
  let new_cell_num := nat_of_int (cell_num cursor) + 1 in
    let page_num := page_num cursor in
      let node := get_page (_pager (_table cursor)) page_num in
    if new_cell_num =? nat_of_int (leaf_node_num_cells (snd node)) then 
    {|
      _table := _table cursor;
      page_num := page_num;
      cell_num := int_of_nat new_cell_num;
      eot := true
    |} else 
    {|
      _table := _table cursor;
      page_num := page_num;
      cell_num := int_of_nat new_cell_num;
      eot := false
    |}.  

(* Definition for the Full_error exception (using option type to simulate) *)
Definition Full_error (msg : string) : option table := None.

Fixpoint bump_leaf_node_cell (cells : list ascii) (num_cells : nat) (cell_num : nat) : list ascii :=
  match num_cells with
    | 0 => cells
    | S new_num_cells =>  
  if Nat.ltb num_cells cell_num then
    cells
    else
      let start_dest := Nat.mul LEAF_NODE_CELL_SIZE num_cells in
      let start_src := Nat.mul LEAF_NODE_CELL_SIZE new_num_cells in
      let shifted_cells := list_blit cells (list_sub cells start_src LEAF_NODE_CELL_SIZE) start_dest in
      bump_leaf_node_cell shifted_cells new_num_cells cell_num 
  end.

Definition leaf_node_insert (cursor : cursor) (key : nat) (value : row) :=
  let (pager, node) := get_page (_pager (_table cursor)) (page_num cursor) in
    let num_cells := nat_of_int (leaf_node_num_cells node) in
    (*WIP >= *)
    let cell_num := nat_of_int (cell_num cursor) in
      if num_cells =? LEAF_NODE_MAX_CELLS then
        (pager, None)
      else
      if cell_num <? num_cells then
        (*Bump nodes forward*)
      let bumped_node := bump_leaf_node_cell node num_cells cell_num in
        (*Increment node cells*)
      let inc_node_cells := sub_leaf_node_num_cells bumped_node (num_cells + 1) in
        (* replace key with new key  *)
      let new_node_key := sub_leaf_node_key inc_node_cells cell_num key in
        (*serialize the inputted row into a list of ascii that has a preallocated size
        and put it as a new_node_value*)
      let new_node_value := sub_leaf_node_value new_node_key cell_num (serialize_row value) in
      (*This is the updated page*)
      let updated_page := new_node_value in
      (pager, Some updated_page)
        else
      let inc_node_cells := sub_leaf_node_num_cells node (num_cells + 1) in
        (* replace key with new key  *)
      let new_node_key := sub_leaf_node_key inc_node_cells cell_num key in
        (*serialize the inputted row into a list of ascii that has a preallocated size
        and put it as a new_node_value*)
      let new_node_value := sub_leaf_node_value new_node_key cell_num (serialize_row value) in
      (*This is the updated page*)
      let updated_page := new_node_value in
      (pager, Some updated_page).
        

(* This executes an insert operation*)
Definition execute_insert (tbl : table) (r : row) : option table :=
  let node := snd (get_page (_pager tbl) (root_page_num tbl)) in
  if LEAF_NODE_MAX_CELLS <? nat_of_int (leaf_node_num_cells node) then (*Checks if there are too many rows*)
    Full_error "inflation made me too full"
  else
    let c := table_end tbl in
    (*Get updated page*)
    let (current_pager, updated_page) := leaf_node_insert c (nat_of_ascii (id r)) (r) in
    (*Gets the index of the page *)
    let page_num := page_num c in
      (*the updated page is added to the pages, creating new pages*)
    let new_pages := update_nth (pages current_pager) page_num updated_page in
    (*The updated table is returned*)
    let updated_pager := {| 
      file_descriptor := (file_descriptor current_pager); 
      file_length := (file_length current_pager); 
      pages := new_pages;
      num_pages := int_of_nat (length new_pages)
    |} in 
    let updated_table := {| 
        root_page_num := root_page_num tbl; 
        _pager := updated_pager 
      |} in
    Some updated_table.

(*This prints all rows*)
Fixpoint get_rows (c : cursor) (ls : list row) (i : nat) : list row :=
  if eot c then ls else
  match i with
  | 0 => ls
  | S i' =>
    let full := cursor_value c in (*Find the page to put it in, if there is none make a new one*) 
    let value := get_third full in
    let row_bytes := value in 
    let advanced_cursor := cursor_advance c in
    let row := deserialize_row row_bytes in get_rows advanced_cursor (ls ++ [row]) i'
  end. 

Definition execute_select (tbl : table) :=
  let c := table_start tbl in
  (*Fix this*)
  get_rows c [] (nat_of_int (num_pages (_pager tbl))).

