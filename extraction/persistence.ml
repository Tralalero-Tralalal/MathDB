open Records
open Regex
open Unix
open Helpers
open Schema
open Extracted
open B_Tree

exception Read_error of string
exception Full_error of string
exception Corrupt_error of string

(* Sizes *)
let _id_size = 1
let _name_size = 32
let _email_size = 32
(* Sizes *)

(* Offset *)
let _id_offset = 0
let _name_offset = _id_size + _id_offset
let _email_offset = _name_offset + _name_size
(* Offset *)

let _row_size = _id_size + _name_size + _email_size
let _page_size = 4096
let _table_max_pages = 100

let char_list_to_bytes (clist : char list) (byte_size : int) : bytes =
  let b = Bytes.make byte_size '\000' in  (* Initialize with null bytes *)
  let rec fill i = function
    | [] -> ()
    | c :: cs when i < byte_size ->
        Bytes.set b i c;
        fill (i + 1) cs
  | _ -> () in fill 0 clist; b


let pager_open (file_name : string) : pager =
  let fd = Unix.openfile file_name [Unix.O_RDWR; Unix.O_CREAT] 0o600 in
    if fd = stderr then raise (Read_error "failed to open") else
      let file_length = Unix.lseek fd 0 Unix.SEEK_END in
        if file_length mod _page_size <> 0 then 
          raise (Corrupt_error "file is corrupt") else
      let new_pages = make_list_of table_max_pages None in
        let pager = { 
      file_descriptor = fd; 
      file_length = file_length; 
      pages = new_pages;
      num_pages = Stdlib.List.length new_pages 
    } in
          pager

let pager_flush (pager : pager) (page_num : int) =
  match Stdlib.List.nth pager.pages page_num with
  | None ->
      prerr_endline "Tried to flush null page";
      exit 1
  | Some page ->
      let offset = page_num * _page_size in
      let _ = Unix.lseek pager.file_descriptor offset Unix.SEEK_SET in
      let written = Unix.write pager.file_descriptor (char_list_to_bytes page _page_size) 0 _page_size in
      if written = -1 then (
        prerr_endline "Error writing page";
        exit 1
      ) else ()

let open_db (filename : string) = 
  let pager = pager_open filename in 
    if pager.num_pages = 0 then 
      let (_, root_node) = _get_page pager 0 in
        let fresh_page = B_Tree.initialize_leaf_node root_node in
          let new_pager = {
            file_descriptor = pager.file_descriptor;
            file_length = pager.file_length;
            pages = [Some fresh_page];
            num_pages = 1;
          } in
          let return_tbl = {
            root_page_num = 0;
            _pager = new_pager;
          } in return_tbl else 
          let return_tbl = {
            root_page_num = 0;
            _pager = pager;
          } in return_tbl

let db_close (table: table) = 
  let pager = table._pager in
  let pages = Stdlib.Array.of_list pager.pages in
      for i = 0 to pager.num_pages - 1 do
        match pages.(i) with
        | Some page -> pager_flush pager i;
        pages.(i) <- None
        | None -> ()
      done;
  (try Unix.close pager.file_descriptor
    with Unix.Unix_error (_, _, _) ->
     prerr_endline "Error closing db file";
     exit 1);
  Array.iteri (fun i page_opt ->
    match page_opt with
    | Some _ -> pages.(i) <- None
    | None -> ()
  ) pages


