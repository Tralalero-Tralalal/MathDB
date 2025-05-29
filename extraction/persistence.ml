open Records
open Regex
open Unix
open Helpers
open Schema

exception Read_error of string
exception Full_error of string

let _id_size = 1
let _name_size = 32
let _id_offset = 0
let _name_offset = _id_size + _id_offset
let _row_size = _id_size + _name_size
let _page_size = 4096
let _table_max_pages = 100
let _rows_per_page = _page_size / _row_size
let _table_max_rows = _rows_per_page * _table_max_pages

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
        let pager = { file_descriptor = fd; file_length = file_length; pages = make_list_of table_max_pages None } in
          pager

let db_open (file_name : string) : table =
  let pager = pager_open file_name in 
    let num_rows = pager.file_length / _row_size in
    { num_rows = (Char.chr num_rows); _pager = pager }

let pager_flush (pager : pager) (page_num : int) (size : int) : pager =
  match Stdlib.List.nth pager.pages page_num with
  | None ->
      prerr_endline "Tried to flush null page";
      exit 1
  | Some page ->
      let offset = page_num * _page_size in
      let _ = Unix.lseek pager.file_descriptor offset Unix.SEEK_SET in
      let written = Unix.write pager.file_descriptor (char_list_to_bytes page _page_size) 0 size in
      if written = -1 then (
        prerr_endline "Error writing page";
        exit 1
      ) else pager

let open_db (filename : string) = 
  let pager = pager_open filename in 
    let num_rows = pager.file_length / _row_size in
          let return_tbl = {
            num_rows = (Char.chr num_rows);
            _pager = pager;
          } in return_tbl


let db_close (table: table) = 
  let pager = table._pager in
  let pages = Stdlib.Array.of_list pager.pages in
    let num_full_pages = (Char.code table.num_rows) / _rows_per_page in 
      for i = 0 to num_full_pages - 1 do
        match pages.(i) with
        | Some page -> pager_flush pager i _page_size;
        pages.(i) <- None
        | None -> ()
      done;
    let num_additional_rows = (Char.code table.num_rows) mod _rows_per_page in
      if (num_additional_rows > 0) then begin
        let page_num = num_full_pages in
        match pages.(page_num) with
        | Some page -> pager_flush pager page_num (num_additional_rows * _row_size); pages.(page_num) <- None
        | None -> ()
      end;
  (try Unix.close pager.file_descriptor
    with Unix.Unix_error (_, _, _) ->
     prerr_endline "Error closing db file";
     exit 1);
  Array.iteri (fun i page_opt ->
    match page_opt with
    | Some _ -> pages.(i) <- None
    | None -> ()
  ) pages


