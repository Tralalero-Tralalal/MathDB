open Records
open Regex
open Unix
open Helpers
open Bytes

exception Full_error of string

let max_pages = 100
let _page_size = 4096

let bytes_to_char_list (b : Bytes.t) : char list =
  let len = Bytes.length b in
  let rec aux i acc =
    if i < 0 then acc
    else aux (i - 1) (Bytes.get b i :: acc) in aux (len - 1) []

let _get_page (pager : pager) (page_num : int) : pager * char list =
  let pages = Stdlib.Array.of_list pager.pages in
  if page_num >= max_pages then begin
    raise (Full_error "inflation made me too full"); 
    end
    else match pages.(page_num) with
          | Some page -> (pager, page)  
          | None ->
      (* Cache miss *)
      let page = Bytes.make _page_size '\000' in
      let num_pages =
        if pager.file_length mod _page_size <> 0 then
          pager.file_length / _page_size + 1
        else pager.file_length / _page_size in
      if page_num <= num_pages then begin
        (* Seek and read *)
        let offset = page_num * _page_size in
        let _ = Unix.lseek pager.file_descriptor offset Unix.SEEK_SET in
        let bytes_read = Unix.read pager.file_descriptor page 0 _page_size in
        if bytes_read = -1 then failwith "Error reading file"
      end;
      (* Cache it *)
      let new_page = bytes_to_char_list page in
      let new_pages = update_nth (Stdlib.Array.to_list pages) page_num (Some new_page) in
      let new_pager = { file_descriptor = pager.file_descriptor; file_length = pager.file_length; pages = new_pages} in 
      (new_pager, new_page)
