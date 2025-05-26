exception Full_error of string

open Pages

let id_size = 1 
let name_size = 32 (*String can be 32 ascii chars long*)

let id_offset = 0
let username_offset = id_offset + id_size

let row_size = id_size + name_size

let page_size =  4096
let table_max_pages = 100
let rows_per_page = page_size / row_size
let table_max_rows = rows_per_page * table_max_pages

let char_list_to_bytes (clist : char list) (byte_size : int) : bytes =
  let b = Bytes.make byte_size '\000' in  (* Initialize with null bytes *)
  let rec fill i = function
    | [] -> ()
    | c :: cs when i < byte_size ->
        Bytes.set b i c;
        fill (i + 1) cs
    | _ -> ()  
  in
  fill 0 clist; b

let bytes_to_char_list (b : Bytes.t) : char list =
  let len = Bytes.length b in
  let rec aux i acc =
    if i < 0 then acc
    else aux (i - 1) (Bytes.get b i :: acc)
  in
  aux (len - 1) []

let print_char_list clist =
  clist |> List.iter (fun c -> print_char c);
  print_newline ()

let update_nth lst (idx : int) (new_val : char list option) =
   List.mapi (fun i x -> if i = idx then new_val else x) lst

let swap_nth (lst : 'a list) (n : int) (x : 'a) : 'a list =
  let rec aux i = function
    | [] -> []
    | _ :: tl when i = n -> x :: tl
    | hd :: tl -> hd :: aux (i + 1) tl
  in
  aux 0 lst

let row_slot (tbl : table) (row_num : int) : table * char list * int =
  let page_num = row_num / rows_per_page in
  let page =
  match Stdlib.List.nth tbl.pages page_num with
    | Some p ->  p
    | None ->
      let new_page = List.init 4096 (fun _ -> '\000') in new_page in
      let row_offset = row_num mod rows_per_page in
        let byte_offset = row_offset * row_size in
if List.for_all (fun c -> c = '\000') page then begin
  let new_pages = update_nth tbl.pages page_num (Some page) in
  let new_table = {
    num_rows = tbl.num_rows;
    pages = new_pages
  } in (new_table, page, byte_offset)
end
else (tbl, page, byte_offset)

let execute_insert (tbl : table) (r : row) =
  let page_num = (Char.code tbl.num_rows) / rows_per_page in
  if Char.code tbl.num_rows >= table_max_rows then begin
    raise (Full_error "inflation made me too full"); 
    end
  else
    let serialized = char_list_to_bytes (serialize_row r) 33 in
    Printf.printf "serialized data: %s\n" (Bytes.to_string serialized);
    let table, page, offset =
      match row_slot tbl (Char.code tbl.num_rows) with
      | t, p, o -> (t, p, o)
    in
let bytes_page = char_list_to_bytes page (Stdlib.List.length page) in
Printf.printf "Serialized length: %d\n" (Bytes.length serialized);
Printf.printf "Page length: %d, Offset: %d, Row size: %d\n" (Bytes.length bytes_page) offset row_size;
    Bytes.blit serialized 0 bytes_page offset row_size;
    let new_pages = update_nth table.pages page_num (Some (bytes_to_char_list bytes_page)) in
    Printf.printf "bytes pages: %s\n" (Bytes.to_string bytes_page);
    let updated_table =  {
      num_rows = Char.chr ((Char.code tbl.num_rows) + 1);
      pages = new_pages
    } in
    updated_table

let execute_select (tbl : table)  =
  for i = 0 to (Char.code tbl.num_rows) - 1 do
    let (new_tbl, page, offset) = row_slot tbl i in
    let bytes_page = char_list_to_bytes page (Stdlib.List.length page) in
    let row_bytes = bytes_to_char_list (Bytes.sub bytes_page offset row_size) in
    let row = deserialize_row row_bytes in
    Printf.printf "(%d, %s)\n" (Char.code row.id) (Regex.char_list_to_string row.name)
  done;
