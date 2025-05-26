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
  fill 0 clist;
  b

let bytes_to_char_list (b : Bytes.t) : char list =
  let len = Bytes.length b in
  let rec aux i acc =
    if i < 0 then acc
    else aux (i - 1) (Bytes.get b i :: acc)
  in
  aux (len - 1) []

let row_slot (tbl : table) (row_num : int) : bytes * int =
  let page_num = row_num / rows_per_page in
  let page =
    match tbl.pages.(page_num) with
    | Some p -> p
    | None ->
        let new_page = Bytes.make page_size '\000' in
        tbl.pages.(page_num) <- Some new_page;
        new_page
  in
  let row_offset = row_num mod rows_per_page in
  let byte_offset = row_offset * row_size in
  (page, byte_offset)

let execute_insert (tbl : table) (r : row) =
  if Char.code tbl.num_rows >= table_max_rows then begin
    raise (Full_error "inflation made me too full"); 
    end
  else
    let serialized = char_list_to_bytes (serialize_row r) 33 in
    let page, offset =
      match row_slot tbl (Char.code tbl.num_rows) with
      | p, o -> (p, o)
    in
Printf.printf "Serialized length: %d\n" (Bytes.length serialized);
Printf.printf "Page length: %d, Offset: %d, Row size: %d\n" (Bytes.length page) offset row_size;
  assert (Bytes.length page >= offset + row_size);
  assert (Bytes.length serialized >= row_size);
    Bytes.blit serialized 0 page offset row_size;
    let updated_table =  {
      num_rows = Char.chr ((Char.code tbl.num_rows) + 1);
      pages = tbl.pages
    } in
    updated_table


let execute_select (tbl : table)  =
  for i = 0 to (Char.code tbl.num_rows) - 1 do
    let (page, offset) = row_slot tbl i in
    let row_bytes = bytes_to_char_list (Bytes.sub page offset row_size) in
    let row = deserialize_row row_bytes in
    Printf.printf "(%d, %s)\n" (Char.code row.id) (Regex.char_list_to_string row.name)
  done;
