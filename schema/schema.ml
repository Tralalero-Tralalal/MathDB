exception Full_error of string

open Pages

let id_size = 1 
let name_size = 32 (*String can be 32 ascii chars long*)

let id_offset = 0
let username_offset = id_offset + id_size
let email_offset = username_offset + name_size

let row_size = id_size + name_size

let page_size =  4096
let table_max_pages = 100
let rows_per_page = page_size / row_size
let table_max_rows = rows_per_page * table_max_pages

let serialize_row (r : Pages.row) : bytes =
  let buffer = Bytes.create row_size in
  let id = Char.code r.id in
  Bytes.set_int8 buffer 0 id;
  let write_fixed_string s offset size =
    let padded = Stdlib.String.sub (s ^ Stdlib.String.make size '\000') 0 size in
    Bytes.blit_string padded 0 buffer offset size;
  in
  write_fixed_string (Regex.char_list_to_string r.name) id_size name_size;
  buffer

let deserialize_row (b : bytes) : row =
  let read_fixed_string offset size =
    let raw = Bytes.sub_string b offset size in
    try Stdlib.String.sub raw 0 (Stdlib.String.index raw '\000') with Not_found -> raw
  in
  let id = Bytes.get_int8 b 0 in
  let name = (Regex.string_to_char_list (read_fixed_string id_size name_size)) in
    let r = {
      id = Char.chr id;
      name = name;
    } in 
r

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
    let serialized = serialize_row r in
    let page, offset =
      match row_slot tbl (Char.code tbl.num_rows) with
      | p, o -> (p, o)
    in
    Bytes.blit serialized 0 page offset row_size;
    let updated_table =  {
      num_rows = Char.chr ((Char.code tbl.num_rows) + 1);
      pages = tbl.pages
    } in
    updated_table


let execute_select (tbl : table)  =
  for i = 0 to (Char.code tbl.num_rows) - 1 do
    let (page, offset) = row_slot tbl i in
    let row_bytes = Bytes.sub page offset row_size in
    let row = deserialize_row row_bytes in
    Printf.printf "(%d, %s)\n" (Char.code row.id) (Regex.char_list_to_string row.name)
  done;
