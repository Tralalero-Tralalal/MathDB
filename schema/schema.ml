exception Full_error of string

open Pages

let id_size = 4 
let name_size = 32
let email_size = 255

let id_offset = 0
let username_offset = id_offset + id_size
let email_offset = username_offset + name_size

let row_size = id_size + name_size + email_size

let page_size =  4096
let table_max_pages = 100
let rows_per_page = page_size / row_size
let table_max_rows = rows_per_page * table_max_pages

let serialize_row (r : Pages.row) : bytes =
  let buffer = Bytes.create row_size in
  let (Pages.ROW (id, name)) = r in
  Bytes.set_int8 buffer 0 id;
  let write_fixed_string s offset size =
    let padded = String.sub (s ^ String.make size '\000') 0 size in
    Bytes.blit_string padded 0 buffer offset size
  in
  write_fixed_string name id_size name_size;
  buffer

let deserialize_row (b : bytes) : row =
  let read_fixed_string offset size =
    let raw = Bytes.sub_string b offset size in
    try Stdlib.String.sub raw 0 (Stdlib.String.index raw '\000') with Not_found -> raw
  in
  let id = Bytes.get_int8 b 0 in
  let name = read_fixed_string id_size name_size in
  Pages.ROW (id, name)  

let row_slot (tbl : table) (row_num : int) : bytes * int =
  let (TABLE (num_rows, pages)) = tbl in
  let page_num = row_num / rows_per_page in
  let page =
    match pages.(page_num) with
    | Some p -> p
    | None ->
        let new_page = Bytes.make page_size '\000' in
        pages.(page_num) <- Some new_page;
        new_page
  in
  let row_offset = row_num mod rows_per_page in
  let byte_offset = row_offset * row_size in
  (page, byte_offset)

let execute_insert (tbl : table) (r : row) =
  let (TABLE (num_rows, pages)) = tbl in
  if num_rows >= table_max_rows then begin
    print_int num_rows;
    print_int table_max_rows;
    raise (Full_error "skibidi"); 
    end
  else
    let serialized = serialize_row r in
    let page, offset =
      match row_slot tbl num_rows with
      | p, o -> (p, o)
    in
    Bytes.blit serialized 0 page offset row_size;
    let updated_table = TABLE (num_rows + 1, pages) in
    updated_table


let execute_select (tbl : table)  =
  let (TABLE (num_rows, _pages)) = tbl in
  for i = 0 to num_rows - 1 do
    let (page, offset) = row_slot tbl i in
    let row_bytes = Bytes.sub page offset row_size in
    let Pages.ROW (id, name) = deserialize_row row_bytes in
    Printf.printf "(%d, %s)\n" id name
  done;
