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
  let id32 = Int32.of_int r.id in
  Bytes.set_int32_le buffer 0 id32;
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
  let id = Int32.to_int (Bytes.get_int32_le b 0) in
  let name = (Regex.string_to_char_list (read_fixed_string id_size name_size)) in
    let r = {
      id = id;
      name = name;
    } in r

let get_page (pager : Pages.pager) (page_num : int) =
  if page_num >= table_max_pages then begin
    raise (Full_error "inflation made me too full"); 
    end
    else match pager.pages.(page_num) with
          | Some page -> page  
          | None ->
      (* Cache miss *)
      let page = Bytes.make page_size '\000' in
      let num_pages = (pager.file_length + page_size - 1) / page_size in
      if page_num < num_pages then begin
        (* Seek and read *)
        let offset = page_num * page_size in
        let _ = Unix.lseek pager.file_descriptor offset Unix.SEEK_SET in
        let bytes_read = Unix.read pager.file_descriptor page 0 page_size in
        if bytes_read < 0 then failwith "Error reading file"
      end;
      (* Cache it *)
      pager.pages.(page_num) <- Some page;
      page


let row_slot (tbl : table) (row_num : int) : bytes * int =
  let page_num = row_num / rows_per_page in
  let page = get_page tbl._pager page_num in
  let row_offset = row_num mod rows_per_page in
  let byte_offset = row_offset * row_size in
  (page, byte_offset)

let execute_insert (tbl : table) (r : row) =
  if tbl.num_rows >= table_max_rows then begin
    raise (Full_error "inflation made me too full"); 
    end
  else
    let serialized = serialize_row r in
    let page, offset =
      match row_slot tbl tbl.num_rows with
      | p, o -> (p, o)
    in
    Bytes.blit serialized 0 page offset row_size;
    let updated_table =  {
      num_rows = tbl.num_rows + 1;
      _pager = tbl._pager
    } in
    updated_table


let execute_select (tbl : table)  =
  for i = 0 to tbl.num_rows - 1 do
    let (page, offset) = row_slot tbl i in
    let row_bytes = Bytes.sub page offset row_size in
    let row = deserialize_row row_bytes in
    Printf.printf "(%d, %s)\n" row.id (Regex.char_list_to_string row.name)
  done;
