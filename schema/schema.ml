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

let serialize_row_into (r : row) (buffer : bytes) (offset : int) : unit =
  let id32 = Int32.of_int r.id in
  Bytes.set_int32_le buffer offset id32;
  let write_fixed_string s off size =
    let padded = Stdlib.String.sub (s ^ Stdlib.String.make size '\000') 0 size in
    Bytes.blit_string padded 0 buffer (offset + off) size
  in
  write_fixed_string (Regex.char_list_to_string r.name) id_size name_size

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


let cursor_end (tbl : table) =
  {
    _table = tbl;
    row_num = tbl.num_rows;
    end_of_table = true;
  }

let cursor_start (tbl : table) =
  {
    _table = tbl;
    row_num = 0;
    end_of_table = (tbl.num_rows = 0);  
  }

let cursor_value (c : Pages.cursor) : bytes * int =
  let r = c.row_num in 
  let page_num = r / rows_per_page in
  let page = get_page c._table._pager page_num in
  let row_offset = r mod rows_per_page in
  let byte_offset = row_offset * row_size in
  (page, byte_offset)

let execute_insert (tbl : table) (r : row) =
  let c = cursor_end tbl in
  if tbl.num_rows >= table_max_rows then
    raise (Full_error "inflation made me too full")
  else
    let page, offset =
      match cursor_value c with
      | p, o -> (p, o)
    in
    serialize_row_into r page offset;
    {
      num_rows = tbl.num_rows + 1;
      _pager = tbl._pager
    }

let cursor_advance (c : cursor) =
  let inc_row_num = c.row_num + 1 in
  let eot = inc_row_num >= c._table.num_rows in
  {
    _table = c._table;
    row_num = inc_row_num;
    end_of_table = eot;
  }

let rec execute_select (c : cursor) =
  if c.end_of_table == false then begin 
    let (page, offset) = cursor_value c in
    let row_bytes = Bytes.sub page offset row_size in
    let row = deserialize_row row_bytes in
    Printf.printf "(%d, %s)\n" row.id (Regex.char_list_to_string row.name);
    execute_select (cursor_advance c);
    end;

