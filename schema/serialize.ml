open Uchar

module RowLayout = struct
  let id_size = 4
  let username_size = 32
  let email_size = 255

  let id_offset = 0
  let username_offset = id_offset + id_size
  let email_offset = username_offset + username_size

  let row_size = id_size + username_size + email_size
end

module TableLayout = struct
  let page_size = 100
  let rows_per_page = page_size / RowLayout.row_size
end

type table = {
  num_rows : int;
  pages : page option array;
}

type row = {
  id : int;  (* assuming 4-byte int *)
  username : Uchar.t array;  
  email : Uchar.t array;
}

let serialize_row (r : row) : bytes =
  let buf = Bytes.create row_size in
  Bytes.set_int32_le buf id_offset (Int32.of_int r.id);
  Array.iteri (fun i c -> Bytes.set buf (username_offset + i) (Char.chr (Uchar.to_int c))) r.username;
  Array.iteri (fun i c -> Bytes.set buf (email_offset + i) (Char.chr (Uchar.to_int c))) r.email;
  buf

let deserialize_row (buf : bytes) : row =
  let id = Int32.to_int (Bytes.get_int32_le buf id_offset) in
  let username = Array.init username_size (fun i ->
    Uchar.of_int (Char.code (Bytes.get buf (username_offset + i)))
  ) in
  let email = Array.init email_size (fun i ->
    Uchar.of_int (Char.code (Bytes.get buf (email_offset + i)))
  ) in
  { id; username; email }

let row_slot (tbl : table) (page_num : int) : page =
  match tbl.pages.(page_num) with
  | Some p -> p
  | None ->
      let new_page = Bytes.create page_size in
      tbl.pages.(page_num) <- Some new_page;
      new_page

let execute_insert()
