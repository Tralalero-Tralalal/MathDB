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

(*Finds the place in memory to do an operation*)
let row_slot (tbl : table) (row_num : int) : table * char list * int =
  (*Finds which page it should be at*)
  let page_num = row_num / rows_per_page in
  (*It finds element in pages at index page num*)
  let page =
  match Stdlib.List.nth tbl.pages page_num with
    | Some p ->  p (*If there is some page, it returns bytes*)
    | None -> (*IF there is no page, it creates one with arbritrary bytes*)
      let new_page = List.init 4096 (fun _ -> '\000') in new_page in
      (*How many rows is the row I'm looking for offset by?*)
      let row_offset = row_num mod rows_per_page in
        (*How many bytes is the byte I'm looking for offset by?*)
        let byte_offset = row_offset * row_size in
          (*Checks if the page is new*)
          if Stdlib.List.nth tbl.pages page_num = None then begin
            (*Adds this to the pages to make new pages*)
            let new_pages = update_nth tbl.pages page_num (Some page) in
              (*Makes a new table with the pages and returns it along with page*)
              let new_table = {
                num_rows = tbl.num_rows;
                pages = new_pages
              } in (new_table, page, byte_offset)
              end
                (*if it is not full of arbritrary bytes, then why just return the normal table*)
                else (tbl, page, byte_offset)

(* This executes an insert operation*)
let execute_insert (tbl : table) (r : row) =
  let num_of_rows = (Char.code tbl.num_rows) in 
    let page_num = num_of_rows / rows_per_page in (*Which page will it go into*)
      if num_of_rows >= table_max_rows then begin (*Checks if there are too many rows*)
        raise (Full_error "inflation made me too full"); 
      end
        else
          let serialized = char_list_to_bytes (serialize_row r) 33 in (*serialize the inputted row into bytes*)
            let table, page, offset =
              match row_slot tbl num_of_rows with (*Find the page to put it in, if there is none make a new one*)
                | t, p, o -> (t, p, o) in
                let bytes_page = char_list_to_bytes page (Stdlib.List.length page) in (*turn page into bytes*)
                  Bytes.blit serialized 0 bytes_page offset row_size; (*add serliazed data to page, in bytes*)
                    (*The new page then replaces the current page, which makes a new list called new_pages*)
                    let new_pages = update_nth table.pages page_num (Some (bytes_to_char_list bytes_page)) in
                    (*The updated table is returned*)
                      let updated_table =  {
                        num_rows = Char.chr (num_of_rows + 1);
                        pages = new_pages
                      } in updated_table

(*This prints all rows*)
let execute_select (tbl : table)  =
  for i = 0 to (Char.code tbl.num_rows) - 1 do (*Iterates over all rows*)
    let (_, page, offset) = row_slot tbl i in (*grabs row of index i in the tbl*)
      let bytes_page = char_list_to_bytes page (Stdlib.List.length page) in (*turns page into bytes*)
        (*returns a row from the page starting from offset and encompassing row size*)
        let row_bytes = bytes_to_char_list (Bytes.sub bytes_page offset row_size) in 
          let row = deserialize_row row_bytes in
            Printf.printf "(%d, %s)\n" (Char.code row.id) (Regex.char_list_to_string row.name)
  done;
