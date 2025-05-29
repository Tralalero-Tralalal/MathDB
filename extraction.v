Require Import ExtrOcamlBasic.
Require Import ExtrOcamlString.
Require Import Schema.Schema.
Require Import Schema.Helpers.
Require Import Schema.Records.
Require Import Parser.Cabs.
Require Import Parser.Lexer.


Extract Constant Schema.get_page => "Get_page._get_page".

Set Extraction Output Directory "./extraction/".

Separate Extraction Cabs.prog Lexer.parse
  (* Schema *)
Schema.serialize_row Schema.deserialize_row Schema.execute_select Schema.execute_insert  
Schema.byte Schema.page Schema.table_max_rows Schema.id_size Schema.name_size Schema.id_offset
Schema.name_offset Schema.row_size Schema.page_size Schema.table_max_pages Schema.rows_per_page 

(*Records*)
Records.row Records.table Records.pager

  (*Helpers*)
Helpers.map Helpers.mapi Helpers.update_nth Helpers.add_n_elems_to_list 
Helpers.make_list_of Helpers.make_list Helpers.list_blit Helpers.list_sub.
