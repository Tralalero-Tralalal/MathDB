Require Import ExtrOcamlBasic.
Require Import ExtrOcamlString.
Require Import Schema.Schema.
Require Import Schema.Helpers.
Require Import Schema.Records.
Require Import Parser.Cabs.
Require Import Parser.Lexer.
Require Import Schema.B_Tree.


Extract Constant Schema.get_page => "Extracted._get_page".

Set Extraction Output Directory "./extraction/".

Separate Extraction Cabs.prog Lexer.parse
  (* Schema *)
Schema.serialize_row Schema.deserialize_row Schema.execute_select Schema.execute_insert  

(*Records*)
Records.row Records.table Records.pager
Records.byte Records.page Records.id_size Records.name_size Records.id_offset
Records.name_offset Records.row_size Records.page_size Records.table_max_pages

  (*Helpers*)
Helpers.map Helpers.mapi Helpers.update_nth Helpers.add_n_elems_to_list 
Helpers.make_list_of Helpers.make_list Helpers.list_blit Helpers.list_sub

(*B_Tree*)
B_Tree.initialize_leaf_node.
