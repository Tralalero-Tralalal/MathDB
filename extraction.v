Require Import ExtrOcamlBasic.
Require Import ExtrOcamlString.
Require Import Schema.Schema.
Require Import Schema.Helpers.
Require Import Parser.Cabs.
Require Import Parser.Lexer.


Set Extraction Output Directory "./extraction/".

Separate Extraction Cabs.prog Schema.row Schema.table Lexer.parse Schema.serialize_row Schema.deserialize_row Schema.execute_select Schema.execute_insert
Schema.new_tbl Helpers.map Helpers.mapi Helpers.update_nth Helpers.add_n_elems_to_list Helpers.make_list_of Helpers.make_list Helpers.list_blit
Helpers.list_sub.
