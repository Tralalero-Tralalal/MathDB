Require Import ExtrOcamlBasic.
Require Import ExtrOcamlString.
Require Import Proj.Pages.
Require Import Proj.Cabs.
Require Import Proj.Lexer.

Set Extraction Output Directory "./extraction/".

Separate Extraction Cabs.prog Pages.row Pages.table Pages.pager Pages.cursor Lexer.parse.
