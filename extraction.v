Require Import ExtrOcamlBasic.
Require Import ExtrOcamlString.
Require Import Schema.Pages.
Require Import Parser.Cabs.
Require Import Parser.Lexer.

Set Extraction Output Directory "./extraction/".

Separate Extraction Cabs.prog Pages.row Pages.table Lexer.parse.
