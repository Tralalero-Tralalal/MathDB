Require Import Proj.Cabs.
Require Import ExtrOcamlBasic.
Require Import ExtrOcamlString.

Extract Constant Cabs.loc => "Lexing.position".
Extract Constant Cabs.integer => "int".
Extract Constant Cabs.floater => "float".
Extract Constant Cabs.str => "string".

Set Extraction Output Directory "./extraction/".

Separate Extraction Cabs.prog.
