Require Import Proj.Parser.
Require Import Proj.Cabs.
Require Import ExtrOcamlBasic.
Require Import ExtrOcamlString.

Extract Constant Cabs.loc => "Lexing.position".
Extract Constant Cabs.uchars => "Uchar.t array".
Extract Constant Cabs.integer => "int".
Extract Constant Cabs.floater => "float".

Set Extraction Output Directory "./extraction/".

Separate Extraction program.

