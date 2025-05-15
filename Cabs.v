From Stdlib Require Import String.
From Stdlib Require Import Ascii.

Parameter loc : Type.

Parameter str : Type.

Parameter char_code : Type.

Inductive prog :=
  | TOKEN : keyword -> prog

with keyword :=
  | SELECT : loc -> keyword.

Require Import ExtrOcamlBasic.
Require Import ExtrOcamlString.

Extract Constant loc => "Lexing.position".
Extract Constant str => "string".
Extract Constant char_code => "int64".
