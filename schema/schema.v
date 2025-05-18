Require Import Proj.Cabs.

Record row := {
  first_name: list Cabs.int
  last_name: list Cabs.int
  age: Cabs.int
}
