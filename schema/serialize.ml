open Cabs
open Uchar

let serialize_uchar (x : Uchar.t array) : int array = Array.map Uchar.to_int arr

let deserialize_uchar (i : int array) : Uchar.t array = Array.map Uchar.of_int arr 

