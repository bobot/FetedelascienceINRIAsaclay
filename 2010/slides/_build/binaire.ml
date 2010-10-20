let f = open_in Sys.argv.(1)
let bit b = if b = 0 then 0 else 1
let print_byte b = 
  Printf.printf "%d%d%d%d%d%d%d%d"
    (bit (b land 0b10000000))
    (bit (b land 0b01000000))
    (bit (b land 0b00100000))
    (bit (b land 0b00010000))
    (bit (b land 0b00001000))
    (bit (b land 0b00000100))
    (bit (b land 0b00000010))
    (bit (b land 0b00000001))

let rec read () = 
  try 
    print_byte (input_byte f);
    read ()
  with _ -> ()

let _= read ()
