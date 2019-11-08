let twa x =
  Printf.printf "add          %s\n" (Twa.to_string (Twa.add (Twa.of_float x) (Twa.of_float x)));
  Printf.printf "sub          %s\n" (Twa.to_string (Twa.sub (Twa.of_float x) (Twa.of_float x)));
  Printf.printf "mul          %s\n" (Twa.to_string (Twa.mul (Twa.of_float x) (Twa.of_float x)));
  Printf.printf "mul_fast     %s\n" (Twa.to_string (Twa.mul_fast (Twa.of_float x) (Twa.of_float x)));
  Printf.printf "mul_dwa      %s\n" (Twa.to_string (Twa.mul_dwa (Twa.of_float x) (Dwa.of_float x)));
  Printf.printf "mul_dwa_fast %s\n" (Twa.to_string (Twa.mul_dwa (Twa.of_float x) (Dwa.of_float x)));
  Printf.printf "reciprocal   %s\n" (Twa.to_string (Twa.reciprocal (Twa.of_float x)));
  Printf.printf "div          %s\n" (Twa.to_string (Twa.div (Twa.of_float x) (Twa.of_float x)));
  Printf.printf "sqrt         %s\n" (Twa.to_string (Twa.sqrt (Twa.of_float x)))

let _ =
  List.iter (fun x ->
      Printf.printf "#TWA tests, x = %h\n" x;
      twa x;
      Printf.printf "\n") [1.; 2.; 4.; 8.; ~-. 1.; ~-. 2.; ~-. 4.; ~-. 8.]
