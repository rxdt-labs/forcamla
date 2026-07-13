open Alcotest

let test_term_extract () =
  let open Formula in
  let x = t 1 in
  (check int) "same int" 1 !x

let test_term_extract_float () =
  let open Formula in
  let x = t 1.0 in
  (check (float 0.01)) "same float" 1.0 !x

let test_form_extract () =
  let open Formula in
  let x = t 1 in
  let y = x + (t 1) in
  (check int) "same int" 2 !y

let test_form_extract_float () =
  let open Formula in
  let x = t 1.0 in
  let y = x +. (t 1.0) in
  (check (float 0.01)) "same int" 2.0 !y

let test_update_term () =
  let open Formula in
  let x = t 1 in
  x =: 3;
  (check int) "same int" 3 !x

let test_update_term_float () =
  let open Formula in
  let x = t 1.0 in
  x =: 3.0;
  (check (float 0.01)) "same int" 3.0 !x

let test_update_form () =
  let open Formula in
  let x = t 1 in
  let y = x + (t 1) in
  x =: 3;
  (check int) "same int" 4 !y

let test_update_form_float () =
  let open Formula in
  let x = t 1.0 in
  let y = x +. (t 1.0) in
  x =: 3.0;
  (check (float 0.01)) "same int" 4.0 !y

let test_complex_expr () =
  let open Formula in
  let x = t 3 in
  let z = x * x in
  (check int) "same int" 9 !z

let test_complex_expr_float () = 
  let open Formula in
  let x = t 3.0 in
  let z = x *. x in
  (check (float 0.01)) "same int" 9.0 !z

let test_term_form_expr () = 
  let open Formula in
  let x = t 3 in
  let y = x + (t 1) in
  let w = x * y in
  (check int) "same int" 12 !w

let test_term_form_expr_float () = 
  let open Formula in
  let x = t 3.0 in
  let y = x +. (t 1.0) in
  let w = x *. y in
  (check (float 0.01)) "same int" 12.0 !w

let test_complex_update () = 
  let open Formula in
  let x = t 3 in
  let z = x * x in
  x =: 2;
  (check int) "same int" 4 !z

let test_complex_update_float () = 
  let open Formula in
  let x = t 3.0 in
  let z = x *. x in
  x =: 2.0;
  (check (float 0.01)) "same int" 4.0 !z

let test_term_form_expr () = 
  let open Formula in
  let x = t 3 in
  let y = x + (t 1) in
  let w = x * y in
  x =: 2;
  (check int) "same int" 6 !w

let test_term_form_expr_float () = 
  let open Formula in
  let x = t 3.0 in
  let y = x +. (t 1.0) in
  let w = x *. y in
  x =: 2.0;
  (check (float 0.01)) "same int" 6.0 !w

let test_simple_inc () =
  let open Formula in
  let x = t 2 in
  x =: !(x + t 1);
  (check int) "same int" 3 !x

let test_complex_update () =
  let open Formula in
  let x = t 2 in
  let z = x * x in
  x =: !(x + t 1); 
  (check int) "same int" 9 !z

let test_term_form_update () =
  let open Formula in
  let x = t 2 in
  let y = x + (t 1) in
  let w = x * y in
  x =: !(x + t 1); 
  (check int) "same int" 12 !w

let test_simple_eq_no_change () =
  let open Formula in
  let x = t 1 in
  let y = x =? (t 0) in
  (check bool) "same bool" false !!y

let test_simple_eq () =
  let open Formula in
  let x = t 1 in
  let y = x =? (t 0) in
  x =: !(x - t 1);
  (check bool) "same bool" true !!y
  
let test_simple_sat () =
  let open Formula in
  let x = t 1 in
  let y = x =? (t 0) in
  let z = ref 1 in
  when_satisfied y (fun () : unit -> (z := 2));
  x =: !(x - t 1);
  (check int) "same int" 2 z.contents


let test_player_health_go () =
  let x = ref "In Play" in
  let g () = x := "Game Over" in
  let open Formula in
  let health = t 3 in
  when_satisfied (health =? t 0) g;
  health =: !(health - t 1);
  health =: !(health - t 1);
  health =: !(health - t 1);
  (check string) "same string" "Game Over" x.contents

let test_player_health_ip () =
  let x = ref "In Play" in
  let g () = x := "Game Over" in
  let open Formula in
  let health = t 3 in
  when_satisfied (health =? t 0) g;
  health =: !(health - t 1);
  health =: !(health - t 1);
  (check string) "same string" "In Play" x.contents

let test_source_simple_while () =
  let x = ref 0 in
  let g () = x := (!x + 1) in
  let open Formula in
  let s = make_source () in
  let y = t 0 in
  let test = (y >=? t 3) in
  exec_while s test g;
  listen s;
  y =: !(y + t 1);
  listen s;
  y =: !(y + t 1);
  listen s;
  y =: !(y + t 1);
  listen s;
  y =: !(y + t 1);
  listen s;
  (check int) "same int" 2 x.contents

let test_source_simple_always () =
  let x = ref true in
  let g (b: bool) = x := b in
  let open Formula in
  let s = make_source () in
  let y = t 0 in
  let test = (y >=? t 1) in
  exec_always s test g;
  (check bool) "same bool" true x.contents;
  listen s;
  (check bool) "same bool" false x.contents;
  y =: !(y + t 1);
  (check bool) "same bool" false x.contents;
  listen s;
  (check bool) "same bool" true x.contents

let () =
  run "Utils" [
      "simple", [
        test_case "Assign term and extract" `Quick test_term_extract;
        test_case "Assign term and extract (float)" `Quick test_term_extract_float;
        test_case "Assign formula and extract" `Quick test_form_extract;
        test_case "Assign formula and extract (float)" `Quick test_form_extract_float;
      ];
      "reassign-term", [
        test_case "Reassign term" `Quick test_update_term;
        test_case "Reassign term (float)" `Quick test_update_term_float;
        test_case "Update formula" `Quick test_update_form;
        test_case "Update formula (float)" `Quick test_update_form_float;
      ];
      "complex-formula", [
        test_case "Complex formula" `Quick test_complex_expr;
        test_case "Complex formula (float)" `Quick test_complex_expr_float;
        test_case "Formula + term formula" `Quick test_term_form_expr;
        test_case "Formula + term formula (float)" `Quick test_term_form_expr_float;
        test_case "Complex update" `Quick test_complex_update;
        test_case "Formula + term update" `Quick test_term_form_update;
      ];
      "simple-equations", [
        test_case "Simple equation (no update)" `Quick test_simple_eq_no_change;
        test_case "Simple equation" `Quick test_simple_eq;
      ];
      "event-listeners", [
        test_case "Simple when_satisfied" `Quick test_simple_sat;
        test_case "Player health test (Game Over)" `Quick test_player_health_go;
        test_case "Player health test (In Play)" `Quick test_player_health_ip;
      ];
      "sources", [
        test_case "Simple source test for exec_while" `Quick test_source_simple_while;
        test_case "Simple source test for exec_always" `Quick test_source_simple_always;
      ];
  ]
