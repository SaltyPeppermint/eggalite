
let foo n =
  let r = n*2 in
  r+1

let () = print_endline ("Hello, World!" ^ Int.to_string (foo 4))
