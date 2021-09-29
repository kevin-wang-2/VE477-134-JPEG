let minus a b next = next (b - a);;
let add a b next = next (a + b);;
let divide a b next = next (b / a);;
let multiply a b next = next (a * b);;

let get b = b;