open Types

(* let id = (λx. x) in id *)
let e0 = ELet ("id", (EAbs ("x", (EVar "x"))), (EVar "id"))
(* let id = (λx. x) in (id id) *)
let e1 = ELet ("id", (EAbs ("x", (EVar "x"))), (EApp ((EVar "id"), (EVar "id"))))
(* let id = (λx. (let y = x in y)) in (id id) *)
let e2 = ELet ("id", (EAbs ("x", (ELet ("y", (EVar "x"), (EVar "y"))))), (EApp ((EVar "id"), (EVar "id"))))
(* let id = (λx. (x x)) in id *)
let e3 = ELet ("id", (EAbs ("x", (EApp ((EVar "x"), (EVar "x"))))), (EVar "id"))
(* λf. (λg. (λx. (f (g x)))) *)
let e4 = EAbs ("f", EAbs ("g", EAbs ("x", EApp (EVar "f", EApp (EVar "g", EVar "x")))))
(* \f.\x.(let x = f x in f x) *)
let e5 = EAbs ("f", EAbs ("x", ELet ("x", EApp (EVar "f", EVar "x"), EApp (EVar "f", EVar "x"))))
(* \f.\x.(let x = f x in x) *)
let e6 = EAbs ("f", EAbs ("x", ELet ("x", EApp (EVar "f", EVar "x"), EVar "x")))
(* \f.\x.(let y = f x in f y) *)
let e7 = EAbs ("f", EAbs ("x", ELet ("y", EApp (EVar "f", EVar "x"), EApp (EVar "f", EVar "y"))))

let e_arr = [e0; e1; e2; e3; e4; e5; e6; e7]
