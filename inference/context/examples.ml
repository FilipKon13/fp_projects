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
(* \x.Cons x [] *)
let e8 = EAbs ("x", EApp (EApp (EVar "Cons", EVar "x"), ELit ListNil))
(* \x.\y.Cons x (Cons y (Cons (x + y) [])) *)
let e9 = EAbs ("x", EAbs ("y", 
  EApp (EApp (EVar "Cons", EVar "x"), 
    EApp (EApp (EVar "Cons", EVar "y"), 
      EApp (EApp (EVar "Cons", EApp (EApp (EVar "Plus", EVar "x"), EVar "y")), ELit ListNil)))))
(* \x.Cons [] (Cons [] []) *)
let e10 = EAbs ("x", EApp (EApp (EVar "Cons", ELit ListNil), EApp (EApp (EVar "Cons", ELit ListNil), ELit ListNil)))
(* \x.Cons (Cons [] []) (Cons (Cons [] []) []) *)
let e11 = EAbs ("x", EApp (EApp (EVar "Cons", EApp (EApp (EVar "Cons", ELit ListNil), ELit ListNil)), EApp (EApp (EVar "Cons", EApp (EApp (EVar "Cons", ELit ListNil), ELit ListNil)), ELit ListNil)))
(* \x.Cons x (Cons [] []) *)
let e12 = EAbs ("x", EApp (EApp (EVar "Cons", EVar "x"), EApp (EApp (EVar "Cons", ELit ListNil), ELit ListNil)))
(* \x.Cons [] x *)
let e13 = EAbs ("x", EApp (EApp (EVar "Cons", ELit ListNil), EVar "x"))
(* \x.let rec l = Cons x l in l *)
let e14 = EAbs ("x", ELetRec ("l", EApp (EApp (EVar "Cons", EVar "x"), EVar "l"), EVar "l"))
(* \a.let rec x = a x in x *)
let e15 = EAbs ("a", ELetRec ("x", EApp (EVar "a", EVar "x"), EVar "x"))
(* let rec f = \x.f in f *)
let e16 = ELetRec ("f", EAbs ("x", EVar "f"), EVar "f")
(* let rec fact = \n. if n == 0 then 1 else n * (fact (n - 1)) in fact 5 *)
let e17 =
  ELetRec ("fact",
    EAbs ("n",
      EIfThenElse (EApp (EApp (EVar "Eq", EVar "n"), ELit (IntLit 0)),
        ELit (IntLit 1),
        EApp (EApp (EVar "Mult", EVar "n"),
          EApp (EVar "fact",
            EApp (EApp (EVar "Minus", EVar "n"), ELit (IntLit 1))
          )
        )
      )
    ),
    EApp (EVar "fact", ELit (IntLit 5))
  )
(* \x. \l. Cons x l *)
let e18 = EAbs ("x", EAbs ("l", EApp (EApp (EVar "Cons", EVar "x"), EVar "l")))
(* let f = (\x. x) in (\y. f y) *)
let e19 = ELet ("f", EAbs ("x", EVar "x"), EAbs ("y", EApp (EVar "f", EVar "y")))
(* \f. f f *)
let e20 = EAbs ("f", EApp (EVar "f", EVar "f"))
(* 2 1 *)
let e21 = EApp (ELit (IntLit 2), ELit (IntLit 1))
(* \b. if b then 1 else [] *)
let e22 = EAbs("b", EIfThenElse (EVar "b", ELit (IntLit 1), ELit ListNil))
(* \f. let _ = f 1 in f true *)
let e23 = EAbs ("f", ELet ("_", EApp (EVar "f", ELit (IntLit 1)), EApp (EVar "f", ELit (BoolLit true))))
(* let l = [] in let _ = Cons 1 l in Cons true l *)
let e24 = ELet ("l", ELit ListNil, ELet ("_", EApp (EApp (EVar "Cons", ELit (IntLit 1)), EVar "l"), EApp (EApp (EVar "Cons", ELit (BoolLit true)), EVar "l")))

let e_arr = [e0; e1; e2; e3; e4; e5; e6; e7; e8; e9; e10; e11; e12; e13; e14; e15; e16; e17; e18; e19; e20; e21; e22; e23; e24]
