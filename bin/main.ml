open Stdio
open Definitional_interpreters.First_order_interpreter

let () =
  printf "%s\n"
    (show_value
       (interpret
          (* letrec f = \x. if x = 10 then x else f (succ x) in f 0 *)
          (LetRec
             {
               dvar = "f";
               dexp =
                 {
                   fp = "x";
                   body =
                     Cond
                       {
                         prem =
                           Appl
                             {
                               opr = Appl { opr = Var "equal"; opnd = Var "x" };
                               opnd = Const 10;
                             };
                         conc = Var "x";
                         altr =
                           Appl
                             {
                               opr = Var "f";
                               opnd = Appl { opr = Var "succ"; opnd = Var "x" };
                             };
                       };
                 };
               rbody = Appl { opr = Var "f"; opnd = Const 0 };
             })))
