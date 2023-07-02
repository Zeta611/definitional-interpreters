open Stdio
open Definitional_interpreters.Continuation_passing_escape_interpreter

let () =
  printf "%s\n"
    (show_value
       (interpret
          (* letrec f = \x. escape e in if x = 10 then e x else f (succ x) in f 0 *)
          (LetRec
             {
               dvar = "f";
               dexp =
                 {
                   fp = "x";
                   body =
                     Escp
                       {
                         escv = "e";
                         ebody =
                           Cond
                             {
                               prem =
                                 Appl
                                   {
                                     opr =
                                       Appl
                                         { opr = Var "equal"; opnd = Var "x" };
                                     opnd = Const 10;
                                   };
                               conc = Appl { opr = Var "e"; opnd = Var "x" };
                               altr =
                                 Appl
                                   {
                                     opr = Var "f";
                                     opnd =
                                       Appl { opr = Var "succ"; opnd = Var "x" };
                                   };
                             };
                       };
                 };
               rbody = Appl { opr = Var "f"; opnd = Const 0 };
             })))
