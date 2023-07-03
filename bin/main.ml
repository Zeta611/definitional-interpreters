open Stdio

open
  Definitional_interpreters
  .Higher_order_continuation_passing_escape_assign_interpreter

let () =
  printf "%s\n"
    (show_value
       (interpret
          (* \p.(letrec f = \p. escape e in if val p = 10 then e (val p) else (\x1.\x2.x2) ((set p) (succ (val p))) (f p) in f p) (ref 0) *)
          (Appl
             {
               opr =
                 Lambda
                   {
                     fp = "p";
                     body =
                       LetRec
                         {
                           dvar = "f";
                           dexp =
                             {
                               fp = "p";
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
                                                     {
                                                       opr = Var "equal";
                                                       opnd =
                                                         Appl
                                                           {
                                                             opr = Var "val";
                                                             opnd = Var "p";
                                                           };
                                                     };
                                                 opnd = Const 10;
                                               };
                                           conc =
                                             Appl
                                               {
                                                 opr = Var "e";
                                                 opnd =
                                                   Appl
                                                     {
                                                       opr = Var "val";
                                                       opnd = Var "p";
                                                     };
                                               };
                                           altr =
                                             Appl
                                               {
                                                 opr =
                                                   Appl
                                                     {
                                                       opr =
                                                         Lambda
                                                           {
                                                             fp = "x1";
                                                             body =
                                                               Lambda
                                                                 {
                                                                   fp = "x2";
                                                                   body =
                                                                     Var "x2";
                                                                 };
                                                           };
                                                       opnd =
                                                         Appl
                                                           {
                                                             opr =
                                                               Appl
                                                                 {
                                                                   opr =
                                                                     Var "set";
                                                                   opnd =
                                                                     Var "p";
                                                                 };
                                                             opnd =
                                                               Appl
                                                                 {
                                                                   opr =
                                                                     Var "succ";
                                                                   opnd =
                                                                     Appl
                                                                       {
                                                                         opr =
                                                                           Var
                                                                             "val";
                                                                         opnd =
                                                                           Var
                                                                             "p";
                                                                       };
                                                                 };
                                                           };
                                                     };
                                                 opnd =
                                                   Appl
                                                     {
                                                       opr = Var "f";
                                                       opnd = Var "p";
                                                     };
                                               };
                                         };
                                   };
                             };
                           rbody = Appl { opr = Var "f"; opnd = Var "p" };
                         };
                   };
               opnd = Appl { opr = Var "ref"; opnd = Const 0 };
             })))
