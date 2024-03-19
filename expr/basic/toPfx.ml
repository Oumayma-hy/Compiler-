open Ast

(*let generate = function
  | Const _ -> failwith "To implement"
  | Binop(_,_,_) -> failwith "To implement"
  | Uminus _ -> failwith "To implement"
  | Var _ -> failwith "Not yet supported"*)



let rec generate (e : Ast.expression) : BasicPfx.Ast.command list = 
  match e with 
  | Const n -> [PUSH n] 
  | Binop(op,e1,e2) -> 
    generate e2 @
    generate e1 @
    (match op with 
      | BinOp.Badd -> [ADD]
      | BinOp.Bsub -> [SUB]
      | BinOp.Bmul -> [MUL]
      | BinOp.Bdiv -> [DIV]
      | BinOp.Bmod -> [REM]
    )
  | Uminus expr -> generate expr @ [UMINUS]
  | Var _ -> failwith "Not yet supported"
  