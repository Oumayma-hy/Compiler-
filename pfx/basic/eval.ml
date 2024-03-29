open Ast
open Printf

let string_of_stack stack = sprintf "[%s]" (String.concat ";" (List.map string_of_int stack))

let string_of_state (cmds,stack) =
  (match cmds with
   | [] -> "no command"
   | cmd::_ -> sprintf "executing %s" (string_of_command cmd))^
    (sprintf " with stack %s" (string_of_stack stack))

(* Question 4.2 *)
let step state =
  match state with
  | [], _ -> Error("Nothing to step",state)
  (* Valid configurations *)
  | DefineMe :: q , stack          -> Ok (q, stack)

  |push n::q , stack -> Ok(q , n::stack)
  |add ::q , stack ->
   (match stack with 
   | x:: y :: s -> Ok(q , (x+y)::s)
   |_ -> Error(" pas d'arguments", (q , [])))
  |sub ::q , stack ->
   (match stack with 
   | x:: y :: s -> Ok(q , (x-y)::s)
   |_ -> Error(" pas d'arguments", (q , [])))
  |mul ::q , stack ->
   (match stack with 
   | x:: y :: s -> Ok(q , (x*y)::s)
   |_ -> Error(" pas d'arguments", (q , [])))
  |div ::q , stack ->
   (match stack with 
   | x:: y :: s -> Ok(q , (x/y)::s)
   |_ -> Error(" pas d'arguments", (q , [])))
  |swap ::q , stack -> 
   (match stack with 
   | x:: y :: s -> Ok(q , y::x::s)
   |_ -> Error(" pas d'arguments", (q , [])))
  |pop ::q , stack with 
    (match stack with 
    | _:: s -> Ok(q , s)
    |_ -> Error(" pas d'arguments", (q , [])))

let eval_program (numargs, cmds) args =
  let rec execute = function
    | [], []    -> Ok None
    | [], v::_  -> Ok (Some v)
    | state ->
       begin
         match step state with
         | Ok s    -> execute s
         | Error e -> Error e
       end
  in
  if numargs = List.length args then
    match execute (cmds,args) with
    | Ok None -> printf "No result\n"
    | Ok(Some result) -> printf "= %i\n" result
    | Error(msg,s) -> printf "Raised error %s in state %s\n" msg (string_of_state s)
  else printf "Raised error \nMismatch between expected and actual number of args\n"
