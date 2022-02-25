structure Interpreter =
struct

(* =========================================================================================================== *)
(*  Naming conventions

    Variables       Symbolic or initial lower case. Use embedded caps for multiword names.   getItem
    Constructors    Initial upper case.  Use embedded caps for multiword names.              Node
                    Historic exceptions are nil, true, and false.  Rarely are                EmptyQueue
                    symbolic names like :: used.
    Types           All lower case.  Use underscores for multiword names.                    priority_queue
    Signatures      All upper case.  Use underscores for multiword names.                    PRIORITY_QUEUE
    Structures      Initial upper case.  Use embedded caps for multiword names.              PriorityQueue
    Functors        Same as structure convention, except Fn completes the name.              PriorityQueueFn
*)
(* =========================================================================================================== *)


(* =========================================================================================================== *)
fun execute [ programTree ] =
    let
        val tree0 = Strategic_Values.getTerm programTree
       
        val _     = TypeChecker.typeCheck(tree0,Model.initialModel); 

        val result = Semantics.M(tree0,Model.initialModel);
    in 
        (* print("\n\n\n");
        print(" ======================================= \n");
        Model.printModel(result);
        print("\n ======================================= \n");
        print("\n\n\n"); *)

        result
    end
  | execute _ = raise Fail("Error in Interpreter.execute - this should never occur")

          
(* ------------------------------------------------------------------------------------------- *)
(* ------------------------------------------------------------------------------------------- *)
(*                                 Exported Function List                                      *)
(* ------------------------------------------------------------------------------------------- *)
(* ------------------------------------------------------------------------------------------- *)
   val functions = 

    [
        (* here is where the call to the interpreter is mapped to the sml function: execute *)
        
        ("interpreter_execute"  , Util.execVoid execute)
    ]

(* =========================================================================================================== *)
end; (* struct *)
(* =========================================================================================================== *)





