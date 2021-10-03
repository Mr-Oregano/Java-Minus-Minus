(* =========================================================================================================== *)                                  
(* =========================================================================================================== *)
structure UniversalUtil =
struct

open CONCRETE_REPRESENTATION;


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

fun getStringFromTL x  = 
    let
        fun cleanDuplicateBlanks ( #" " :: #" " :: rest ) = cleanDuplicateBlanks ( #" " :: rest )
          | cleanDuplicateBlanks ( x :: xs )              = x :: cleanDuplicateBlanks xs
          | cleanDuplicateBlanks []                       = []
        val str = Strategic_Values.getString x handle _ => CONCRETE.leavesToStringWithSep(Strategic_Values.getTerm x, " ") 
    in
        implode( cleanDuplicateBlanks (explode str) )
    end
    
(* ----------------------------------------------------------------------------------------------------------- *)        
fun getNameFromTL_List nameList =  foldr (fn (x, acc) => getStringFromTL x ^ acc) "" nameList         

fun safeDivide( numerator, denominator ) = if denominator > 0.0 then numerator / denominator else 0.0

(* ----------------------------------------------------------------------------------------------------------- *)
fun termFromForest (rootStr :: forest) =
    let
        val root      = Strategic_Values.getString rootStr
        val forestStr = getNameFromTL_List forest

        val nullNodeInfo = CONCRETE_REPRESENTATION.dummy_node_info
        val theTerm = itree(inode(root,nullNodeInfo),
                              [
                                 itree(inode(forestStr,nullNodeInfo),[])
                              ]
                            );
    in 
        ABSTRACT.makeTerm(theTerm,nullNodeInfo)
    end
  | termFromForest _ = raise General.Fail("Error: termFromForest badly formed")    
    
(* =========================================================================================================== *)
(* ----------------------------------------------------------------------------------------------------------- *)
(*                                        TL Interface                                                         *)
(* ----------------------------------------------------------------------------------------------------------- *)
(* =========================================================================================================== *)
val execVoid = Util.execVoid

val functions =
    [
        (* Naming convention: "filePath_functionName", functionName    *)

        ("termFromForest" , termFromForest )
    ];    
(* =========================================================================================================== *)
end;
(* =========================================================================================================== *)
