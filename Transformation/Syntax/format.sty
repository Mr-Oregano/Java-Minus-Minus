 
(* =================================================================================================== *)
(* --------------------------------------------------------------------------------------------------- *)
(*                                    Structure                                                        *)
(* --------------------------------------------------------------------------------------------------- *)
(* =================================================================================================== *)
structure PRETTYPRINT_STYLES: PRETTYPRINT_STYLES_SIG =
struct
open CONCRETE_REPRESENTATION;
open PRETTYPRINT_DATATYPES;


(* --------------------------------------------------------------------------------------------------- *)
(*                                  Auxiliary Functions                                                *)
(* --------------------------------------------------------------------------------------------------- *)

val leaf_node_count = ref 0;

fun auxLeafNodeCount(itree( inode      ("", _)                   , [] ) ) = ()
  | auxLeafNodeCount(itree( inode      (name, _)                 , [] ) ) = leaf_node_count := !leaf_node_count + 1

  | auxLeafNodeCount(t as itree(inode(name,_),children) ) =
    let
        fun traverse( itree( inode("",_)                         , []       ) ) = ()
          | traverse (itree( inode(name,_)                       , []       ) ) = leaf_node_count := !leaf_node_count + 1
          |    traverse (itree( inode(name,_)                       , children ) ) = foldr (fn (t, _) => traverse t ) () children
              | traverse t                                                 = (
                                            print("\n\n=========================================\n\n");
                                            CONCRETE_REPRESENTATION.fullPrintTree " " t;
                                            raise General.Fail("Error in Oberon.sty: auxLeafNodeCount.traverse.\n")
                                            );
    in
        traverse t
    end
  | auxLeafNodeCount t = (
                              print("\n\n=========================================\n\n");
                              CONCRETE_REPRESENTATION.fullPrintTree " " t;
                              raise General.Fail("Error in Oberon.sty: auxLeafNodeCount.\n")
                                 );

fun nonEmptyLeafNodeCount( i ) =
    (
        leaf_node_count := 0;
        auxLeafNodeCount( getSubTree i );
        !leaf_node_count
    );

(* --------------------------------------------------------------------------------------------------- *)
fun sumLeaves( i, max ) = if i > max then 0
                  else String.size(CONCRETE.leavesToStringRaw(getSubTree i)) + sumLeaves(i+1,max)

fun makeBlanks(0) = ""
  | makeBlanks(n) = if n > 0 then " " ^ makeBlanks(n-1)
                 else raise General.Fail("Error in syle file: makeBlanks\n")
(* --------------------------------------------------------------------------------------------------- *)



val nl     = "\n";
val sp  = " ";
val tab = sp ^ sp ^ sp ^ sp;


(* =================================================================================================== *)
(*                                        Formatting                                                   *)
(* =================================================================================================== *)

(* -------------------------------------------------------------------- *)
val format_list =

    [
        (* ------------------------------------------------------------------------------ *)
        (
            "elementList ::= element ; elementList",
            fn LM =>
            [
                process(1,LM),
                process(2,LM),
                insert(nl ^ LM),
                process(3,LM)
            ]
        )
    ];


(* =================================================================================================== *)
end (* struct *)
(* =================================================================================================== *)


