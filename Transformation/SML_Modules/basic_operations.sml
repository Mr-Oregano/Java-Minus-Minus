(* ------------------------------------------------------------------------------------------- *)
structure BasicOperations =
struct

val TRUE  = ABSTRACT_REPRESENTATION.TRUE;
val FALSE = ABSTRACT_REPRESENTATION.FALSE;

(* ------------------------------------------------------------------------------------------- *)
fun toInt( term ) = valOf(Int.fromString ( Strategic_Values.termToString term ) )
                    handle _=> raise Fail("BasicOperations: expected int")

(* ------------------------------------------------------------------------------------------- *)
fun relOp [ operator, term1, term2 ] =
    let
        fun compare( theOp, v1, v2) =
            (
                case theOp of
                      "<"  => v1 < v2
                    | "<=" => v1 <= v2
                    | ">"  => v1 > v2
                    | ">=" => v1 >= v2
                    | "="  => v1 = v2
                    | "<>" => v1 < v2
                    | x    => raise Fail("BasicOperations.relOpInt: unknown operator " ^ x)
            )

        val theOp = Strategic_Values.getString operator

        val v1 = toInt term1
        val v2 = toInt term2

        val result = compare(theOp,v1,v2)

    in
        if result then TRUE else FALSE
    end
  | relOp _ = raise Fail("BasicOperations.relOp: requires 3 args ")

(* ------------------------------------------------------------------------------------------- *)
(* ------------------------------------------------------------------------------------------- *)
(*                                 Exported Function List                                      *)
(* ------------------------------------------------------------------------------------------- *)
(* ------------------------------------------------------------------------------------------- *)
   val execVoid = Util.execVoid
   
   val functions = 

    [
        ("relOp" , relOp )
    ]

(* ------------------------------------------------------------------------------------------- *)    
end (* struct *)
(* ------------------------------------------------------------------------------------------- *)