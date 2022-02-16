(* =========================================================================================================== *)
structure TypeChecker =
struct

open Model;
open CONCRETE_REPRESENTATION;

(* =========================================================================================================== *)
(*
    Here is where your typeCheck and typeOf definitions go. The primary challenge here is to translate the parse 
    expression notation we used in M2 to the actual SML tree patterns used in the TL System. See the comments in
    the semantics.sml file for a more detailed discussion on this topic. 
*)
fun typeOf( itree(inode("expr", _), 
        [ 
            expr1,
            itree(inode("or", _), [] ),
            andExpr1
        ] 
    ), m) =
        let 
            val t1 = typeOf(expr1, m)
            val t2 = typeOf(andExpr1, m)
        in
            if t1 = t2 andalso t1 = BOOL then BOOL 
            else ERROR
        end

  | typeOf( itree(inode("expr", _), [ andExpr1 ]), m) = typeOf(andExpr1, m)

(* andExpr *)

  | typeOf( itree(inode("andExpr", _),
        [
            andExpr1,
            itree(inode("and", _), []),
            equalExpr1
        ]
    ), m) =
        let 
            val t1 = typeOf(andExpr1, m)
            val t2 = typeOf(equalExpr1, m)
        in
            if t1 = t2 andalso t1 = BOOL then BOOL 
            else ERROR
        end

  | typeOf( itree(inode("andExpr", _), [ equalExpr1 ]), m) = typeOf(equalExpr1, m)

(* equalExpr *)

  | typeOf( itree(inode("equalExpr", _), 
        [ 
            equalExpr1,
            itree(inode("==", _), [] ),
            relExpr1
        ] 
    ), m) =
        let 
            val t1 = typeOf(equalExpr1, m)
            val t2 = typeOf(relExpr1, m)
        in
            if t1 = t2 andalso t1 <> ERROR then BOOL else ERROR
        end

  | typeOf( itree(inode("equalExpr", _), 
        [ 
            equalExpr1,
            itree(inode("!=", _), [] ),
            relExpr1
        ] 
    ), m) =
        let
            val t1 = typeOf(equalExpr1, m)
            val t2 = typeOf(relExpr1, m)
        in
            if t1 = t2 andalso t1 <> ERROR then BOOL else ERROR
        end

  | typeOf( itree(inode("equalExpr", _), [ relExpr1 ]), m) = typeOf(relExpr1, m)
    
(* relExpr *)

  | typeOf( itree(inode("relExpr", _), 
        [
            relExpr1,
            itree(inode("<", _), [] ),
            sumExpr1
        ]
    ), m) =
        let
            val t1 = typeOf(relExpr1, m)
            val t2 = typeOf(sumExpr1, m)
        in
            if t1 = t2 andalso t1 = INT then BOOL 
            else ERROR
        end

  | typeOf( itree(inode("relExpr", _), 
        [
            relExpr1,
            itree(inode(">", _), [] ),
            sumExpr1
        ]
    ), m) =
        let
            val t1 = typeOf(relExpr1, m)
            val t2 = typeOf(sumExpr1, m)
        in
            if t1 = t2 andalso t1 = INT then BOOL 
            else ERROR
        end

  | typeOf( itree(inode("relExpr", _), [ sumExpr1 ]), m) = typeOf(sumExpr1, m)

(* sumExpr *)

  | typeOf( itree(inode("sumExpr", _), 
        [ 
            sumExpr1,
            itree(inode("+", _), [] ),
            mulExpr1
        ] 
    ), m) = 
        let
            val t1 = typeOf(sumExpr1, m)
            val t2 = typeOf(mulExpr1, m)
        in
            if t1 = t2 andalso t1 = INT then INT
            else ERROR
        end

  | typeOf( itree(inode("sumExpr", _), 
        [ 
        sumExpr1,
        itree(inode("-", _), [] ),
        mulExpr1
        ] 
    ), m) =
        let
            val t1 = typeOf(sumExpr1, m)
            val t2 = typeOf(mulExpr1, m)
        in
            if t1 = t2 andalso t1 = INT then INT
            else ERROR
        end

  | typeOf( itree(inode("sumExpr", _), [ mulExpr1 ]), m) = typeOf(mulExpr1, m)

(* mulExpr *)

  | typeOf( itree(inode("mulExpr", _), 
        [ 
            mulExpr1,
            itree(inode("*", _), [] ),
            unaryExpr1
        ]
    ), m) =
        let
            val t1 = typeOf(mulExpr1, m)
            val t2 = typeOf(unaryExpr1, m)
        in
            if t1 = t2 andalso t1 = INT then INT
            else ERROR
        end

  | typeOf( itree(inode("mulExpr", _), 
        [ 
            mulExpr1,
            itree(inode("/", _), [] ),
            unaryExpr1
        ] 
    ), m) =
        let
            val t1 = typeOf(mulExpr1, m)
            val t2 = typeOf(unaryExpr1, m)
        in
            if t1 = t2 andalso t1 = INT then INT
            else ERROR
        end

  | typeOf( itree(inode("mulExpr", _), 
        [ 
            mulExpr1,
            itree(inode("%", _), [] ),
            unaryExpr1
        ] 
    ), m) = 
        let
            val t1 = typeOf(mulExpr1, m)
            val t2 = typeOf(unaryExpr1, m)
        in
            if t1 = t2 andalso t1 = INT then INT 
            else ERROR
        end

  | typeOf( itree(inode("mulExpr", _), [ unaryExpr1 ]), m) = typeOf(unaryExpr1, m) 

(* unaryExpr *)

  | typeOf( itree(inode("unaryExpr", _),
        [
            itree(inode("abs", _), [] ),
            unaryExpr1
        ]
    ), m) = 
        let
            val t1 = typeOf(unaryExpr1, m)
        in
            if t1 = INT then INT else ERROR
        end

  | typeOf( itree(inode("unaryExpr", _),
        [
            itree(inode("not", _), [] ),
            unaryExpr1
        ]
    ), m) = 
        let
            val t1 = typeOf(unaryExpr1, m)
        in
            if t1 = BOOL then BOOL else ERROR 
        end

  | typeOf( itree(inode("unaryExpr", _),
        [
            itree(inode("~", _), [] ),
            unaryExpr1
        ]
    ), m) =
        let
            val t1 = typeOf(unaryExpr1, m)
        in
            if t1 = INT then INT else ERROR
        end

  | typeOf( itree(inode("unaryExpr", _), [ expExpr1 ]), m) = typeOf(expExpr1, m)

(* expExpr *)
    
  | typeOf( itree(inode("expExpr", _), 
        [ 
            factor1,
            itree(inode("^", _), [] ),
            expExpr1
        ] 
    ), m) = 
        let
            val t1 = typeOf(factor1, m)
            val t2 = typeOf(expExpr1, m)
        in
            if t1 = t2 andalso t1 = INT then INT
            else ERROR
        end

  | typeOf( itree(inode("expExpr", _), [ factor1 ]), m) = typeOf(factor1, m)

(* factor *)

  | typeOf( itree(inode("factor", _), [ itree(inode("true", _), [] ) ]), m) = BOOL
  | typeOf( itree(inode("factor", _), [ itree(inode("false", _), [] ) ]),  m) = BOOL        
  | typeOf( itree(inode("factor", _), [ child ]), m) = typeOf(child, m)
  | typeOf( itree(inode("factor", _), 
        [ 
            itree(inode("(", _), [] ),
            expr1,
            itree(inode(")", _), [] )
        ] 
    ), m) = typeOf(expr1, m)
    
  | typeOf( ID as itree(inode("IDENTIFIER", _), [ _ ] ), m) = 
        let 
            val idName  = getLeaf(ID)
        in
            getType(accessEnv(idName, m))
        end

  | typeOf( itree(inode("INT_LITERAL", _), [ _ ] ), m) = INT

(* decoratedID *)

  | typeOf( itree(inode("decoratedID", _), 
        [ 
            itree(inode("++", _), [] ),
            id1
        ] 
    ), m) = 
        let 
            val idName  = getLeaf(id1)
            val t1 = getType(accessEnv(idName, m))
        in
            if t1 = INT then INT else ERROR
        end

  | typeOf( itree(inode("decoratedID", _), 
        [ 
            itree(inode("--", _), [] ),
            id1
        ] 
    ), m) = 
        let 
            val idName  = getLeaf(id1)
            val t1 = getType(accessEnv(idName, m))
        in
            if t1 = INT then INT else ERROR
        end

  | typeOf( itree(inode("decoratedID", _), 
        [ 
            id1,
            itree(inode("++", _), [] )
        ] 
    ), m) = 
        let 
            val idName  = getLeaf(id1)
            val t1 = getType(accessEnv(idName, m))
        in
            if t1 = INT then INT else ERROR
        end

  | typeOf( itree(inode("decoratedID", _), 
        [ 
            id1,
            itree(inode("--", _), [] )
        ] 
    ), m) = 
        let 
            val idName  = getLeaf(id1)
            val t1 = getType(accessEnv(idName, m))
        in
            if t1 = INT then INT else ERROR
        end

  | typeOf( itree(inode(x_root, _), children), _) = raise General.Fail("\n\nIn typeOf root = " ^ x_root ^ "\n\n")
  | typeOf _ = raise Fail("Error in TypeChecker.typeOf - this should never occur")

(* Type Checker *)

fun typeCheck( itree(inode("statementList", _), [ itree(inode("", _), []) ] ), m) = m
  | typeCheck( itree(inode("statementList", _), 
        [
            stmt,
            stmtList
        ]
    ), m) = 
        let
            val m1 = typeCheck(stmt, m)
            val m2 = typeCheck(stmtList, m1)
        in
            m2
        end
    
(* Statement *)

  | typeCheck( itree(inode("statement", _), [ child ] ), m) = typeCheck(child, m) 

  | typeCheck( itree(inode("openStatement", _),
        [
            itree(inode("if", _), [] ),
            itree(inode("(", _), [] ),
            expr1,
            itree(inode(")", _), [] ),
            stmt1
        ]
    ), m) = 
        let
            val t1 = typeOf(expr1, m)
            val m1 = typeCheck(stmt1, m)
        in
            if t1 = BOOL then m else raise Fail("Type mistmatch")
        end

  | typeCheck( itree(inode("openStatement", _),
        [
            itree(inode("if", _), [] ),
            itree(inode("(", _), [] ),
            expr1,
            itree(inode(")", _), [] ),
            closedStmt1,
            itree(inode("else", _), [] ),
            openStmt1
        ]
    ), m) = 
        let
            val t1 = typeOf(expr1, m)
            val m1 = typeCheck(closedStmt1, m)
            val m2 = typeCheck(openStmt1, m1)
        in
            if t1 = BOOL then m else raise Fail("Type mistmatch")
        end
  | typeCheck( itree(inode("openStatement", _),
        [
            itree(inode("while", _), [] ),
            itree(inode("(", _), [] ),
            expr1,
            itree(inode(")", _), [] ),
            openStmt1
        ]
    ), m) =
        let
          val t1 = typeOf(expr1, m)    
          val m1 = typeCheck(openStmt1, m)
        in
          if t1 = BOOL then m else raise Fail("Type mistmatch")
        end
  | typeCheck( itree(inode("openStatement", _),
        [
            itree(inode("for", _), [] ),
            itree(inode("(", _), [] ),
            assignment1,
            itree(inode(";", _), [] ),
            expr1,
            itree(inode(";", _), [] ),
            assignment2,
            itree(inode(")", _), [] ),
            openStmt1
        ]
    ), m) = 
        let
            val m1 = typeCheck(assignment1, m)    
            val t1 = typeOf(expr1, m1)
            val m2 = typeCheck(assignment2, m1)    
            val m3 = typeCheck(openStmt1, m2)
        in
            if t1 = BOOL then m else raise Fail("Type mistmatch")
        end

  | typeCheck( itree(inode("closedStatement", _), [ child ] ), m) = typeCheck(child, m) 
  | typeCheck( itree(inode("closedStatement", _),
        [
            itree(inode("if", _), [] ),
            itree(inode("(", _), [] ),
            expr1,
            itree(inode(")", _), [] ),
            closedStmt1,
            itree(inode("else", _), [] ),
            closedStmt2
        ]
    ), m) = 
        let
            val t1 = typeOf(expr1, m)
            val m1 = typeCheck(closedStmt1, m)
            val m2 = typeCheck(closedStmt2, m1)
        in
            if t1 = BOOL then m else raise Fail("Type mistmatch")
        end
  | typeCheck( itree(inode("closedStatement", _),
        [
            itree(inode("while", _), [] ),
            itree(inode("(", _), [] ),
            expr1,
            itree(inode(")", _), [] ),
            closedStmt1
        ]
    ), m) =
        let
          val t1 = typeOf(expr1, m)    
          val m1 = typeCheck(closedStmt1, m)
        in
          if t1 = BOOL then m else raise Fail("Type mistmatch")
        end
  | typeCheck( itree(inode("closedStatement", _),
        [
            itree(inode("for", _), [] ),
            itree(inode("(", _), [] ),
            assignment1,
            itree(inode(";", _), [] ),
            expr1,
            itree(inode(";", _), [] ),
            assignment2,
            itree(inode(")", _), [] ),
            closedStmt1
        ]
    ), m) = 
        let
            val m1 = typeCheck(assignment1, m)    
            val t1 = typeOf(expr1, m1)
            val m2 = typeCheck(assignment2, m1)    
            val m3 = typeCheck(closedStmt1, m2)
        in
            if t1 = BOOL then m else raise Fail("Type mistmatch")
        end

  | typeCheck( itree(inode("simpleStatement", _), [ itree(inode(";", _), [] ) ]), m) = m
  | typeCheck( itree(inode("simpleStatement", _), [ child ]), m) = typeCheck(child, m)
  | typeCheck( itree(inode("simpleStatement", _),
        [
            child,
            itree(inode(";", _), [] )
        ]
    ), m) = typeCheck(child, m)    
  | typeCheck( itree(inode("simpleStatement", _),
        [
            itree(inode("print", _), [] ),
            itree(inode("(", _), [] ),
            expr1,
            itree(inode(")", _), [] ),
            itree(inode(";", _), [] )
        ]
    ), m) = 
        let 
            val t1 = typeOf(expr1, m)
        in
            if t1 <> ERROR then m else raise Fail("Type mistmatch")
        end

(* assignment *)

  | typeCheck( itree(inode("assignment", _),
        [
            id1,
            itree(inode("=", _), [] ),
            expr1
        ]
    ), m) =
        let
          val t1 = typeOf(expr1, m)
          val idName  = getLeaf(id1)
          val t2 = getType(accessEnv(idName, m))
        in
          if t1 = t2 then m else raise Fail("Type mistmatch")
        end
        
  | typeCheck( itree(inode("assignment", _), [ decoratedID1 ]), m) = 
        let 
            val t1 = typeOf(decoratedID1, m)
        in
            if t1 = INT then m else raise Fail("Type mistmatch")
        end

(* declaration *)

  | typeCheck( itree(inode("decl", _),
        [
            itree(inode("bool", _), [] ),
            id1
        ]
    ), m) = 
        let
          val idName  = getLeaf(id1)
        in 
          updateEnv(idName, BOOL, m)
        end

  | typeCheck( itree(inode("decl", _),
        [
            itree(inode("int", _), [] ),
            id1
        ]
    ), m) = 
        let
          val idName  = getLeaf(id1)
        in 
          updateEnv(idName, INT, m)
        end

(* block *)

  | typeCheck( itree(inode("block", _),
        [
            itree(inode("{", _), [] ),
            statementList1,
            itree(inode("}", _), [] )
        ]
    ), m) = 
        let
            val m1 = typeCheck(statementList1, m)
        in 
            m
        end

  | typeCheck( itree(inode(x_root, _), children), _) = raise General.Fail("\n\nIn typeCheck root = " ^ x_root ^ "\n\n")
  | typeCheck _ = raise Fail("Error in TypeChecker.typeCheck - this should never occur")


(* =========================================================================================================== *)  
end; (* struct *)
(* =========================================================================================================== *)
