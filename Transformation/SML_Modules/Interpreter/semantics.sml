(* =========================================================================================================== *)
structure Semantics =
struct


(* This makes contents of the Model structure directly accessible (i.e., the prefix "Model." is not needed. *)            
open Model; 
            
(* This makes the internal representation of parse trees directly accessible. *)            
open CONCRETE_REPRESENTATION;

(* The following tree structure, defined in the CONCERETE_REPRESENTATION structure, is used in the TL System:

    datatype NODE_INFO = info of { id : IntInf.int, line : int * int , column : int * int, label : string };
    datatype INODE = inode of string * NODE_INFO
                     | ...  
                                                            
    datatype ITREE = itree of INODE * ITREE list;
*)


(* =========================================================================================================== *)
(* Here is where you add the M and E (as well as any other) definitions you developed in M2. The primary challenge here
   is to translate the parse expression notation we used in M2 to the actual SML tree patterns used in the TL System. 
   
   Example:
   
   M1: <stmtList> ::= <stmt> ";" <stmList>
   
   M2: M( [[ stmt_1 ; stmtList_1 ]], m) = M(stmtList_1, M(stmt_1,m))
    
   M4: 
        M( itree(inode("stmtList", _),
                    [
                        stmt,     (* this is a regular variable in SML and has no other special meaning *)
                        semiColon,  (* this is a regular variable in SML and has no other special meaning *)
                        stmtList  (* this is a regular variable in SML and has no other special meaning *) 
                    ]
                ),
           m
           
        ) = M( stmtList, M(stmt, m) )  
        
        
        Note that the above M4 implementation will match ANY tree whose root is "stmtList" having three children.
        Such matches can be further constrained by explicitly exposing more of the tree structure.
        
        M( itree(inode("stmtList", _),
                    [
                        stmt,                     (* this is a regular variable in SML and has no other special meaning *)
                        itree(inode(";", _), [] ),   (* A semi-colon is a leaf node. All leaf nodes have an empty children list. *)
                        
                        stmtList                  (* this is a regular variable in SML and has no other special meaning *) 
                    ]
                ),
           m
           
        ) = M( stmtList, M(stmt, m) )  
        
        Note that the above M4 implementation will match ANY tree satisifying the following constraints:
            (1) the root is "stmtList"
            (2) the root has three children
            (3) the second child is a semi-colon   
*)

(* expr: *)
fun E( itree(inode("expr", _), 
        [ 
            expr1
            itree(inode("or", _), [] ),
            andExpr1
        ]
    ), m) =
        let 
            val (v1, m1) = E(expr1, m)
        in
            if v1 then (v1, m1)
            else E(andExpr1, m1)
        end;
        
  | E( itree(inode("expr", _), [ andExpr1 ]), m) = E(andExpr1, m)

(* andExpr *)

  | E( itree(inode("andExpr"),
        [
            andExpr1,
            itree(inode("and", _), []),
            equalExpr1
        ]
    ), m) =
        let
            val (v1, m1) = E(andExpr1, m)
        in
            if not v1 then (v1, m1) 
            else E(equalExpr1, m1)
        end;

  | E( itree(inode("andExpr"), [ equalExpr1 ]), m) = E(equalExpr1, m)

(* equalExpr *)

  | E( itree(inode("equalExpr", _), 
        [ 
            equalExpr1,
            itree(inode("==", _), [] ),
            relExpr1
        ]
    ), m) =
        let
            val (v1, m1) = E(equalExpr1, m)
            val (v2, m2) = E(relExpr1, m1)
        in
            (v1 = v2, m2)
        end;

  | E( itree(inode("equalExpr", _), 
        [ 
            equalExpr1,
            itree(inode("!=", _), [] ),
            relExpr1
        ]
    ), m) =  
        let
            val (v1, m1) = E(equalExpr1, m)
            val (v2, m2) = E(relExpr1, m1)
        in
           (v1 <> v2, m2)
        end;

  | E( itree(inode("equalExpr", _), [ relExpr1 ]), m) = E(relExpr1, m)
        
(* relExpr *)

  | E( itree(inode("relExpr", _), 
        [
            relExpr1,
            itree(inode("<", _), [] ),
            sumExpr1
        ]
    ), m) =
        let
            val (v1, m1) = E(relExpr1, m)
            val (v2, m2) = E(sumExpr1, m1)
        in
            (v1 < v2, m2)
        end;
        
  | E( itree(inode("relExpr", _), 
        [
            relExpr1,
            itree(inode(">", _), [] ),
            relExpr1
        ]
    ), m) =
        let
            val (v1, m1) = E(relExpr1, m)
            val (v2, m2) = E(sumExpr1, m1)
        in
            (v1 > v2, m2)
        end;

  | E( itree(inode("sumExpr", _), [ sumExpr1 ]), m) = E(sumExpr1, m)

(* sumExpr *)

  | E( itree(inode("sumExpr", _), 
        [ 
            sumExpr1,
            itree(inode("+", _), [] ),
            mulExpr1
        ]
    ), m) =
        let
            val (v1, m1) = E(sumExpr1, m)
            val (v2, m2) = E(mulExpr1, m1)
        in
            (v1 + v2, m2)
        end;
        
  | E( itree(inode("sumExpr", _), 
        [ 
            sumExpr1,
            itree(inode("-", _), [] ),
            mulExpr1
        ]
    ), m) =
        let
            val (v1, m1) = E(sumExpr1, m)
            val (v2, m2) = E(mulExpr1, m1)
        in
            (v1 - v2, m2)
        end;

  | E( itree(inode("sumExpr", _), [ mulExpr1 ]), m) = E(mulExpr1, m)

(* mulExpr *)
    
  | E( itree(inode("mulExpr", _), 
        [ 
            mulExpr1,
            itree(inode("*", _), [] ),
            unaryExpr1
        ]
    ), m) =   
        let
            val (v1, m1) = E(mulExpr1, m)
            val (v2, m2) = E(unaryExpr1, m1)
        in
            (v1 * v2, m2)
        end;

  | E( itree(inode("mulExpr", _), 
        [ 
            mulExpr1,
            itree(inode("/", _), [] ),
            unaryExpr1
        ]
    ), m) = 
        let
            val (v1, m1) = E(mulExpr1, m)
            val (v2, m2) = E(unaryExpr1, m1)
        in
            (v1 div v2, m2)
        end;

  | E( itree(inode("mulExpr", _), 
        [ 
            mulExpr1,
            itree(inode("%", _), [] ),
            unaryExpr1
        ]
    ), m) = 
        let
            val (v1, m1) = E(mulExpr1, m)
            val (v2, m2) = E(unaryExpr1, m1)
        in
            (v1 mod v2, m2)
        end;

  | E( itree(inode("mulExpr", _), [ unaryExpr1 ]), m) = E(unaryExpr1, m)
        
(* unaryExpr *)

  | E( itree(inode("unaryExpr", _), 
        [ 
            itree(inode("abs", _), [] ),
            unaryExpr1
        ]
    ), m) =
        let
            val (v1, m1) = E(unaryExpr1, m)
        in
            (abs(v1), m1) (* Assume abs is a function *)
        end;

  | E( itree(inode("unaryExpr", _), 
        [ 
            itree(inode("not", _), [] ),
            unaryExpr1
        ]
    ), m) =
        let
            val (v1, m1) = E(unaryExpr1, m1)
        in
            (not v1, m1) 
        end;

  | E( itree(inode("unaryExpr", _), 
        [ 
            itree(inode("~", _), [] ),
            unaryExpr1
        ]
    ), m) =
        let
            val (v1, m1) = E(unaryExpr1, m1)
        in
            (~v1, m2)
        end;

  | E( itree(inode("unaryExpr", _), [ expExpr1 ]), m) = E(expExpr1, m)

(* expExpr *)

  | E( itree(inode("expExpr", _), 
        [
            factor1,
            itree(inode("^", _), [] ),
            expExpr1
        ]
    ), m) =
        let
            val (v1, m1) = E(factor1, m)
            val (v2, m2) = E(expExpr1, m1)
        in
            (exp(v1, v2), m2) (* Assume exp is a function *)
        end;

  | E( itree(inode("expExpr", _), [ factor1 ]), m) = E(factor1, m)

(* factor *)

  | E( itree(inode("factor", _), [ id1 ]), m) = 
        let 
            val idName  = getLeaf(id1)
            val loc = getLoc(accessEnv(idName, m))
            val v = accessStore(loc, m)
        in
            (v, m)
        end;

  | E( itree(inode("factor", _), 
        [ 
            itree(inode("true", _), [] ), 
        ]
    ), m) = (Boolean true, m)
  | E( itree(inode("factor", _), 
        [ 
            itree(inode("false", _), [] ), 
        ]
    ), m) = (Boolean false, m)
  | E( itree(inode("factor", _), 
        [ 
            itree(inode("INT_LITERAL", _), [ intLiteral ]) 
        ]
    ), m) = (Integer Int.fromString(intLiteral), m)
  | E( itree(inode("factor", _), 
        [ 
            itree(inode("(", _), [] ),
            expr1,
            itree(inode(")", _), [] )
        ]
    ), m) = E(expr1)

  | E( itree(inode("factor", _), [ decoratedID1 ]), m) =  E(decoratedID1, m)

(* decoratedID *)

  | E( itree(inode("decoratedID", _), 
        [ 
            itree(inode("++", _), [] ),
            id1
        ]
    ), m) = 
        let 
            val idName  = getLeaf(id1)
            val loc = getLoc(accessEnv(idName, m))
            val v = accessStore(loc, m)
            val v2 = v + 1
            val m1 = updateStore(loc, v2, m)
        in
            (v2, m1)
        end;

  | E( itree(inode("decoratedID", _), 
        [ 
            itree(inode("--", _), [] ),
            id1
        ]
    ), m) =   
        let 
            val idName  = getLeaf(id1)
            val loc = getLoc(accessEnv(idName, m))
            val v = accessStore(loc, m)
            val v2 = v - 1
            val m1 = updateStore(loc, v2, m)
        in
            (v2, m1)
        end;

  | E( itree(inode("decoratedID", _), 
        [ 
            id1,
            itree(inode("++", _), [] )
        ]
    ), m) = 
        let 
            val idName  = getLeaf(id1)
            val loc = getLoc(accessEnv(idName, m))
            val v = accessStore(loc, m)
            val v2 = v + 1
            val m1 = updateStore(loc, v2, m)
        in
            (v, m1)
        end;

  | E( itree(inode("decoratedID", _), 
        [ 
            id1,
            itree(inode("--", _), [] )
        ]
    ), m) = 
          let 
            val idName  = getLeaf(id1)
            val loc = getLoc(accessEnv(idName, m))
            val v = accessStore(loc, m)
            val v2 = v - 1
            val m1 = updateStore(loc, v2, m)
        in
            (v, m1)
        end;

(* Semantics *)

fun M( itree(inode("statementList", _), [ itree(inode("", _), []) ]), m) = m
  | M( itree(inode("statementList", _), 
        [
            stmt,
            stmtList
        ]
    ), m) = 
        let
            val m1 = M(stmt, m)
            val m2 = M(stmtList, m1)
        in
            m2
        end;

  (* Statement *)

  | M( itree(inode("statement", _), [ itree(inode(";", _), [] ) ]), m) = m
  | M( itree(inode("statement", _), [ child ]), m) = M(child, m)
  
  | M( itree(inode("statement", _),
        [
            child,
            itree(inode(";", _), [] )
        ]
    ), m) = M(child, m)

  | M( itree(inode("statement", _),
        [
            itree(inode("if", _), [] ),
            itree(inode("(", _), [] ),
            expr1,
            itree(inode(")", _), [] ),
            block1
        ]
    ), m) = 
        let
            val (v, m1) = E(expr1, m)
        in
            if v then M(block1, m1)
            else m1
        end;

  | M( itree(inode("statement", _),
        [
            itree(inode("if", _), [] ),
            itree(inode("(", _), [] ),
            expr1,
            itree(inode(")", _), [] ),
            block1,
            itree(inode("else", _), [] ),
            block2
        ]
    ), m) = 
        let
            val (v, m1) = E(expr1, m)
        in
            if v then M(block1, m1)
            else M(block2, m1)
        end;
        
  | M( itree(inode("statement", _),
        [
            itree(inode("print", _), [] ),
            itree(inode("(", _), [] ),
            expr1
            itree(inode(")", _), [] ),
            itree(inode(";", _), [] )
        ]
    ), m) = 
        let 
            val (v, m1) = E(expr1, m)
            print(dvToString v)
        in
            m1
        end;

(* assignment *)

  | M( itree(inode("assignment", _),
        [
            id1,
            itree(inode("=", _), [] )
            expr1
        ]
    ), m) = 
        let
            val (v, m1) = E(expr1, m)
            val idName = getLeaf(id1)
            val loc = getLoc(accessEnv(idName, m))
            val m2 = updateStore(loc, v, m1)
        in
            m2
        end;

  | M( itree(inode("assignment", _), [ decoratedID1 ]), m) = 
        let 
            val (v, m1) = E(decoratedID1, m)
        in
            m1
        end;

(* declaration *)

  | M( itree(inode("decl", _),
        [
            itree(inode("bool", _), [] ),
            id1
        ]
    ), m) = 
        let
            val idName  = getLeaf(id1)
        in
            updateEnv(idName, BOOL, m)
        end;

  | M( itree(inode("decl", _),
        [
            itree(inode("int", _), [] ),
            id1
        ]
    ), m) = 
        let
            val idName  = getLeaf(id1)
        in
            updateEnv(idName, INT, m)
        end;

(* block *)

  | M( itree(inode("block", _),
        [
            itree(inode("{", _), [] ),
            statementList1,
            itree(inode("}", _), [] )
        ]
    ), (env, c, s)) = 
        let
            val (env1, c1, str1) = M(statementList1, (env, c, s))
        in
            (env, c, str1)
        end;      

(* whileLoop *)

  | M( itree(inode("whileLoop", _),
        [
            itree(inode("while", _), [] ),
            itree(inode("(", _), [] ),
            expr1,
            itree(inode(")", _), [] ),
            block1
        ]
    ), m) = 
        let
            fun N(expr1, block1, m) = 
                let
                    val (v, m1) = E(expr1, m)
                in
                    if v then 
                        let              
                            val m2 = M(block1, m1)    
                            val m3 = N(expr1, block1, m2)    
                        in
                            m3
                        end
                    else m1
                end;
        in
            N(expr1, block1, m)
        end;

(* forLoop *)

  | M( itree(inode("forLoop", _),
        [
            itree(inode("for", _), [] ),
            itree(inode("(", _), [] ),
            assignment1,
            itree(inode(";", _), [] ),
            expr1
            itree(inode(";", _), [] ),
            assignment2,
            itree(inode(")", _), [] ),
            block1
        ]
    ), m) =
        let
            val m1 = M(assignment1, m)
            fun N(cond, iter, block, m) = 
                let
                    val (v, m1) = E(cond, m)
                in
                    if v then 
                        let              
                            val m2 = M(block, m1)    
                            val m3 = M(iter, m2)
                            val m4 = N(cond, iter, block, m3)
                        in
                            m4
                        end;   
                    else m1
                end;
        in
            N(expr1, assignment2, block1, m1)
        end;
        
  | M( itree(inode(x_root, _), children), _) = raise General.Fail("\n\nIn M root = " ^ x_root ^ "\n\n")
  | M _ = raise Fail("error in Semantics.M - this should never occur")

(* =========================================================================================================== *)
end; (* struct *)
(* =========================================================================================================== *)