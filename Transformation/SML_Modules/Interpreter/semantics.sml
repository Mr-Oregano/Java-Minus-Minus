structure Semantics =
struct

open Model; 
open CONCRETE_REPRESENTATION;

(* =========================================================================================================== *)

(* NOTE: utility power function*)
fun pow(x, 0) = 1
  | pow(x, y) = x * pow(x, y - 1)

(* expr: *)
fun E( itree(inode("expr", _), 
        [ 
            expr1,
            itree(inode("or", _), [] ),
            andExpr1
        ]
    ), m) =
        let 
            val (v1, m1) = E(expr1, m)
            val b1 = getBool(v1)
        in
            if b1 then (v1, m1)
            else E(andExpr1, m1)
        end
        
  | E( itree(inode("expr", _), [ andExpr1 ]), m) = E(andExpr1, m)

(* andExpr *)

  | E( itree(inode("andExpr", _),
        [
            andExpr1,
            itree(inode("and", _), []),
            xorExpr1
        ]
    ), m) =
        let
            val (v1, m1) = E(andExpr1, m)
            val b1 = getBool(v1)
        in
            if not b1 then (v1, m1) 
            else E(xorExpr1, m1)
        end

  | E( itree(inode("andExpr", _), [ xorExpr1 ]), m) = E(xorExpr1, m)

(* xorExpr *)

(* NOTES: OR and AND were both short circuited as we can determine
          the result based on just one of the expressions. XOR, however,
          requires both expressions to be evaluated. *)

  | E( itree(inode("xorExpr", _),
        [
            xorExpr1,
            itree(inode("xor", _), []),
            equalExpr1
        ]
    ), m) =
        let
            val (v1, m1) = E(xorExpr1, m)
            val (v2, m2) = E(equalExpr1, m1)
            val b1 = getBool(v1)
            val b2 = getBool(v2)
        in
            (Boolean (b1 <> b2), m2) 
        end

  | E( itree(inode("xorExpr", _), [ equalExpr1 ]), m) = E(equalExpr1, m)

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
            (Boolean (v1 = v2), m2)
        end

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
           (Boolean (v1 <> v2), m2)
        end

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
            val i1 = getInt(v1)
            val i2 = getInt(v2)
        in
            (Boolean (i1 < i2), m2)
        end
        
  | E( itree(inode("relExpr", _), 
        [
            relExpr1,
            itree(inode(">", _), [] ),
            sumExpr1
        ]
    ), m) =
        let
            val (v1, m1) = E(relExpr1, m)
            val (v2, m2) = E(sumExpr1, m1)
            val i1 = getInt(v1)
            val i2 = getInt(v2)
        in
            (Boolean (i1 > i2), m2)
        end
  | E( itree(inode("relExpr", _), 
        [
            relExpr1,
            itree(inode("<=", _), [] ),
            sumExpr1
        ]
    ), m) =
        let
            val (v1, m1) = E(relExpr1, m)
            val (v2, m2) = E(sumExpr1, m1)
            val i1 = getInt(v1)
            val i2 = getInt(v2)
        in
            (Boolean (i1 <= i2), m2)
        end
  | E( itree(inode("relExpr", _), 
        [
            relExpr1,
            itree(inode(">=", _), [] ),
            sumExpr1
        ]
    ), m) =
        let
            val (v1, m1) = E(relExpr1, m)
            val (v2, m2) = E(sumExpr1, m1)
            val i1 = getInt(v1)
            val i2 = getInt(v2)
        in
            (Boolean (i1 >= i2), m2)
        end

  | E( itree(inode("relExpr", _), [ sumExpr1 ]), m) = E(sumExpr1, m)

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
            val tp1 = typeDVToEnum(v1)
            val tp2 = typeDVToEnum(v2)
        in
            if tp1 = tp2 andalso tp1 = INT then
                let
                    val i1 = getInt(v1)
                    val i2 = getInt(v2)
                in
                    (Integer (i1 + i2), m2)
                end
            else
                let
                    val s1 = dvToString(v1) 
                    val s2 = dvToString(v2) 
                in
                    (String (s1 ^ s2), m2)
                end
        end
        
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
            val i1 = getInt(v1)
            val i2 = getInt(v2)
        in
            (Integer (i1 - i2), m2)
        end

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
            val i1 = getInt(v1)
            val i2 = getInt(v2)
        in
            (Integer (i1 * i2), m2)
        end

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
            val i1 = getInt(v1)
            val i2 = getInt(v2)
        in
            (Integer (i1 div i2), m2)
        end

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
            val i1 = getInt(v1)
            val i2 = getInt(v2)
        in
            (Integer (i1 mod i2), m2)
        end

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
            val i1 = getInt(v1)
        in
            (Integer (abs i1), m1)
        end

  | E( itree(inode("unaryExpr", _), 
        [ 
            itree(inode("not", _), [] ),
            unaryExpr1
        ]
    ), m) =
        let
            val (v1, m1) = E(unaryExpr1, m)
            val b1 = getBool(v1)
        in
            (Boolean (not b1), m1) 
        end

  | E( itree(inode("unaryExpr", _), 
        [ 
            itree(inode("~", _), [] ),
            unaryExpr1
        ]
    ), m) =
        let
            val (v1, m1) = E(unaryExpr1, m)
            val i1 = getInt(v1)
        in
            (Integer (~i1), m1)
        end

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
            val i1 = getInt(v1)
            val i2 = getInt(v2)
        in
            (Integer (pow(i1, i2)), m2)
        end

  | E( itree(inode("expExpr", _), [ factor1 ]), m) = E(factor1, m)

(* factor *)

  | E( itree(inode("factor", _), [ itree(inode("true", _), [] ) ]), m) = (Boolean true, m)
  | E( itree(inode("factor", _), [ itree(inode("false", _), [] ) ]), m) = (Boolean false, m)
  | E( itree(inode("factor", _), [ ID as itree(inode("IDENTIFIER", _), [ _ ] ) ]), m) = 
        let
            val idName  = getLeaf(ID)
            val loc = getLoc(accessEnv(idName, m))
            val v = accessStore(loc, m)
        in
            (v, m)
        end

  | E( itree(inode("factor", _), [ STR_LITERAL as itree(inode("STR_LITERAL", _), [ _ ] ) ]), m) = 
        let
            val literal = getLeaf(STR_LITERAL)
            val v = valOf(String.fromString literal)
        in
            (String v, m)
        end

  | E( itree(inode("factor", _), [ INT_LITERAL as itree(inode("INT_LITERAL", _), [ _ ] ) ]), m) = 
        let
            val literal = getLeaf(INT_LITERAL)
            val v = valOf(Int.fromString literal)
        in
            (Integer v, m)
        end
    
  | E( itree(inode("factor", _), [ child ]), m) = E(child, m)
  | E( itree(inode("factor", _), 
        [ 
            itree(inode("(", _), [] ),
            expr1,
            itree(inode(")", _), [] )
        ]
    ), m) = E(expr1, m)

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
            val v2 = Integer (getInt(v) + 1)
            val m1 = updateStore(loc, v2, m)
        in
            (v2, m1)
        end

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
            val v2 = Integer (getInt(v) - 1)
            val m1 = updateStore(loc, v2, m)
        in
            (v2, m1)
        end

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
            val v2 = Integer (getInt(v) + 1)
            val m1 = updateStore(loc, v2, m)
        in
            (v, m1)
        end

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
            val v2 = Integer (getInt(v) - 1)
            val m1 = updateStore(loc, v2, m)
        in
            (v, m1)
        end

(* Fails *)

  | E( TREE as itree(inode(x_root, _), children), _) = raise General.Fail("\n\nIn E() root = " ^ x_root ^ "\n\tHere ---->\t\"" ^ getLeaf(TREE) ^ "\"\n\n")
  | E _ = raise Fail("Error in Semantics.E - this should never occur")

(* =========================================================================================================== *)

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
        end

(* Statement *)

  | M( itree(inode("statement", _), [ child ] ), m) = M(child, m) 

(* Open & Closed statements *)

  | M( itree(_,
        [
            itree(inode("if", _), [] ),
            itree(inode("(", _), [] ),
            expr1,
            itree(inode(")", _), [] ),
            stmt1
        ]
    ), m) = 
        let
            val (v, m1 as (env, c, str)) = E(expr1, m)
            val b = getBool(v)
            val (env1, c1, str1) = if b then M(stmt1, m1) else m1
        in
            (env, c, str1)
        end

  | M( itree(_,
        [
            itree(inode("if", _), [] ),
            itree(inode("(", _), [] ),
            expr1,
            itree(inode(")", _), [] ),
            stmt1,
            itree(inode("else", _), [] ),
            stmt2
        ]
    ), m) = 
        let
            val (v, m1 as (env, c, str)) = E(expr1, m)
            val b = getBool(v)
            val (env1, c1, str1) = if b then M(stmt1, m1) else M(stmt2, m1)
        in
            (env, c, str1)
        end
        
  | M( itree(_,
        [
            itree(inode("while", _), [] ),
            itree(inode("(", _), [] ),
            expr1,
            itree(inode(")", _), [] ),
            stmt1
        ]
    ), m as (env, c, str)) = 
        let 
            fun N(cond, stmt, m) = 
                let
                    val (v, m1) = E(cond, m)
                    val b = getBool(v)
                in
                    if b then 
                        let              
                            val m2 = M(stmt, m1)    
                            val m3 = N(cond, stmt, m2)    
                        in
                            m3
                        end
                    else m1
                end

            val (env1, c1, str1) = N(expr1, stmt1, m)
        in
            (env, c, str1)
        end
        
  | M( itree(_,
        [
            itree(inode("for", _), [] ),
            itree(inode("(", _), [] ),
            forInit1,
            itree(inode(";", _), [] ),
            expr1,
            itree(inode(";", _), [] ),
            assignment1,
            itree(inode(")", _), [] ),
            stmt1
        ]
    ), m as (env, c, str)) = 
        let
            fun N(cond, stmt, m, iter) = 
                let
                    val (v, m1) = E(cond, m)
                    val b = getBool(v)
                in
                    if b then 
                        let              
                            val m2 = M(stmt, m1)    
                            val m3 = M(iter, m2)
                            val m4 = N(cond, stmt, m3, iter)
                        in
                            m4
                        end   
                    else m1
                end

            val m1 = M(forInit1, m)
            val (env1, c1, str1) = N(expr1, stmt1, m1, assignment1)
        in
            (env, c, str1)
        end

  | M( itree(inode("closedStatement", _), [ child ] ), m) = M(child, m) 

(* Basic Statements *)

  | M( itree(inode("simpleStatement", _), [ itree(inode(";", _), [] ) ]), m) = m
  | M( itree(inode("simpleStatement", _), [ child ]), m) = M(child, m)
  | M( itree(inode("simpleStatement", _),
        [
            child,
            itree(inode(";", _), [] )
        ]
    ), m) = M(child, m)

  | M( itree(inode("simpleStatement", _),
        [
            itree(inode("print", _), [] ),
            itree(inode("(", _), [] ),
            expr1,
            itree(inode(")", _), [] ),
            itree(inode(";", _), [] )
        ]
    ), m) = 
        let 
            val (v, m1) = E(expr1, m)
        in
            print(dvToString(v));
            m1
        end
  | M( itree(inode("simpleStatement", _),
        [
            itree(inode("println", _), [] ),
            itree(inode("(", _), [] ),
            expr1,
            itree(inode(")", _), [] ),
            itree(inode(";", _), [] )
        ]
    ), m) = 
        let 
            val (v, m1) = E(expr1, m)
        in
            print(dvToString(v) ^ "\n");
            m1
        end

(* assignment *)

  | M( itree(inode("assignment", _),
        [
            id1,
            itree(inode("=", _), [] ),
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
        end

  | M( itree(inode("assignment", _), [ decoratedID1 ]), m) = 
        let 
            val (v, m1) = E(decoratedID1, m)
        in
            m1
        end

(* initialization *)

  | M( itree(inode("init", _),
        [
            type1,
            id1,
            itree(inode("=", _), [] ),
            expr1
        ]
    ), m) = 
        let
            val (v, m1) = E(expr1, m)
            val tp = typeStrToEnum(getLeaf(type1))
            val idName = getLeaf(id1)
            val m2 = updateEnv(idName, tp, m1)
            val loc = getLoc(accessEnv(idName, m2))
            val m3 = updateStore(loc, v, m2)
        in
            m3
        end
        
  | M( itree(inode("decl", _),
        [
            type1,
            id1
        ]
    ), m) = 
        let
            val tp = typeStrToEnum(getLeaf(type1))
            val idName  = getLeaf(id1)
        in
            updateEnv(idName, tp, m)
        end

  | M( itree(inode("forInit", _), [ child ]), m) = M(child, m)

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
        end

(* Fails *)  

  | M( TREE as itree(inode(x_root, _), children), _) = raise General.Fail("\n\nIn M() root = " ^ x_root ^ "\n\tHere ---->\t\"" ^ getLeaf(TREE) ^ "\"\n\n")
  | M _ = raise Fail("error in Semantics.M - this should never occur")

(* =========================================================================================================== *)
end; (* struct *)
(* =========================================================================================================== *)