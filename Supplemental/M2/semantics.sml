(* statementList  *)

M([[  ]], m) = m
M([[ statement1 statementList1 ]], m) = 
    let
        val m1 = M(statement1, m)
        val m2 = M(statementList1, m1)
    in
        m2
    end

(* statement: *)

M([[ block1 ]]) = M(block1, m)
M([[ assignment1 ; ]], m) = M(assignment1, m)
M([[ decl1 ;]], m) = M(decl1, m) = M(decl1, m)
M([[ if ( expr1 ) block1 ]], m) =
    let
        val (v, m1) = E'(expr1, m)
    in
        if v then M(block1, m1)
        else m1
    end

M([[ if ( expr1 ) block1 else block2 ]], m) = 
    let
        val (v, m1) = E'(expr1, m)
    in
        if v then M(block1, m1)
        else M(block2, m1)
    end

M([[ whileLoop1 ]], m) = M(whileLoop1, m)
M([[ forLoop1 ]], m) = M(forLoop1, m)
M([[ print ( expr1 ) ; ]], m) = 
    let 
        val (v, m1) = E'(expr1, m)
        print(v) (* Assume print is function *)
    in
        m1
    end

M([[ ; ]], m) = m

(* assignment *)

M([[ IDENTIFIER = expr1 ]], m) = 
    let
        val (v, m1) = E'(expr1, m)
        val loc = getLoc(accessEnv(IDENTIFIER, m))
        val m2 = updateStore(loc, v, m1)
    in
        m2
    end

M([[ decID1 ; ]], m) = 
    let 
        val (v, m1) = E'(decID1, m)
    in
        m1
    end

(* declaration *)

M([[ bool IDENTIFIER ]], m) = updateEnv(IDENTIFIER, BOOL, new(), m)
M([[ int IDENTIFIER]], m) = updateEnv(IDENTIFIER, INT, new(), m)

(* block *)

M([[ { statementList1 } ]], (env, str)) = 
	let
		val (env1, str1) = M(statementList1, (env, str))
		val m1 = (env, str1)
	in
		m1
	end

(* whileLoop *)

M([[ while ( expr1 ) block1 ]], m)
    let
        val (v, m1) = E'(expr1, m)    
    in
        if v then 
            let              
                val m2 = M(block1, m1)    
                val m3 = M([[ while ( expr1 ) block1 ]], m2)           
            in
                m3
            end   
        else m1
    end

(* forLoop *)

M([[ for ( assignment1 ; expr1 ; assignment2 ) block1 ]], m)
    let
        val m1 = M(assignment1, m)
    in
        N(expr1, assignment2, block1, m1)
    end
    
N(cond, iter, block, m)
    let
        val (v, m1) = E'(cond, m)
    in
        if v then 
            let              
                val m2 = M(block, m1)    
                val m3 = M(iter, m2)
                val m4 = N(cond, iter, block, m3)
            in
                m4
            end   
        else m1
    end

(* expr: *)

E'([[ expr1 or andExpr1 ]], m) = 
    let 
        val (v1, m1) = E'(expr1, m)
    in
        if v1 then (v1, m1)
        else E'(andExpr1, m1)
    end

E'([[ andExpr1 ]], m) = E'(andExpr1, m)

(* andExpr *)

E'([[ andExpr1 and equalExpr1 ]], m) =
    let
        val (v1, m1) = E'(andExpr1, m)
    in
        if not v1 then (v1, m1) 
        else E'(equalExpr1, m1)
    end

E'([[ equalExpr1 ]], m) = E'(equalExpr1, m)

(* equalExpr *)

E'([[ equalExpr1 == relExpr1 ]], m) =
    let
        val (v1, m1) = E'(equalExpr1, m)
        val (v2, m2) = E'(relExpr1, m1)
    in
        (v1 = v2, m2)
    end

E'([[ equalExpr1 != relExpr1 ]], m) =  
    let
        val (v1, m1) = E'(equalExpr1, m)
        val (v2, m2) = E'(relExpr1, m1)
    in
        (v1 <> v2, m2)
    end

E'([[ relExpr1 ]], m) = E'(relExpr1, m)
    
(* relExpr *)

E'([[ relExpr1 < sumExpr1 ]], m) =
    let
        val (v1, m1) = E'(relExpr1, m)
        val (v2, m2) = E'(sumExpr1, m1)
    in
        (v1 < v2, m2)
    end

E'([[ relExpr1 > sumExpr1 ]], m) =
    let
        val (v1, m1) = E'(relExpr1, m)
        val (v2, m2) = E'(sumExpr1, m1)
    in
        (v1 > v2, m2)
    end

E'([[ sumExpr1 ]], m) = E'(sumExpr1, m)

(* sumExpr *)

E'([[ sumExpr1 + mulExpr1 ]], m) = 
    let
        val (v1, m1) = E'(sumExpr1, m)
        val (v2, m2) = E'(mulExpr1, m1)
    in
        (v1 + v2, m2)
    end

E'([[ sumExpr1 - mulExpr1 ]], m) = 
    let
        val (v1, m1) = E'(sumExpr1, m)
        val (v2, m2) = E'(mulExpr1, m1)
    in
        (v1 - v2, m2)
    end

E'([[ mulExpr1 ]], m) = E'(mulExpr1, m)

(* mulExpr *)

E'([[ mulExpr1 * unaryExpr1 ]], m) = 
    let
        val (v1, m1) = E'(mulExpr1, m)
        val (v2, m2) = E'(unaryExpr1, m1)
    in
        (v1 * v2, m2)
    end

E'([[ mulExpr1 / unaryExpr1 ]], m) = 
    let
        val (v1, m1) = E'(mulExpr1, m)
        val (v2, m2) = E'(unaryExpr1, m1)
    in
        (v1 div v2, m2)
    end

E'([[ mulExpr1 % unaryExpr1 ]], m) = 
    let
        val (v1, m1) = E'(mulExpr1, m)
        val (v2, m2) = E'(unaryExpr1, m1)
    in
        (v1 mod v2, m2)
    end

E'([[ unaryExpr1 ]], m) = E'(unaryExpr1, m)
    
(* unaryExpr *)

E'([[ abs unaryExpr1 ]], m) = 
    let
        val (v1, m1) = E'(unaryExpr1, m)
    in
        (abs(v1), m1) (* Assume abs is a function *)
    end

E'([[ not unaryExpr1 ]], m) = 
    let
        val (v1, m1) = E'(unaryExpr1, m1)
    in
        (not v1, m1) 
    end

E'([[ ~ unaryExpr1 ]], m) = 
    let
        val (v1, m1) = E'(unaryExpr1, m1)
    in
        (~v1, m2)
    end

E'([[ expExpr1 ]], m) = E'(expExpr1, m)

(* expExpr *)
    
E'([[ factor1 ^ expExpr1 ]], m) = 
    let
        val (v1, m1) = E'(factor1, m)
        val (v2, m2) = E'(expExpr1, m1)
    in
        (exp(v1, v2), m2) (* Assume exp is a function *)
    end

E'([[ factor1 ]], m) = E'(factor1, m)

(* factor *)

E'([[ IDENTIFIER ]], m) = 
    let 
        val loc = getLoc(accessEnv(IDENTIFIER, m))
        val v = accessStore(loc, m)
    in
        (v, m)
    end

E'([[ true ]], m) = (true, m)
E'([[ false ]], m) = (false, m)
E'([[ INT_LITERAL ]], m) = (INT_LITERAL, m)
E'([[ ( expr1 ) ]], m) = E'(expr1)
E'([[ decID1 ]], m) = E'(decID1, m)

(* decID *)

E'([[ ++ IDENTIFIER ]], m) = 
    let 
        val loc = getLoc(accessEnv(IDENTIFIER, m))
        val v = accessStore(loc, m)
        val v2 = v + 1
        val m1 = updateStore(loc, v2, m)
    in
        (v2, m1)
    end

E'([[ -- IDENTIFIER ]], m) = 
    let 
        val loc = getLoc(accessEnv(IDENTIFIER, m))
        val v = accessStore(loc, m)
        val v2 = v - 1
        val m1 = updateStore(loc, v2, m)
    in
        (v2, m1)
    end

E'([[  IDENTIFIER ++ ]], m) = 
    let 
        val loc = getLoc(accessEnv(IDENTIFIER, m))
        val v = accessStore(loc, m)
        val v2 = v + 1
        val m1 = updateStore(loc, v2, m)
    in
        (v, m1)
    end

E'([[  IDENTIFIER -- ]], m) = 
    let 
        val loc = getLoc(accessEnv(IDENTIFIER, m))
        val v = accessStore(loc, m)
        val v2 = v - 1
        val m1 = updateStore(loc, v2, m)
    in
        (v, m1)
    end