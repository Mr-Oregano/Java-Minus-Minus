(* statementList  *)

M([[ ε ]], m) = m
M([[ statement₁ statementList₁ ]], m) = 
    let
        val m1 = M( statement₁, m )
        val m2 = M( statementList₁, m1 )
    in
        m2
    end

(* statement: *)

M([[ block₁ ]]) = M(block₁, m)
M([[ assignment₁ ";" ]], m) = M(assignment₁, m)
M([[ decl₁ ";"]], m) = M(decl₁, m) = M(decl₁, m)
M([[ "if" "(" expr₁ ")" block₁ ]], m) =
    let
        val (v, m1) = E'(expr₁, m)
    in
        if v then M(block₁, m1)
        else m1
    end

M([[ "if" "(" expr₁ ")" block₁ "else" block₂ ]], m) = 
    let
        val (v, m1) = E'(expr₁, m)
    in
        if v then M(block₁, m1)
        else M(block₂, m1)
    end

M([[ whileLoop₁ ]], m) = M(whileLoop₁, m)
M([[ forLoop₁ ]], m) = M(forLoop₁, m)
M([[ "print" "(" expr₁ ")" ";" ]], m) = 
    let 
        val (v, m1) = E'(expr₁, m)
        print(v) (* Assume print is function *)
    in
        m1
    end

M([[ ";" ]], m) = m

(* assignment *)

M([[ IDENTIFIER "=" expr₁ ]], m) = 
    let
        val (v, m1) = E'(expr₁, m)
        val loc = getLoc(accessEnv(IDENTIFIER, m))
        val m2 = updateStore(loc, v, m1)
    in
        m2
    end

M([[ decID₁ ";" ]], m) = 
    let 
        val (v, m1) = E'(decID₁, m)
    in
        m1
    end

(* declaration *)

M([[ "bool" IDENTIFIER ]], m) = updateEnv(IDENTIFIER, BOOL, new(), m)
M([[ "int" IDENTIFIER]], m) = updateEnv(IDENTIFIER, INT, new(), m)

(* block *)

M([[ "{" statementList₁ "}" ]], m) = M(statementList₁, m)

(* whileLoop *)

M([[ "while" "(" expr₁ ")" block₁ ]], m)
    let
        val (v, m1) = E'( expr₁, m )    
    in
        if v then 
            let              
                val m2 = M( block₁, m1 )    
                val m3 = M( [[ "while" "(" expr₁ ")" block₁ ]], m2 )           
            in
                m1
            end   
        else m1
    end

(* forLoop *)

M([[ "for" "(" forInit₁ ";" forCond₁ ";" forIter₁ ")" block₁ ]], m)
    let
        val m1 = M(forInit₁, m)
    in
        N(forCond₁, forIter₁ ,block₁, m1)
    end
    
N(cond, iter, block, m)
    let
        val (v, m1) = E'( cond, m )
    in
        if v then 
            let              
                val m2 = M( block, m1 )    
                val m3 = M( iter, m2 )
                val m4 = N( cond, iter, block, m3 )
            in
                m1
            end   
        else m1
    end

M(forInit₁, m) = M([[ assignment₁ ]], m)
E'(forCond₁, m) = E'([[ expr₁ ]], m)
M(forIter₁, m) = M([[ assignment₁ ]], m)
M(forIter₁, m) = M([[ decID₁ ]], m)

(* expr: *)

E'([[ expr₁ "or" andExpr₁ ]], m) = 
    let 
        val (v1, m1) = E'(expr₁, m)
    in
        if v1 then (v1, m1)
        else E'(andExpr₁, m1)
    end

E'([[ andExpr₁ ]], m) = E'(andExpr₁, m)

(* andExpr *)

E'([[ andExpr₁ "and" equalExpr₁ ]], m) =
    let
        val (v1, m1) = E'(andExpr₁, m)
    in
        if not v1 then (v1, m1) 
        else E'(equalExpr₁, m1)
    end

E'([[ equalExpr₁ ]], m) = E'(equalExpr₁, m)

(* equalExpr *)

E'([[ equalExpr₁ "==" relExpr₁ ]], m) =
    let
        val (v1, m1) = E'(equalExpr₁, m)
        val (v2, m2) = E'(relExpr₁, m1)
    in
        (v1 = v2, m2)
    end

E'([[ equalExpr₁ "!=" relExpr₁ ]], m) =  
    let
        val (v1, m1) = E'(equalExpr₁, m)
        val (v2, m2) = E'(relExpr₁, m1)
    in
        (v1 <> v2, m2)
    end

E'([[ relExpr₁ ]], m) = E'(relExpr₁, m)
    
(* relExpr *)

E'([[ relExpr₁ "<" sumExpr₁ ]], m) =
    let
        val (v1, m1) = E'(relExpr₁, m)
        val (v2, m2) = E'(sumExpr₁, m1)
    in
        (v1 < v2, m2)
    end

E'([[ relExpr₁ ">" sumExpr₁ ]], m) =
    let
        val (v1, m1) = E'(relExpr₁, m)
        val (v2, m2) = E'(sumExpr₁, m1)
    in
        (v1 > v2, m2)
    end

E'([[ sumExpr₁ ]], m) = E'(sumExpr₁, m)

(* sumExpr *)

E'([[ sumExpr₁ "+" mulExpr₁ ]], m ) = 
    let
        val (v1, m1) = E'(sumExpr₁, m)
        val (v2, m2) = E'(mulExpr₁, m1)
    in
        (v1 + v2, m2)
    end

E'([[ sumExpr₁ "-" mulExpr₁ ]], m ) = 
    let
        val (v1, m1) = E'(sumExpr₁, m)
        val (v2, m2) = E'(mulExpr₁, m1)
    in
        (v1 - v2, m2)
    end

E'([[ mulExpr₁ ]], m ) = E'(mulExpr₁, m)

(* mulExpr *)

E'([[ mulExpr₁ "*" unaryExpr₁ ]], m ) = 
    let
        val (v1, m1) = E'(mulExpr₁, m)
        val (v2, m2) = E'(unaryExpr₁, m1)
    in
        (v1 * v2, m2)
    end

E'([[ mulExpr₁ "/" unaryExpr₁ ]], m ) = 
    let
        val (v1, m1) = E'(mulExpr₁, m)
        val (v2, m2) = E'(unaryExpr₁, m1)
    in
        (v1 div v2, m2)
    end

E'([[ mulExpr₁ "%" unaryExpr₁ ]], m ) = 
    let
        val (v1, m1) = E'(mulExpr₁, m)
        val (v2, m2) = E'(unaryExpr₁, m1)
    in
        (v1 mod v2, m2)
    end

E'([[ unaryExpr₁ ]], m ) = E'(unaryExpr₁, m)
    
(* unaryExpr *)

E'([[ "abs" unaryExpr₁ ]], m ) = 
    let
        val (v1, m1) = E'(unaryExpr₁, m)
    in
        (abs(v1), m1) (* Assume abs is a function *)
    end

E'([[ "not" unaryExpr₁ ]], m ) = 
    let
        val (v1, m1) = E'(unaryExpr₁, m1)
    in
        (not v1, m1) 
    end

E'([[ "~" unaryExpr₁ ]], m ) = 
    let
        val (v1, m1) = E'(unaryExpr₁, m1)
    in
        (~v1, m2)
    end

E'([[ expExpr₁ ]], m ) = E'(expExpr₁, m)

(* expExpr *)
    
E'([[ factor₁ "^" expExpr₁ ]], m ) = 
    let
        val (v1, m1) = E'(factor₁, m)
        val (v2, m2) = E'(expExpr₁, m1)
    in
        (exp(v1, v2), m2) (* Assume exp is a function *)
    end

E'([[ factor₁ ]], m ) = E'(factor₁, m)

(* factor *)

E'([[ IDENTIFIER ]], m ) = 
    let 
        val loc = getLoc(accessEnv(IDENTIFIER, m))
        val v = accessStore(loc, m)
    in
        (v, m)
    end

E'([[ "true" ]], m ) = true
E'([[ "false" ]], m ) = false
E'([[ INT_LITERAL ]], m ) = INT_LITERAL
E'([[ "(" expr₁ ")" ]], m ) = E'(expr₁)
E'([[ decID₁ ]], m) = E'(decID₁, m)

(* decID *)

E'([[ "++" IDENTIFIER ]], m) = 
    let 
        val loc = getLoc(accessEnv(IDENTIFIER, m))
        val v = accessStore(loc, m)
        val v2 = v + 1
        val m1 = updateStore(loc, v2, m)
    in
        (v2, m1)
    end

E'([[ "--" IDENTIFIER ]], m) = 
    let 
        val loc = getLoc(accessEnv(IDENTIFIER, m))
        val v = accessStore(loc, m)
        val v2 = v - 1
        val m1 = updateStore(loc, v - 1, m)
    in
        (v2, m1)
    end

E'([[  IDENTIFIER "++" ]], m) = 
    let 
        val loc = getLoc(accessEnv(IDENTIFIER, m))
        val v = accessStore(loc, m)
        val v2 = v + 1
        val m1 = updateStore(loc, v2, m)
    in
        (v, m1)
    end

E'([[  IDENTIFIER "--" ]], m) = 
    let 
        val loc = getLoc(accessEnv(IDENTIFIER, m))
        val v = accessStore(loc, m)
        val v2 = v - 1
        val m1 = updateStore(loc, v - 1, m)
    in
        (v, m1)
    end