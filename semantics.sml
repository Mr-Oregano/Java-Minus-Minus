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

    M([[ incStmt₁ ";" ]], m) = M(incStmt₁)
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
            print(v)
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

    M(forInit₁, m) = M([[ assignment ]], m)
    E'(forCond₁, m) = E'([[ expr ]], m)
    M(forIter₁, m) = M([[ assignment ]], m)
    M(forIter₁, m) = M([[ incStmt ]], m)

(* expr: *)
    E'( [[ expr₁ "or" andExpr₁ ]], m) = 
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

    E'([[equalExpr₁]], m) = E'(equalExpr₁, m)

(* equalExpr *)
    E'([[ equalExpr equalOp relExpr]], m) =
        let
            val (v1, m1) = E'(equalExpr₁, m)
            val (v2, m2) = E'(equalOp₁, m)
            val (v3, m3) = E'(relExpr₁, m)
        in
            (v1 v2 v3, m3)
        end

(* equalOp *)
    E'([["=="]], m) = E'("==", m)

    E'([[ "!="]], m) = E'("==", m)
    
(* relExpr *)
    E'([[ relExpr₁ relOp₁ sumExpr₁ ]], m) =
        let
            val (v1, m1) = E'(relExpr₁, m)
            val (v2, m2) = E'(relOp₁, m1)
            val (v3, m3) = E'(sumExpr₁, m2)
        in
            (v1 v2 v3, m3)
        end
    E'([[ sumExpr₁ ]], m) = E'(sumExpr₁, m)

(* relOp *)
    E'([[ "<" ]], m) = E'("<", m)
    E'([[ ">" ]], m) = E'(">", m)

(* sumExpr *)
    E'( [[ expr₁ + term1 ]], m0 ) = 
    let
        val (v1, m1) = E'( expr1, m0 ) 
        val (v2, m2) = E'( term1, m1 )       
    in
        (v1 + v2, m2) 
    end
    
(* addOp *)
    E'([["+"]], m) = E'("+", m)
    E'([["-"]], m) = E'("-", m)

(* mulExpr *)
    
(* mulOp *)
    E'([["*"]], m) = E'("*", m)
    E'([["/"]], m) = E'("/", m)
    E'([["%"]], m) = E'("%", m)

(* unaryExpr *)

(* unaryOp *)
    E'([["abs"]], m) = E'("abs", m)
    E'([["=="]], m) = E'("==", m)
    E'([["=="]], m) = E'("==", m)

(* expExpr *)

(* factor *)










