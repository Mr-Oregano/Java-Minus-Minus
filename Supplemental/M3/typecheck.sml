(* statementList  *)

typeCheck([[  ]], m) = m
typeCheck([[ statement1 statementList1 ]], m) = 
    let
        val m1 = typeCheck(statement1, m)
        val m2 = typeCheck(statementList1, m1)
    in
        m2
    end

(* statement: *)

typeCheck([[ block1 ]]) = typeCheck(block1, m)
typeCheck([[ assignment1 ; ]], m) = typeCheck(assignment1, m)
typeCheck([[ decl1 ; ]], m) = typeCheck(decl1, m) = typeCheck(decl1, m)
typeCheck([[ if ( expr1 ) block1 ]], m) =
    let
        val t1 = typeOf(expr1, m)
		val m1 = typeCheck(block1, m)
    in
        if t1 = BOOL then m1 else raise model_error
    end

typeCheck([[ if ( expr1 ) block1 else block2 ]], m) = 
    let
        val t1 = typeOf(expr1, m)
        val m1 = typeCheck(block1, m)
        val m2 = typeCheck(block2, m1)
    in
        if t1 = BOOL then m2 else raise model_error
    end

typeCheck([[ whileLoop1 ]], m) = typeCheck(whileLoop1, m)
typeCheck([[ forLoop1 ]], m) = typeCheck(forLoop1, m)
typeCheck([[ print ( expr1 ) ; ]], m) = 
    let 
        val t1 = typeOf(expr1, m)
    in
        if t1 <> ERROR then m else raise model_error
    end

typeCheck([[ ; ]], m) = m

(* assignment *)

typeCheck([[ IDENTIFIER = expr1 ]], m) = 
    let
        val t1 = typeOf(expr1, m)
        val t2 = getType(accessEnv(IDENTIFIER, m))
    in
        if t1 = t2 then m else raise model_error
    end

typeCheck([[ decID1 ; ]], m) = 
	let 
		val t1 = typeOf(decID1)
	in
		if t1 <> ERROR then m else raise model_error
	end

(* declaration *)

typeCheck([[ bool IDENTIFIER ]], m) = updateEnv(IDENTIFIER, BOOL, new(), m)
typeCheck([[ int IDENTIFIER]], m) = updateEnv(IDENTIFIER, INT, new(), m)

(* block *)

(* DOUBLE CHECK *)
typeCheck([[ { statementList1 } ]], m) = 
	let
		val m1 = typeCheck(statementList1, m)
	in 
		m
	end

(* whileLoop *)

typeCheck([[ while ( expr1 ) block1 ]], m)
    let
        val t1 = typeOf(expr1, m)    
		val m1 = typeCheck(block1, m)
    in
        if t1 = BOOL then m1 else raise model_error
    end

(* forLoop *)

typeCheck([[ for ( assignment1 ; expr1 ; assignment2 ) block1 ]], m)
    let
        val m1 = typeCheck(assignment1, m)    
        val t1 = typeOf(expr1, m1)
        val m2 = typeCheck(assignment1, m1)    
		val m3 = typeCheck(block1, m2)
    in
        if t1 = BOOL then m3 else raise model_error
    end

(* expr: *)

typeOf([[ expr1 or andExpr1 ]], m) = 
    let 
        val t1 = typeOf(expr1, m)
        val t2 = typeOf(andExpr1, m)
    in
        if t1 = t2 andalso t1 = BOOL then BOOL 
		else ERROR
    end

typeOf([[ andExpr1 ]], m) = typeOf(andExpr1, m)

(* andExpr *)

typeOf([[ andExpr1 and equalExpr1 ]], m) =
    let 
        val t1 = typeOf(andExpr1, m)
        val t2 = typeOf(equalExpr1, m)
    in
        if t1 = t2 andalso t1 = BOOL then BOOL 
		else ERROR
    end

typeOf([[ equalExpr1 ]], m) = typeOf(equalExpr1, m)

(* equalExpr *)

typeOf([[ equalExpr1 == relExpr1 ]], m) =
    let 
        val t1 = typeOf(equalExpr1, m)
        val t2 = typeOf(relExpr1, m)
    in
        if t1 = t2 then t1 else ERROR
    end

typeOf([[ equalExpr1 != relExpr1 ]], m) =  
    let
        val t1 = typeOf(equalExpr1, m)
        val t2 = typeOf(relExpr1, m1)
    in
        if t1 = t2 then t1 else ERROR
    end

typeOf([[ relExpr1 ]], m) = typeOf(relExpr1, m)
    
(* relExpr *)

typeOf([[ relExpr1 < sumExpr1 ]], m) =
    let
        val t1 = typeOf(relExpr1, m)
        val t2 = typeOf(sumExpr1, m1)
    in
        if t1 = t2 andalso t1 = INT then INT 
		else ERROR
    end

typeOf([[ relExpr1 > sumExpr1 ]], m) =
    let
        val t1 = typeOf(relExpr1, m)
        val t2 = typeOf(sumExpr1, m1)
    in
        if t1 = t2 andalso t1 = INT then INT 
		else ERROR
    end

typeOf([[ sumExpr1 ]], m) = typeOf(sumExpr1, m)

(* sumExpr *)

typeOf([[ sumExpr1 + mulExpr1 ]], m) = 
    let
        val t1 = typeOf(sumExpr1, m)
        val t2 = typeOf(mulExpr1, m1)
    in
        if t1 = t2 andalso t1 = INT then INT
        else ERROR
    end

typeOf([[ sumExpr1 - mulExpr1 ]], m) = 
    let
        val t1 = typeOf(sumExpr1, m)
        val t2 = typeOf(mulExpr1, m1)
    in
        if t1 = t2 andalso t1 = INT then INT
        else ERROR
    end

typeOf([[ mulExpr1 ]], m) = typeOf(mulExpr1, m)

(* mulExpr *)

typeOf([[ mulExpr1 * unaryExpr1 ]], m) = 
    let
        val t1 = typeOf(mulExpr1, m)
        val t2 = typeOf(unaryExpr1, m1)
    in
        if t1 = t2 andalso t1 = INT then INT
        else ERROR
    end

typeOf([[ mulExpr1 / unaryExpr1 ]], m) = 
    let
        val t1 = typeOf(mulExpr1, m)
        val t2 = typeOf(unaryExpr1, m1)
    in
        if t1 = t2 andalso t1 = INT then INT
        else ERROR
    end

typeOf([[ mulExpr1 % unaryExpr1 ]], m) = 
    let
        val t1 = typeOf(mulExpr1, m)
        val t2 = typeOf(unaryExpr1, m1)
    in
        if t1 = t2 andalso t1 = INT then INT 
        else ERROR
    end

typeOf([[ unaryExpr1 ]], m) = typeOf(unaryExpr1, m)
    
(* unaryExpr *)

typeOf([[ abs unaryExpr1 ]], m) = 
    let
        val t1 = typeOf(unaryExpr1, m)
    in
        if t1 = INT then INT
        else ERROR
    end

typeOf([[ not unaryExpr1 ]], m) = 
    let
        val t1 = typeOf(unaryExpr1, m1)
    in
        if t1 = BOOL then BOOL else ERROR 
    end

typeOf([[ ~ unaryExpr1 ]], m) = 
    let
        val t1 = typeOf(unaryExpr1, m)
    in
        if t1 = INT then INT else ERROR
    end

typeOf([[ expExpr1 ]], m) = typeOf(expExpr1, m)

(* expExpr *)
    
typeOf([[ factor1 ^ expExpr1 ]], m) = 
    let
        val t1 = typeOf(factor1, m)
        val t2 = typeOf(expExpr1, m)
    in
        if t1 = t2 andalso t1 = INT then INT
        else ERROR
    end

typeOf([[ factor1 ]], m) = typeOf(factor1, m)

(* factor *)

typeOf([[ IDENTIFIER ]], m) = getType(accessEnv(IDENTIFIER, m))

typeOf([[ true ]], m) = BOOL
typeOf([[ false ]], m) = BOOL
typeOf([[ INT_LITERAL ]], m) = INT
typeOf([[ ( expr1 ) ]], m) = typeOf(expr1, m)
typeOf([[ decID1 ]], m) = typeOf(decID1, m)

(* decID *)

typeOf([[ ++ IDENTIFIER ]], m) = 
    let 
        val t1 = getType(accessEnv(IDENTIFIER, m))
    in
        if t1 = INT then INT else ERROR
    end

typeOf([[ -- IDENTIFIER ]], m) = 
    let 
        val t1 = getType(accessEnv(IDENTIFIER, m))
    in
        if t1 = INT then INT else ERROR
    end

typeOf([[  IDENTIFIER ++ ]], m) = 
    let 
        val t1 = getType(accessEnv(IDENTIFIER, m))
    in
        if t1 = INT then INT else ERROR
    end

typeOf([[  IDENTIFIER -- ]], m) = 
    let 
        val t1 = getType(accessEnv(IDENTIFIER, m))
    in
        if t1 = INT then INT else ERROR
    end