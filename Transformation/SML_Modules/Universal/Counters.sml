(* =========================================================================================================== *)                                  
(* =========================================================================================================== *)
structure Counters =
struct

open UniversalUtil;
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

fun getCounterNameList argList = List.take(tl argList, List.length argList - 2);

fun split xs n = (List.take(xs,n), List.drop(xs,n))

(* =========================================================================================================== *)
type counter  = { counterName : string, data : int ref }
type table    = { tableName   : string, counterList : counter list ref }

val tables    = ref ( [] : table list );

fun getCounterData { counterName, data } = !data

(* ----------------------------------------------------------------------------------------------------------- *)        
fun counterToString( {counterName,data} ) = counterName ^ " = " ^ Int.toString (!data)

fun sumCounterList xs = foldr (fn ({data,...},acc) => !data + acc) 0 xs

fun computeMean counterList =
    let
        val sum  = sumCounterList counterList
        val size = List.length counterList
        val mean = safeDivide(Real.fromInt sum , Real.fromInt size )
    in
        mean
    end
    
fun computeVariance counterList =
    let
        val mean = computeMean counterList
        val size = List.length counterList
        val numerator = foldr (fn ({data,...},acc) =>  let
                                                          val diff = Real.fromInt(!data) - mean 
                                                       in 
                                                          diff * diff + acc
                                                       end
                               )
                               0.0
                               counterList
                               
        val variance = safeDivide(numerator , Real.fromInt size)
    in
       variance
    end

(* one can use standard deviation to determine which elements are within one standard deviation
   of the mean.
*)   
fun computeStandardDeviation counterList =
    let
        val variance = computeVariance counterList
    in
        Math.sqrt variance
    end
    
fun performTableOp "arithmetic_mean"    counterList = Real.trunc (computeMean counterList)
  | performTableOp "variance"           counterList = Real.trunc (computeVariance counterList)
  | performTableOp "standard_deviation" counterList = Real.trunc (computeStandardDeviation counterList)
  | performTableOp "sum"                counterList = sumCounterList counterList
  | performTableOp x _ = raise General.Fail("Error: performTableOp - operation [" ^ x ^ "] not supported")
  
(* ----------------------------------------------------------------------------------------------------------- *)        
fun lookupCounter name counterListRef =
    let
        fun aux( (entry as {counterName, ...}) :: rest ) = if name = counterName then entry else aux rest
          | aux [] = raise General.Fail("Error: counterName = " ^ name ^ " not found")
    in
        aux (!counterListRef)
    end

(* ----------------------------------------------------------------------------------------------------------- *)            
fun lookupTable(name) =
    let
        fun aux( (entry as {tableName,...}) :: rest ) = if name = tableName then entry else aux rest
          | aux [] = raise General.Fail("Error: table = " ^ name ^ " not found")
    in
        aux (!tables)
    end
    
(* ----------------------------------------------------------------------------------------------------------- *)        
fun lookupCounterDirectly(tableName, counterName) =
    let
        val theTable   = lookupTable tableName
        val theCounter = lookupCounter counterName (#counterList theTable)
    in
        theCounter
    end
  
    
(* =========================================================================================================== *)
fun storeTable( newTableName ) =
    let
        fun checkName {tableName,...} = tableName = newTableName
    in
        if List.exists checkName (!tables) then raise General.Fail("Error: duplicate table")
        else tables := {tableName = newTableName, counterList = ref []} :: (!tables)
    end

(* ----------------------------------------------------------------------------------------------------------- *)                
fun storeCounter( tableName, newCounterName, value ) =
    let
        fun checkName {counterName,...} = counterName = newCounterName  
        
        val theCounterListRef = #counterList( lookupTable tableName )
        val entry             = {counterName = newCounterName, data = ref value}
    in
        if List.exists checkName (!theCounterListRef) then raise General.Fail("Error: duplicate counter = " ^ tableName ^ "." ^ newCounterName)
        else theCounterListRef := entry :: (!theCounterListRef)
    end

(* =========================================================================================================== *)            
fun incCounterRefBy(tableName, counterName, delta ) = 
    let
        fun aux( {data,...}, delta ) = data := !data + delta
        
        val theCounter  = lookupCounterDirectly(tableName, counterName) 
    in
        aux (theCounter, delta)
    end
(* ----------------------------------------------------------------------------------------------------------- *)                
fun setCounterRef(tableName, counterName, value ) = 
    let
        fun aux {data,...} = data := value
        
        val theCounter  = lookupCounterDirectly(tableName, counterName) 
    in
        aux theCounter
    end
(* ----------------------------------------------------------------------------------------------------------- *)                
    

(* =========================================================================================================== *)        
(* =========================================================================================================== *)    
fun declareTable [tableNameStr]  =
    let
        val tableName   = getStringFromTL tableNameStr
    in
        storeTable tableName
    end
  | declareTable _ = raise General.Fail("Error: declareTable badly formed")  

(* ----------------------------------------------------------------------------------------------------------- *)     
fun clearTableContents [tableNameStr]  =
    let
        fun auxClear( name, (entry as {tableName,counterList}) :: rest ) = 
                if name = tableName 
                then (counterList := []; entry::rest) 
                else entry :: auxClear(name, rest)
                
          | auxClear (name, []) = raise General.Fail("Error: table = " ^ name ^ " not found")

        val tableName   = getStringFromTL tableNameStr
        val tableList   = !tables
    in
        tables := auxClear (tableName, tableList)
    end
  | clearTableContents _ = raise General.Fail("Error: clearTableContents badly formed")  

(* ----------------------------------------------------------------------------------------------------------- *)     
fun declareCounter (argList as x1 :: x2 :: x3 :: _)  =
    let
        val tableNameStr    = hd argList
        val counterNameList = getCounterNameList argList
        val defaultInt      = List.last argList
        
        val tableName       = getStringFromTL tableNameStr
        val counterName     = getNameFromTL_List counterNameList
        val default         = Strategic_Values.getInt defaultInt
    in
        storeCounter(tableName, counterName, default )
    end
  | declareCounter _ = raise General.Fail("Error: declareCounter badly formed")  

(* ----------------------------------------------------------------------------------------------------------- *)     
fun softDeclareCounter (argList as x1 :: x2 :: x3 :: _)  =
    let
        val tableNameStr    = hd argList
        val counterNameList = getCounterNameList argList
        val defaultInt      = List.last argList
        
        val tableName       = getStringFromTL tableNameStr
        val counterName     = getNameFromTL_List counterNameList
        val default         = Strategic_Values.getInt defaultInt
    in
        storeCounter(tableName, counterName, default )
        handle _ => ()
    end
  | softDeclareCounter _ = raise General.Fail("Error: softDeclareCounter badly formed")  
  


(* ----------------------------------------------------------------------------------------------------------- *)        
fun incCounter (argList as x1 :: x2 :: x3 :: _)  =
    let
        val tableNameStr    = hd argList
        val counterNameList = getCounterNameList argList
        val deltaInt        = List.last argList
        
        val tableName   = getStringFromTL tableNameStr
        val counterName = getNameFromTL_List counterNameList
        val delta       = Strategic_Values.getInt deltaInt
        
        val theCounter  = lookupCounterDirectly(tableName, counterName) 
    in
        incCounterRefBy(tableName, counterName, delta)
    end
  | incCounter _ = raise General.Fail("Error - Counters.incCounter badly formed")

(* ----------------------------------------------------------------------------------------------------------- *)        
(* dangerous but convenient -- creates default entry if not present *)
fun creatingIncCounter (tableNameStr :: xs)  =
    let   
        fun getDefault [default,_] = default
          | getDefault _           = raise General.Fail("Error - Counters.creatingIncCounter.getDefault.")

        fun getDelta [_, delta ] = delta
          | getDelta _           = raise General.Fail("Error - Counters.creatingIncCounter.getDelta.")
          
        val (counterNameList, defaultAndDelta) = split xs (List.length xs - 2)
        
        val defaultInt  = getDefault defaultAndDelta
        val deltaInt    = getDelta defaultAndDelta
        
        val tableName   = getStringFromTL tableNameStr
        val counterName = getNameFromTL_List counterNameList
        val delta       = Strategic_Values.getInt deltaInt
        
        val theCounter  = lookupCounterDirectly(tableName, counterName) 
                          handle _ => (
                                        declareCounter( [tableNameStr] @ counterNameList @ [defaultInt] );
                                        lookupCounterDirectly(tableName, counterName) 
                                      )
    in
        incCounterRefBy(tableName, counterName, delta)
    end
  | creatingIncCounter _ = raise General.Fail("Error - Counters.creatingIncCounter badly formed")
  
(* ----------------------------------------------------------------------------------------------------------- *)          
fun incCounterByHighLine(argList as x1 :: x2 :: x3 :: _)  =
    let
        val tableNameStr    = hd argList
        val counterNameList = getCounterNameList argList
        val someTerm        = List.last argList

        val tableName   = getStringFromTL tableNameStr
        val counterName = getNameFromTL_List counterNameList
        val (_,high)    = Strategic_Values.getLine( someTerm )
    in   
        incCounterRefBy(tableName, counterName, high) 
    end  
  | incCounterByHighLine _ =  raise General.Fail("Error - incCounterByHighLine badly formed")

(* ----------------------------------------------------------------------------------------------------------- *)          
fun setRange(argList as x1 :: x2 :: x3 :: _)  =
    let
        val tableNameStr    = hd argList
        val counterNameList = getCounterNameList argList
        val someTerm        = List.last argList

        val tableName   = getStringFromTL tableNameStr
        val counterName = getNameFromTL_List counterNameList
        val (low,high)  = Strategic_Values.getLine( someTerm )
    in
        incCounterRefBy(tableName, counterName, high - low + 1) 
    end  
  | setRange _ =  raise General.Fail("Error - setRange badly formed")

(* ----------------------------------------------------------------------------------------------------------- *)          
fun setCounter(argList as x1 :: x2 :: x3 :: _)  =
    let
        val tableNameStr    = hd argList
        val counterNameList = getCounterNameList argList
        val valueInt        = List.last argList

        val tableName   = getStringFromTL tableNameStr
        val counterName = getNameFromTL_List counterNameList
        val value       = Strategic_Values.getInt valueInt
    in
        setCounterRef(tableName,counterName,value)
    end  
  | setCounter _ =  raise General.Fail("Error - setCounter badly formed")
  
(* ----------------------------------------------------------------------------------------------------------- *)          
fun tableOp (operationStr :: tableNameStr :: storeTableStr :: rest)   =
    let     
        val operation   = getStringFromTL operationStr
        val tableName   = getStringFromTL tableNameStr
        val storeTable  = getStringFromTL storeTableStr
        val counterName = getNameFromTL_List rest
        
        val theTable    = lookupTable tableName
        val counterList = !(#counterList theTable)
        
        val value       =  performTableOp operation counterList        
    in
        setCounterRef(storeTable,counterName,value)
    end  
    
  | tableOp _ =  raise General.Fail("Error - setRange badly formed")

 

(* =========================================================================================================== *)          
fun showCounter [tableNameStr, counterNameStr] =
    let
        val tableName   = getStringFromTL tableNameStr
        val counterName = getStringFromTL counterNameStr
        val entry       = lookupCounterDirectly(tableName, counterName)
    in
        print( counterToString entry ^ "\n" )
    end
  | showCounter _  = raise General.Fail("Error: showCounter badly formed")    

(* ----------------------------------------------------------------------------------------------------------- *)   
fun showTable [tableNameStr] = 
    let
        fun aux entry   = print("\t" ^ counterToString entry ^ "\n")
        
        val tableName      = getStringFromTL tableNameStr
        val theTable       = lookupTable tableName
        val counterListRef = #counterList theTable
    in
        print("\n\n");
        print("========================================\n");
        print(tableName ^ ":" ^ "\n");
        print("========================================\n");        
        
        map aux (List.rev (!counterListRef))
    end
  | showTable _  = raise General.Fail("Error: showTable badly formed")

(* =========================================================================================================== *)
(* ----------------------------------------------------------------------------------------------------------- *)
(*                                        TL Interface                                                         *)
(* ----------------------------------------------------------------------------------------------------------- *)
(* =========================================================================================================== *)
val execVoid = Util.execVoid

val functions =
    [
        (* Naming convention: "filePath_functionName", functionName    *)

        ("declareTable"         , execVoid declareTable                ),
        ("declareCounter"       , execVoid declareCounter              ),
        ("softDeclareCounter"   , execVoid softDeclareCounter          ),
        ("incCounter"           , execVoid incCounter                  ),
        ("creatingIncCounter"   , execVoid creatingIncCounter          ),
        ("incCounterByHighLine" , execVoid incCounterByHighLine        ),
        ("setRange"             , execVoid setRange                    ),
        ("setCounter"           , execVoid setCounter                  ),
        ("tableOp"              , execVoid tableOp                     ),
        
        ("clearTableContents"   , execVoid clearTableContents          ),
                
        ("showCounter"          , execVoid showCounter                 ),
        ("showTable"            , execVoid showTable                   )
    ];
    
(* =========================================================================================================== *)
end;
(* =========================================================================================================== *)
