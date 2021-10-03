(* =========================================================================================================== *)                                  
(* =========================================================================================================== *)
structure Sets =
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
(* =========================================================================================================== *)
type set      = { setName     : string, elementList : string list ref }
val sets      = ref ( [] : set   list );

(* =========================================================================================================== *)
fun lookupSet(name) =
    let
        fun aux( (entry as {setName,...}) :: rest ) = if name = setName then entry else aux rest
          | aux [] = raise General.Fail("Error: set = " ^ name ^ " not found")
    in
        aux (!sets)
    end   

    
(* ----------------------------------------------------------------------------------------------------------- *)            
fun storeSet( newSetName ) =
    let
        fun checkName {setName,...} = setName = newSetName
    in
        if List.exists checkName (!sets) then raise General.Fail("Error: duplicate set = " ^ newSetName)
        else sets := {setName = newSetName, elementList = ref []} :: (!sets)
    end

(* ----------------------------------------------------------------------------------------------------------- *)            
fun addElement( setName, elementName ) =
    let
        val theSet      = lookupSet setName
        val elementsRef = #elementList theSet
    in
        if List.exists (fn element => element = elementName) (!elementsRef) then ()
        else elementsRef := elementName :: (!elementsRef)
    end
    
(* =========================================================================================================== *)        
(* =========================================================================================================== *)    
fun declareSet [setNameStr]  =
    let
        val setName   = getStringFromTL setNameStr
    in
        storeSet setName
    end
  | declareSet _ = raise General.Fail("Error: declareSet badly formed")  

(* ----------------------------------------------------------------------------------------------------------- *)        
fun addElementToSet (setNameStr :: rest)  =
    let
        val setName     = getStringFromTL setNameStr
        val elementName = getNameFromTL_List rest       
    in
        addElement(setName, elementName)
    end
  | addElementToSet _ = raise General.Fail("Error - addElementToSet badly formed")

(* ----------------------------------------------------------------------------------------------------------- *)        
fun addToTraceSet [setNameStr, term]  =
    let
        val setName = getStringFromTL setNameStr
        
        val (lineLo, lineHi)       = Strategic_Values.getLine term       
        val (columnLo , columnHi ) = Strategic_Values.getColumn term       
        val label                  = Strategic_Values.getLabel term 
        (* val directory              = Environment.getInputFolder() *)
        val element                = label ^ " (line,column) = (" ^ Int.toString lineLo ^ "," ^ Int.toString columnLo ^ ")"
    in
        addElement(setName, element)
    end
  | addToTraceSet _ = raise General.Fail("Error - addToTraceSet badly formed")

(* ----------------------------------------------------------------------------------------------------------- *)        
fun setToCounter [setOpStr,setNameStr,tableNameTerm,counterNameTerm]  =
    let
        val setOp       = getStringFromTL setOpStr
        val setName     = getStringFromTL setNameStr
        val tableName   = getStringFromTL tableNameTerm
        val counterName = getStringFromTL counterNameTerm
        
        val theSet      = lookupSet setName
        val elements    = !(#elementList theSet)     
    in
        case setOp of
              "cardinality" => let
                                    val cardinality = List.length elements
                               in
                                    Counters.incCounterRefBy(tableName,counterName, cardinality) 
                               end
            | _             => raise General.Fail("Error: set operation " ^ setOp ^ " not supported")

    end
  | setToCounter _ = raise General.Fail("Error - setToCounter badly formed")
  
(* =========================================================================================================== *)    
(* =========================================================================================================== *)          
fun showSet [setNameStr] = 
    let       
        val setName        = getStringFromTL setNameStr
        val theSet         = lookupSet setName
        val elementListRef = #elementList theSet
    in
        print("\n\n");
        print("========================================\n");
        print(setName ^ ":" ^ "\n");
        print("========================================\n");        
        
        map (fn x => print("\t" ^ x ^ "\n")) (List.rev (!elementListRef))
    end
  | showSet _  = raise General.Fail("Error: showSet badly formed")

(* =========================================================================================================== *)          
fun showSetSorted [setNameStr] = 
    let 
        fun increasing(x:string,y:string) = x >= y;

        val setName        = getStringFromTL setNameStr
        val theSet         = lookupSet setName
        val elementListRef = #elementList theSet
        val sortedSet      = ListMergeSort.sort increasing (!elementListRef)
    in
        print("\n\n");
        print("==========================================\n");
        print(setName ^ ":" ^ "\n");
        print("==========================================\n");        
        
        map (fn x => print("\t" ^ x ^ "\n")) sortedSet
    end
  | showSetSorted _  = raise General.Fail("Error: showSetSorted badly formed")

(* =========================================================================================================== *)
(* ----------------------------------------------------------------------------------------------------------- *)
(*                                        TL Interface                                                         *)
(* ----------------------------------------------------------------------------------------------------------- *)
(* =========================================================================================================== *)
val execVoid = Util.execVoid

val functions =
    [
        (* Naming convention: "filePath_functionName", functionName    *)

        ("declareSet"       , execVoid declareSet      ),
        ("setToCounter"     , execVoid setToCounter    ),
        ("addElementToSet"  , execVoid addElementToSet ),
        ("addToTraceSet"    , execVoid addToTraceSet   ),
        ("showSet"          , execVoid showSet         ),
        ("showSetSorted"    , execVoid showSetSorted   )
    ];
    
(* =========================================================================================================== *)
end;
(* =========================================================================================================== *)
