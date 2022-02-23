(* =========================================================================================================== *)
structure Model =

struct 

(* =========================================================================================================== *)
(* This function may be useful to get the leaf node of a tree -- which is always a string (even for integers).
   It is up to the interpreter to translate values accordingly (e.g., string to integer and string to boolean).
   
   Consult (i.e., open Int and open Bool) the SML structures Int and Bool for functions that can help with 
   this translation. 
*)
fun getLeaf( term ) = CONCRETE.leavesToStringRaw term 


(* For your typeChecker you may want to have a datatype that defines the types 
  (i.e., integer, boolean and error) in your language. *)
datatype types = INT | BOOL | ERROR;


(* It is recommended that your model store integers and Booleans in an internal form (i.e., as terms belonging to
   a userdefined datatype (e.g., denotable_value). If this is done, the store can be modeled as a list of such values.
*)
datatype denotable_value =  Boolean of bool 
                          | Integer of int;


type loc   = int
type env   = (string * types * loc) list
type store = (loc * denotable_value) list

(* The model defined here is a triple consisting of an environment, an address counter, and a store. The environment
   and the store are lists similar to what we have used in class. The address counter serves as an implementation of
   new(). Note that, depending on your implementation, this counter either contains the address of (1) the
   next available memory location, or (2) the last used memory location -- it all depends on when the counter is 
   incremented. *)
val initialModel = ( []:env, 0:loc, []:store )

(* NOTE: This is important! When accounting for scope we may want to keep 
		 the old counter and environment after scope execution is complete.
		 any values left in the store will be overwritten later. This will
		 avoid the need for garbage collection. *)

fun typeStrToEnum(name) = 
  if name = "bool" then BOOL
  else if name = "int" then INT
  else raise Fail("Unknown type '" ^ name ^ "'")

fun getLoc(tp, loc) = loc
fun getType(tp, loc) = tp

fun getInt(Integer v) = v
  | getInt _ = raise Fail("Invalid type.")

fun getBool(Boolean v) = v
  | getBool _ = raise Fail("Invalid type.")
  
fun getVarName(loc, []) = raise Fail("ERROR: variable not found.")  
  | getVarName(loc, (name, tp, loc_n)::envs) = 
      let
        val typeStr = if tp = INT then "int" else "bool";
      in
        if loc = loc_n then name ^ ":" ^ typeStr
        else getVarName(loc, envs)
      end

fun accessStore(loc, (env, _, [])) = raise Fail("ERROR: variable not initialized '" ^ getVarName(loc, env) ^ "'.")
  | accessStore(loc, (env, c, (loc_n, v)::s)) = 
        if loc = loc_n then v 
        else accessStore(loc, (env, c, s))

fun accessEnv(id, ([], _, _)) = raise Fail("ERROR: '" ^ id ^ "' was not declared in this scope.")
  | accessEnv(id, ((id_n, tp, loc)::envs, c, s)) = 
        if id = id_n then (tp, loc) 
        else accessEnv(id, (envs, c, s))

(* NOTE: updateStore() will add a new entry for unused location
		 otherwise update the value stored at the given location. *)
fun updateStore(loc, v, (env, c, s)) = 
    let 
        fun aux([]) = [(loc, v)]
          | aux((loc_n, old)::s) = 
            if loc = loc_n then (loc, v)::s
            else (loc_n, old)::aux(s)
    in
        (env, c, aux(s))
    end;

(* Although a location was provided in the original updateEnv(), it does not
   make sense to do so here since a new location can be computed from the counter
   present in the program model. *)
fun updateEnv(id, tp, (env, c, s)) = 
    let
        fun aux ([]) = [(id, tp, c)]
          | aux ((id_n, tp_n, loc)::env) = 
            if id = id_n then raise Fail("ERROR: '" ^ id_n ^ "' already declared in this scope.")
            else (id_n, tp_n, loc)::aux(env)
    in
        (aux(env), c + 1, s)
    end;

(* Utility print and string conversion functions *)

fun envEntryToString(name, tp, loc) = 
    let 
      val typeStr = if tp = INT then "int" else "bool";
      val locStr = Int.toString(loc);
    in
      name ^ ":" ^ typeStr ^ " @ " ^ locStr
    end;

fun dvToString(Integer v) = Int.toString(v)
  | dvToString(Boolean v) = Bool.toString(v)
  
fun storeEntryToString(loc, v) = 
    let 
      val typeStr = dvToString(v);
      val locStr = Int.toString(loc);
    in
      "loc=" ^ locStr ^ ", val=" ^ typeStr
    end;

fun printStore([]) = ()
  | printStore(s::str) = 
	( 
		print(storeEntryToString(s) ^ "\n");
		printStore(str)
	)

fun printEnv([]) = ()
  | printEnv(e::env) = 
	( 
		print(envEntryToString(e) ^ "\n");
		printEnv(env)
	)

fun printModel(env, c, s) = 
	(
		print("MODEL          \n---------------------------------------\n\nCounter: " ^ Int.toString(c));
		print("\n\nEnvironment\n---------------------------------------\n\n");
		printEnv(env);
		print("\nStore        \n---------------------------------------\n\n");
		printStore(s)
	)
(* =========================================================================================================== *)
end; (* struct *) 
(* =========================================================================================================== *)
