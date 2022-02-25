structure Model =

struct 

fun getLeaf( term ) = CONCRETE.leavesToStringRaw term 

datatype types = INT 
               | BOOL
               | STRING 
               | ERROR;

(* Lists are homogenous containers so we define a custom datatype housing constructors
   for different primitives. *)

datatype denotable_value =  Boolean of bool 
                          | Integer of int
                          | String  of string; 

type loc   = int
type env   = (string * types * loc) list
type store = (loc * denotable_value) list

(* The model defined here is a triple consisting of an environment, an address counter, and a store. The address counter 
   serves as an implementation of new(). *)

val initialModel = ( []:env, 0:loc, []:store )

(* NOTE: This is important! When accounting for scope we may want to keep 
		 the old counter and environment after scope execution is complete.
		 any values left in the store will be overwritten later. This will
		 avoid the need for garbage collection. *)

fun typeStrToEnum(name) = 
  if name = "bool" then BOOL
  else if name = "int" then INT
  else if name = "string" then STRING
  else if name = "error" then ERROR
  else raise Fail("Unknown type '" ^ name ^ "'")

fun typeEnumToStr(tp : types) = 
  if tp = BOOL then "bool"
  else if tp = INT then "int"
  else if tp = STRING then "string"
  else "error"

fun typeDVToEnum(Integer _) = INT
  | typeDVToEnum(Boolean _) = BOOL
  | typeDVToEnum(String _)  = STRING

fun dvToString(Integer v) = Int.toString(v)
  | dvToString(Boolean v) = Bool.toString(v)
  | dvToString(String v)  = v

fun getLoc(tp, loc) = loc
fun getType(tp, loc) = tp

fun getInt(Integer v) = v
  | getInt _ = raise Fail("Invalid type.")

fun getBool(Boolean v) = v
  | getBool _ = raise Fail("Invalid type.")

fun getString(String v) = v
  | getString _ = raise Fail("Invalid type.")
  
(* SIG: loc * env -> string 
   NOTES: This function retrieves the name and type of a variable given a location
          and an environment. Throws an error if the location is invalid. 
*)
fun varName(loc, []) = raise Fail("ERROR: variable not found.")  
  | varName(loc, (name, tp, loc_n)::envs) = 
      let
        val typeStr = typeEnumToStr(tp);
      in
        if loc = loc_n then name ^ ":" ^ typeStr
        else varName(loc, envs)
      end

(* SIG: loc * (env * loc * store) -> (loc * denotable_value) 
   NOTES: This function retrieves, from the model, the store entry at the specified location.
          Throws an error if the location has not been initialized. 
*)
fun accessStore(loc, (env, _, [])) = raise Fail("ERROR: variable not initialized '" ^ varName(loc, env) ^ "'.")
  | accessStore(loc, (env, c, (loc_n, v)::s)) = 
        if loc = loc_n then v 
        else accessStore(loc, (env, c, s))

(* SIG: loc * (env * loc * store) -> (string * types * loc)
   NOTES: This function retrieves, from the model, the env entry housing the specified id.
          Throws an error if the id does not exist. 
*)
fun accessEnv(id, ([], _, _)) = raise Fail("ERROR: '" ^ id ^ "' was not declared in this scope.")
  | accessEnv(id, ((id_n, tp, loc)::envs, c, s)) = 
        if id = id_n then (tp, loc) 
        else accessEnv(id, (envs, c, s))

(* SIG: loc * denotable_value * (env * loc * store) -> (env * loc * store)
   NOTES: This function will update the value stored at the specified location or add a new entry 
          with specified value if location is not in use.
*)
fun updateStore(loc, v, (env, c, s)) = 
    let 
        fun aux([]) = [(loc, v)]
          | aux((loc_n, old)::s) = 
            if loc = loc_n then (loc, v)::s
            else (loc_n, old)::aux(s)
    in
        (env, c, aux(s))
    end;

(* SIG: string * type * (env * loc * store) -> (env * loc * store)
   NOTES: This function will add an environment entry with the specified id and type 
          in the model. Throws an error if the id already exists.
*)
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
      val typeStr = typeEnumToStr(tp);
      val locStr = Int.toString(loc);
    in
      name ^ ":" ^ typeStr ^ " @ " ^ locStr
    end;

  
fun storeEntryToString(loc, v) = 
    let 
      val vStr = dvToString(v);
      val locStr = Int.toString(loc);
    in
      "loc=" ^ locStr ^ ", val=" ^ vStr
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

end; 
