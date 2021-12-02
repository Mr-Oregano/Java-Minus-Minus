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

fun getLoc(t, l) = l
fun getType(t, l) = t

fun accessStore(_, (_, _, []])) = raise Fail("Error in Model.accessStore - this should never occur")
  | accessStore(l1, (_, _, (l2, v)::s)) = 
        if l1 = l2 then v 
        else accessStore(l1, (_, _, s))

fun accessEnv(_, ([], _, _)) = raise Fail("Error in Model.accessEnv - this should never occur")
  | accessEnv(id1, ((id2, type, loc)::env, _, _)) = 
        if id1 = id2 then (type, loc) 
        else accessEnv(id1, (env, _, _))

fun updateStore(loc, new, (env, n, s)) = 
    let 
        fun aux (l1, new, []) = [(l1, new)]
          | aux (l1, new, (l2, old)::s) = 
            if l1 = l2 then (l2, new)::s
            else (l2, old)::aux(l1, new, s)
    in
        (env, n, aux(loc, new, s))
    end;

fun updateEnv(id1, type, loc, (env, n, s)) = 
    let
        fun aux (id1, type1, l1, []) = [(id1, type, loc)]
          | aux (id1, type1, l1, (id2, type2, l2)::env) = 
            if id1 = id2 then (id1, type1, l1)::env
            else (id2, type2, l2)::aux(id1, type1, l1, env)
    in
        (aux(id1, type1, l1, env), n, s)
    end;

(* =========================================================================================================== *)
end; (* struct *) 
(* =========================================================================================================== *)












