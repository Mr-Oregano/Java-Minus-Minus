(* ------------------------------------------------------------------------------------------- *)
structure Properties =
struct

(* ------------------------------------------------------------------------------------------- *)
fun property_01 [] = 
    let
        val sum       = Counters.getCounterData(
                            Counters.lookupCounterDirectly("Metrics", "sum of constructs")
                        )
        val mean      = Counters.getCounterData(
                            Counters.lookupCounterDirectly("Metrics", "mean")
                        )
        val variance  = Counters.getCounterData(
                            Counters.lookupCounterDirectly("Metrics", "variance")
                        )
        val std_dev   = Counters.getCounterData(
                            Counters.lookupCounterDirectly("Metrics", "standard deviation")
                        )
        val correct   = Counters.getCounterData(
                            Counters.lookupCounterDirectly("Metrics", "correct")
                        )
        val incorrect = Counters.getCounterData(
                            Counters.lookupCounterDirectly("Metrics", "incorrect")
                        )
                        
        val check     = (sum > 100) andalso (mean > 4) andalso (variance < 3) 
                       andalso 
                       (std_dev < 3) andalso (correct = 10) andalso (incorrect = 0)
    in
        if not(sum > 100) then print("\nSum of constructs must be greater than 100.") else ();
        if not(mean > 4) then print("\nMean must be greater than 4.") else ();
        if not(variance < 3) then print("\nVariance must be less than 3.") else ();
        if not(std_dev < 3) then print("\nStandard deviation must be less than 3.") else ();
        if not(correct = 10) then print("\nMust have exactly 10 correct translations.") else ();
        if not(incorrect = 0) then print("\nMust have exactly 0 incorrect translations.") else ();
        
        if check then print("\n\nPASSED!") else print("\n\nFAILED.")
    end

  | property_01 _  = raise Fail("Error: property_01 badly formed")

(* ------------------------------------------------------------------------------------------- *)
(* ------------------------------------------------------------------------------------------- *)
(*                                 Exported Function List                                      *)
(* ------------------------------------------------------------------------------------------- *)
(* ------------------------------------------------------------------------------------------- *)
   val execVoid = Util.execVoid
   
   val functions = 

    [
        ("property_01"   , execVoid property_01 )
    ]

(* ------------------------------------------------------------------------------------------- *)    
end (* struct *)
(* ------------------------------------------------------------------------------------------- *)