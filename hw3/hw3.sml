(* Coursera Programming Languages, Homework 3, Provided Code *)

exception NoAnswer

datatype pattern = Wildcard
		 | Variable of string
		 | UnitP
		 | ConstP of int
		 | TupleP of pattern list
		 | ConstructorP of string * pattern

datatype valu = Const of int
	      | Unit
	      | Tuple of valu list
	      | Constructor of string * valu

fun g f1 f2 p =
    let 
	val r = g f1 f2 
    in
	case p of
	    Wildcard          => f1 ()
	  | Variable x        => f2 x
	  | TupleP ps         => List.foldl (fn (p,i) => (r p) + i) 0 ps
	  | ConstructorP(_,p) => r p
	  | _                 => 0
    end

(**** for the challenge problem only ****)

datatype typ = Anything
	     | UnitT
	     | IntT
	     | TupleT of typ list
	     | Datatype of string

(**** you can put all your code here ****)


(* 1. *)
fun only_capitals ls = 
                        List.filter (fn x => Char.isUpper(String.sub(x, 0)))  ls

(* 2. *)
fun longest_string1 ls = foldl (fn (x, y) => if String.size x < String.size y then y else x) "" ls


(* 3. *)
fun longest_string2 ls = foldl (fn (x, y) => if String.size x <= String.size y then y else x) "" ls


(* 4. *)
fun longest_string_helper f  = fn ls => foldl (fn (x, y) => if f(String.size x, String.size y) then y else x)  ""  ls 
val longest_string3 = longest_string_helper (fn (x, y) => x < y)
val longest_string4 = longest_string_helper (fn (x, y) => x <= y)

(* 5. *)
val longest_capitalized = longest_string1 o only_capitals

(* 6. *)
fun  rev_string s =  (implode o rev o explode) s

(* 7. *)

fun first_answer f = fn ls => 
    case ls of
    [] => raise NoAnswer
    | x::xs => case f x of
                SOME v => v 
                | _ => first_answer f xs
            
 (* 8. *)

fun all_answers f = fn lst =>
    let fun helper xs agg =
       case xs of
        [] => SOME agg 
        | x::xt => case f x of
                    SOME v => helper xt (agg@v)
                   | NONE => NONE
    in  
       case lst of
        [] => SOME []
        | _ => helper lst []
    end


(* 9. *)
(* 9a. *)
fun count_wildcards p = g (fn _ => 1) (fn _ => 0) p

(* 9b. *)
fun count_wild_and_variable_lengths p = g (fn _ => 1) (fn x => String.size x) p


(* 9c *)
fun count_some_var (s,p) = g (fn _ => 0) (fn x => if x=s then 1 else 0) p 


(* 10. *)

fun check_pat p = 
    let fun helper1 p =
        case p of
        Variable x        =>  [x]
        | TupleP ps         => List.foldl (fn (x,i) => (helper1 x)@i ) [] ps
        | ConstructorP(_,p) => helper1 p
        | _                 => []

        fun helper2 lst =
            case lst of
            [] => false
            | x1::xs => if List.exists (fn x => x1=x )  xs  then true else false orelse (helper2 xs)

    in
    not (helper2 (helper1 p))
    end
    

fun match (v,p) = 
    case p of
		 Variable s => SOME [(s, v)]
		| UnitP => ( case v of 
                        Unit => SOME []
                        |_ => NONE)
		| ConstP n1 => (case v of 
                        Const n2 => if n2=n1 then SOME [] else NONE
                        |_ => NONE)
		| TupleP pl => (case v of
                        Tuple vlst => if List.length pl = List.length vlst then all_answers match (ListPair.zip(vlst, pl)) else NONE
                        | _ => NONE)
		| ConstructorP (s1,pt) => (case v of 
                                Constructor (s2, value) => if s1 = s2 then match(value, pt) else NONE
                                |_ => NONE)
        | Wildcard => SOME []


(* 12. *)

 fun first_match v plst =
    SOME (first_answer (fn p => match(v, p)) plst) handle NoAnswer => NONE 