(* Dan Grossman, Coursera PL, HW2 Provided Code *)

(* if you use this function to compare two strings (returns true if the same
   string), then you avoid several of the functions in problem 1 having
   polymorphic types that may be confusing *)
fun same_string(s1 : string, s2 : string) =
    s1 = s2

(* put your solutions for problem 1 here *)

(* a) *)
fun all_except_option(l_str, str) =
    let fun red(ls, acc, count) = 
        case ls of
        [x1] => if same_string(x1, str) then  SOME acc else if count > 0 then SOME (x1::acc) else NONE
        |x1::xs => if same_string(x1, str) then red(xs, acc, count + 1) else  red(xs, x1::acc, count)
    in
    red(l_str, [], 0)
    end
   

(* b) *)
fun get_substitutions1(str, l_str) = 
    case l_str of
    [] => []
    |x1::xs => case all_except_option(x1, str) of
               NONE => get_substitutions1(str, xs)
               |SOME res => res @ get_substitutions1(str, xs)
    
(* c) *)
fun get_substitutions2(str, l_str) = 
    let fun reducer(l_str, acc) =
        case  l_str of
            [] => acc
            | x1::xs => case all_except_option(x1, str) of
                    NONE => reducer(xs, acc)
                    |SOME res => reducer(xs, acc@res)
    in
        reducer(l_str, [])
    end



fun similar_names(l_str, fulln) =
    let fun extract_name fulln =
            case fulln of
                {first = f_n, last = l_n, middle = m_n} =>  f_n  
        fun mk_name(str, fulln) = 
            case fulln of
                {first = f_n, last = l_n, middle = m_n} => {first = str, last = l_n, middle = m_n}
        fun mk_names(l_str, fulln) = 
            case l_str of
                [] => []
                |x1::xs => mk_name(x1, fulln)::mk_names(xs, fulln)
        val res_list = get_substitutions1(extract_name fulln, l_str) 
    in
      mk_names(res_list, fulln) 
    end 


(* you may assume that Num is always used with values 2, 3, ..., 10
   though it will not really come up *)
datatype suit = Clubs | Diamonds | Hearts | Spades
datatype rank = Jack | Queen | King | Ace | Num of int 
type card = suit * rank

datatype color = Red | Black
datatype move = Discard of card | Draw 

exception IllegalMove

(* put your solutions for problem 2 here *)
(* 2. *)
fun card_color(crd) =
    case crd of
        (Diamonds,_) => Red
        |(Hearts,_) => Red
        |_ => Black

fun card_value (crd) = 
    case crd of
         (_, Num v) => v
        |(_, Ace) => 11
        | _ => 10

fun remove_card(cs, c, e) =
    let fun compare(cs, c, count) = 
        case cs of
        [] => if count = 0 then raise e else []
        |cd::ct => if cd = c andalso count=0 
                     then compare(ct, c, 1)
                     else cd::compare(ct, c, count) 
    in 
        compare(cs, c, 0)
    end

fun all_same_color(cards) =
    case cards of
    [x1] => true
    |x1::x2::xs => case  (card_color(x1),card_color(x2)) of
                        (Red, Red) =>  all_same_color(x2::xs)
                        |(Black, Black) =>  all_same_color(x2::xs)
                        | _ => false

fun sum_cards(cards) = 
    let fun aggr(ls, agg) =
        case ls of
        [] => agg
        | x1::xs => aggr(xs,card_value(x1) + agg)
    in 
    aggr(cards, 0)
    end

fun score(cards, goal) = 
    goal - sum_cards(cards) 


fun officiate(cards, moves, goal) =
    let fun helper(cards, moves, held_cards) =
        if score(held_cards, goal) <= 0
            then score(held_cards,goal)
            else case moves of
            [] => score(held_cards,goal)
            | m1::ms => case m1 of
                         Discard cd => helper(cards, ms,  remove_card(held_cards, cd, IllegalMove))
                        | Draw => case cards of
                                [] => score(held_cards, goal)
                                | cd::cs => helper(cs, ms, cd::held_cards)
    in 
        helper(cards, moves, [])
    end

