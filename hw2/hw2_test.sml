use "hw2.sml";


(* Test 1 *)
val res = all_except_option(["john", "george", "kostas"], "kostas")
val res1_2 = all_except_option(["john", "george", "kostas"], "paul")

(* Test 2 *)
val res2 = get_substitutions1("Fred", [["Fred","Fredrick"],["Elizabeth","Betty"],["Freddie","Fred","F"]])


(* Test 3 *)
val res3 = get_substitutions2("Fred", [["Fred","Fredrick"],["Elizabeth","Betty"],["Freddie","Fred","F"]])

(* test 4 *)
val res4_1 = similar_names([["Fred","Fredrick"],["Elizabeth","Betty"],["Freddie","Fred","F"]],{first="Fred", middle="W", last="Smith"})


(* Test 2a *)

val test_cc1 = card_color((Clubs, Ace))
val test_cc2 = card_color((Hearts, Num 2))
val test_cc3 = card_color((Spades, Queen))


(* Test 2b *)
val test_cv1 = card_value((Clubs, Ace))
val test_cv2 = card_value((Clubs, Num 2))
val test_cv3 = card_value((Clubs, Queen))

(* Test 2c *)
exception NotRemoved
val list_cards = [(Clubs, Ace),(Clubs, Ace),(Clubs, Ace),(Clubs, Num 2),(Clubs, Queen)]
val res2c = remove_card(list_cards,(Clubs, Ace), NotRemoved)

 
(* Test 2d *)
val test2d_t = all_same_color(list_cards)
val list_cards2 = [(Clubs, Ace),(Clubs, Ace),(Clubs, Ace),(Clubs, Num 2),(Hearts, Queen)]
val test2d_f = all_same_color(list_cards2)

(* Test 2e *)
val test2e = sum_cards(list_cards2)


(* Test 2f *)
val test2f = score(list_cards2, 50)