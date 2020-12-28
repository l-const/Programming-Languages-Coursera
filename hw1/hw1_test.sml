use "hw1.sml";

val date1 = option_to_date (make_date (2020, 3, 28))
val date2 =  option_to_date (make_date (2020, 4, 28))
val date4 = option_to_date (make_date (2020, 6, 28))
val date3 =  option_to_date (make_date (2020, 6, 28))

(* Test 1  *)
val res1  = is_older(date1, date2) (*TRUE*)
val res2  = is_older(date2, date1) (*FALSE*)
val res3  = is_older(date2, date2) (*FALSE*)

(* Test 2 *)
val list_dt = [date1, date2, date3, date4]
val res2_1 = number_in_month(list_dt, 6)  (*2*)
val res2_2 = number_in_month(list_dt, 2)  (*0*)

(* Test 3 *)         
val list_month = [3,1, 2, 5, 6]
val res3_1 = number_in_months(list_dt, list_month)

(* Test 4 *)  
val res4_1 = dates_in_month(list_dt, 3)

(* Test 5 *)  
val res5_1 = dates_in_months(list_dt, [3,4]) 

(* Test 6 *)         
val strings = ["first", "second", "third", "fourth"]
val res6_1 = get_nth(strings, 2)


(* Test 7 *)         
val date_str = date_to_string(date1)

(* Test 8 *)         
val ints = [1,2,3,4,5]
val sum = 4
val res8_1 = number_before_reaching_sum(sum, ints)

(* Test 9 *)         
val res9_1 = what_month(48) (* February -> 2*)

(* Test 10 *)
val res10_1 = month_range(32, 98)

(* Test 11 *)
val empty = oldest([])
val non_empty = oldest(list_dt)