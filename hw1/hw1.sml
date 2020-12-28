(* In all problems, a “date” is an SML value of type int*int*int, where the first part 
is the year, the second part is the month, and the third part is the day. A “reasonable”
date has a positive year,a month between 1 and 12, and a day no
greater than 31.*)

fun make_date(year: int, month: int, day: int) =
    if ((day > 0) andalso (day <= 31)) andalso ((month > 0) andalso (month <= 12))
    then
    SOME (year, month, day)
    else
    NONE

fun option_to_date(date_op: (int*int*int) option) =
    valOf date_op

fun is_some(date_op: (int*int*int) option) =
    isSome date_op

(* 1. Write a function is_older that takes two dates and evaluates to true or false.
It evaluates to true if the first argument is a date that comes before the second argument. 
(If the two dates are the same,the result is false.*)
(* is_older = fn : (int * int * int) * (int * int * int) -> bool *)
fun is_older(date1: (int*int*int), date2: (int*int*int)) = 
    if #1 date1 < #1 date2
    then
    true
    else
        if #1 date1 = #1 date2
        then 
            if #2 date1 < #2 date2
            then 
            true
            else
                if #2 date1 = #2 date2
                then
                    if #3 date1 < #3 date2
                    then
                    true
                    else 
                    false  
                else
                false
        else
        false


(*2. Write a function number_in_month that takes a list of dates and a month (i.e., an 
int) and returns how many dates in the list are in the given month.*)
fun number_in_month(list_dates :(int *int *int) list, month: int) = 
        if null list_dates
        then 0
        else
            if #2 (hd list_dates) = month 
            then 1 + number_in_month(tl list_dates, month)
            else number_in_month(tl list_dates, month)


(* 3. Write a function number_in_months that takes a list of dates and a list of months 
(i.e., an int list) and returns the number of dates in the list of dates that are in
 any of the months in the list of months. Assume the list of months has
no number repeated. Hint: Use your answer to the previous problem. *)
fun number_in_months(list_dates :(int *int *int) list, months: int list) = 
    if null months
    then 0
    else   
    number_in_month(list_dates, hd months) + number_in_months(list_dates, tl months)

(* 4. Write a function dates_in_month that takes a list of dates and a month
(i.e., an int) and returns a list holding the dates from the argument list
of dates that are in the month. The returned list should
contain dates in the order they were originally given. *)

fun dates_in_month(list_dates : (int*int*int) list, month: int) =
    if null list_dates
    then []
    else
        if #2 (hd list_dates) = month
        then (hd list_dates)::dates_in_month(tl list_dates, month)
        else dates_in_month(tl list_dates, month)


(* 5. Write a function dates_in_months that takes a list of dates and a list of months (i.e., an int list)
and returns a list holding the dates from the argument list of dates that are in any of the months in
the list of months. Assume the list of months has no number repeated. Hint: Use your answer to the
previous problem and SML’s list-append operator (@) *)

fun dates_in_months(list_dates : (int*int*int) list, list_months: int list) =
    if null list_months
    then []
    else
    dates_in_month(list_dates, (hd list_months)) @ dates_in_months(list_dates, tl list_months)
        
(* 6. Write a function get_nth that takes a list of strings and an int n and returns the n th element of the
list where the head of the list is 1 st . Do not worry about the case where the list has too few elements:
your function may apply hd or tl to the empty list in this case, which is okay. *)
fun get_nth(list_str: string list, index :int) =
    if index = 1
    then hd list_str
    else get_nth(tl list_str, index-1)


(* 7. Write a function date_to_string that takes a date and returns a string
of the form January 20, 2013 (for example). Use the operator ^ for concatenating
strings and the library function Int.toString for converting an int to a string.
For producing the month part, do not use a bunch of conditionals.
Instead, use a list holding 12 strings and your answer to the previous problem. For consistency, put a
comma following the day and use capitalized English month names: January, February, March, April,
May, June, July, August, September, October, November, December.*)
val months_str = ["January", "February", "March", "April","May", "June", "July", "August", "September", "October", "November", "December"]

fun date_to_string(date: (int*int*int)) =
   
    let val month_str = get_nth(months_str, #2 date)
        val year_str = Int.toString(#1 date)
        val day_str = Int.toString(#3 date)
    in
        month_str ^ " " ^ day_str ^ ", " ^ year_str
    end

(* 8. Write a function number_before_reaching_sum that takes an int called sum, which you can assume
is positive, and an int list, which you can assume contains all positive numbers, and returns an int.
You should return an int n such that the first n elements of the list add to less than sum, but the first
n + 1 elements of the list add to sum or more. Assume the entire list sums to more than the passed in
value; it is okay for an exception to occur if this is not the case. *)

fun number_before_reaching_sum(sum:int, list_int : int list) =
        let val head = hd list_int
        in 
            if sum > head
            then 1 + number_before_reaching_sum(sum - head, tl list_int)
            else 0
        end

(* 9. Write a function what_month that takes a day of year (i.e., an int between 1 and 365) and returns
what month that day is in (1 for January, 2 for February, etc.). Use a list holding 12 integers and your
answer to the previous problem. *)
val days_in_month = [31,28,31,30,31,30,31,31,30,31,30,31]

fun what_month(day_int: int) =
    1 + number_before_reaching_sum(day_int, days_in_month)


(* 10. Write a function month_range that takes two days of the year day1 and day2 and returns an int list
[m1,m2,...,mn] where m1 is the month of day1, m2 is the month of day1+1, ..., and mn is the month
of day day2. Note the result will have length day2 - day1 + 1 or length 0 if day1>day2.  *)
fun month_range(day1:int, day2:int) =
    if day1 > day2
    then []
    else
        what_month(day1)::month_range(day1+1, day2)


(* 11. Write a function oldest that takes a list of dates and evaluates to an (int*int*int) option. It
evaluates to NONE if the list has no dates and SOME d if the date d is the oldest date in the list. *)
fun oldest(list_d :(int*int*int) list) =
    if null list_d
    then NONE
    else
    let fun compare(date, list_date) =
        if null list_date
        then true
        else is_older(date, hd list_date) andalso compare(date, tl list_date)
        val head = hd list_d
        val tail = tl list_d
    in
        if compare(head, tail)
        then SOME head
        else oldest(tail)
    end

