use "hw3.sml";

val ls_ex = ["sddsd", "Georg", "dsdsdsds", "John", "KOnstantinsisds", "Porererere", "KOnstantinsisds"]


val test1 = only_capitals ls_ex

val test2 = longest_string1 ls_ex

val test3 = longest_string2 ls_ex

val test5 = longest_capitalized ls_ex

val test6 = rev_string "kostas"

val ls_patterns = TupleP [Wildcard, Wildcard, ConstructorP ("lkol", Wildcard), Variable "12345", Variable "1245",  Variable "1345"]

val test9a = count_wildcards ls_patterns

val test9b = count_wild_and_variable_lengths ls_patterns

val test9c = count_some_var ("12345", ls_patterns)

val test10 = check_pat ls_patterns

