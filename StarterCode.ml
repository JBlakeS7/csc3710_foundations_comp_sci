(* CSC 3710 starter code for exploring lists, sorting, and lambda rules in
   CFG's. 
   Author: Martha Kosa
   Date: 04.18.2020 *)

(* Modifications by Jeffrey Simpson *)

(****************************)
(* list utility functions *)

(* converts from String to list of chars *)
(* from https://caml.inria.fr/mantis/view.php?id=5367 *)
let explode s =
  let rec exp i l =
    if i < 0 then l else exp (i - 1) (s.[i] :: l) in
    exp (String.length s - 1) [];;





(* TODO: Call explode with your name as input.  If you are working in a team, call
   explode once for each team member. *)
let exp_name = explode "Jeffrey";;





(* converts from list of chars to String *)
(* single char conversion from https://stackoverflow.com/questions/20441263/convert-char-to-string-in-ocaml/20463186 *)
let rec to_one_string char_list =
  match char_list with
    | [] -> ""
    | h::t -> (String.make 1 h)^(to_one_string t);; (* ^ does string concatenation *)

(* TODO: Call to_one_string with the result(s) of your call(s) to explode. *)
to_one_string(exp_name);;





(* tail-recursive list reversal *)
let reverse xs =
  let rec reverse_tail xs rxs =
    match xs with
      | [] -> rxs
      | h::t -> reverse_tail t (h::rxs)
  in reverse_tail xs [];;

(* TODO: Call reverse with the result(s) of your call(s) to explode. *)
reverse(exp_name);;





(* TODO: Complete (and uncomment) the below recursive function to determine whether
   a list is a prefix of another list. *)
let rec prefix xs ys = 
  match (xs, ys) with
    | ([], _) -> true
    | (_, []) -> false
    | (x::xss, y::yss) ->
        if x=y then (prefix xss yss) else false;;





(* TODO: Write 8 test cases to test your prefix function thoroughly. *)
(* These 4 should be true *)
prefix ['J';'e';'f';'f'] ['J';'e';'f';'f';'r';'e';'y'];;
prefix ['J'] exp_name;;
prefix ['S';'i';'m'] ['S';'i';'m';'p';'s';'o';'n'];;
prefix [1;2;3;4;5] [1;2;3;4;5;6;7;8;9;10];;

(* These 4 should be false *)
prefix ['i';'m'] ['S';'i';'m';'p';'s';'o';'n'];;
prefix ['s';'o';'n'] ['S';'i';'m';'p';'s';'o';'n'];;
prefix ['B';'l';'a';'k';'e'] ['J';'e';'f';'f';'r';'e';'y';'B';'l';'a';'k';'e';'S';'i';'m';'p';'s';'o';'n'];;
prefix [3;4;5] [1;2;3;4;5;6;7;8;9;10];;





(* TODO: Complete (and uncomment) the below function to determine whether
   a list is a suffix of another list. *)
let suffix xs ys = prefix (reverse xs) (reverse ys);;





(* TODO: Write 8 test cases to test your suffix function thorougly. *)
(* These 4 should be true *)
suffix ['s';'o';'n'] ['S';'i';'m';'p';'s';'o';'n'];;
suffix ['y'] exp_name;;
suffix ['J';'e';'f';'f';'r';'e';'y'] ['J';'e';'f';'f';'r';'e';'y'];;
suffix [8;9;10] [1;2;3;4;5;6;7;8;9;10];;

(* These 4 should be false *)
suffix ['J'] exp_name;;
suffix ['S';'i';'m'] ['S';'i';'m';'p';'s';'o';'n'];;
suffix ['B';'l';'a';'k';'e'] ['J';'e';'f';'f';'r';'e';'y';'B';'l';'a';'k';'e';'S';'i';'m';'p';'s';'o';'n'];;
suffix [3;4;5] [1;2;3;4;5;6;7;8;9;10];;





let rec sublist xs ys ks =
  match (xs, ys, ks) with
    | ([], _, _) -> true
    | (_, [], _) -> false
    | (a::xss, b::yss, kss) ->
        if a != b then sublist (kss@(a::xss)) yss []
        else sublist xss yss (kss@[a]);;

let sublist_public xs ys = sublist xs ys [];;

(* TODO: Complete (and uncomment) the below function to determine whether
   a string is a substring of another list. *)
let substring x y = sublist_public (explode x) (explode y);;





(* TODO: Write 8 test cases to test your substring function thoroughly. *)
(* These 4 should be true *)
substring "Jeff" "Jeffrey";;
substring "son" "Simpson";;
substring "Blake" "JeffreyBlakeSimpson";;
substring "e" "Blake";;

(* These 4 should be false *)
substring "abc" "Simspon";;
substring " " "Jeffrey";;
substring "j" "Jeffrey";;
substring "ekalB" "Blake";;





(* TODO: Write a comment to explain what map does. *)
(* map is a higher order function that takes a function f as a parameter and a list xs *)
(* map will apply the function f to each element in the list xs *)
let rec map f xs =
  match xs with
    | [] -> []
    | h::t -> (f h)::(map f t);;





(* TODO: Write a comment to explain what filter does. *)
(* filter takes a function f and a list xs as parameters *)
(* f is applied to each item in the list and the item will get filtered out of the list depending on whether f returns true or false*)
let rec filter f xs =
  match xs with
    | [] -> []
    | h::t ->
        if (f h) then h::(filter f t) else (filter f t);;





(* TODO: Write a comment to explain what my_fold_left does. *)
(* f is a function, id is the identity, xs is a list *)
(* my_fold_left applies the function to the elements in xs in the pattern (f(f(f id a) b) c) where a, b, and c are elements in xs ([a;b;c]) *)
(* example: my_fold_left (+) 0 [a;b;c] will evaluate ((0+a)+b)+c *)
let rec my_fold_left f id xs =
  match xs with
    | [] -> id
    | h::t -> my_fold_left f (f id h) t;;





(* TODO: Write a test case for flatten with your full name and your partner's name.
   Write a comment to explain what flatten does. *)
(* flatten takes a list of lists as a parameter. It concatenates the lists together to create one list *)
let flatten xs = my_fold_left (@) [] xs;;

let first = ['J';'e';'f';'f';'r';'e';'y'];;
let middle = ['B';'l';'a';'k';'e'];;
let last = ['S';'i';'m';'p';'s';'o';'n'];;
flatten [first;middle;last];; (* I don't have a partner so I used my full name *)





let make_ordered_pair a b = (a,b);;

let make_CSC_ordered_pair = make_ordered_pair "CSC";;

(* TODO: Complete (and uncomment) the following function call to produce ordered
   pairs corresponding to your CSC courses this semester.  Make a call for
   each person on your team. *)	
let classes = [4620;4100;3710;4710];;	 
map make_CSC_ordered_pair classes;;





let rec cartesian_product xs ys =
  match xs with
    | [] -> []
    | h::t -> (map (make_ordered_pair h) ys)@(cartesian_product t ys);;

(* TODO: Write a test case for cartesian_product with your full name and your
   partner's name in the first list and a list of your choice having at
   least 3 items. *)
let fullname = ["Jeffrey"  ; "Blake" ; "Simpson"];;
let secondlist = [" is graduating"; " is a student" ; " likes to hike"];; 
cartesian_product fullname secondlist;;





let cartesian_product_better xs ys =
  let rec cartesian_product_tr xs ys acc =
    match xs with
      | [] -> acc
      | h::t -> cartesian_product_tr t ys (acc@(map (make_ordered_pair h) ys))
  in cartesian_product_tr xs ys [];;

(* TODO: Call cartesian_product_better with the same test case for cartesian_product. *)
cartesian_product_better fullname secondlist;;





let rec concatenate xs ys =
  match xs with
    | [] -> []
    | h::t -> (map ((^) h) ys)@(concatenate t ys);;

(* TODO: Write two test cases for concatenate: one which does not produce
   duplicate elements and one which does. One of them must involve
   the empty string. *)
		 
let test_list_1 = ["a";"b";"c";"d";"e";"f"];;
let test_list_2	= ["g";"h";"i";"j";"k";" "];;
let test_list_3	= ["a";"a";"b";"b";"c";"c"];;
concatenate test_list_1 test_list_2;;
concatenate test_list_1 test_list_3;;

	 
(* TODO: Write a comment to tell what sorting algorithm is implemented by the
   neg, partition, and sort functions below. *)		 
let neg f x = not (f x);;

(* Merge Sort *)
let partition f l = (filter f l, filter (neg f) l);;

(* Quick Sort *)
let rec sort xs gt =
  match xs with 
    | [] -> []
    | (pivot::t) -> let (l, r) = partition (gt pivot) t in
          (sort l gt)@(pivot::(sort r gt));;


(* TODO: Complete (and uncomment) the below recursive function that removes
   duplicate items from a list. *)
let rec remove_dupes xs =
  match xs with
    | [] -> []
    | [h] -> xs
    | x1::x2::t ->
        let after = remove_dupes (x2::t) in
          if x1 = x2 then after else x2::after;; 


(* TODO: Uncomment the below code. *)
let concatenate_no_dupes xs ys gt =
  remove_dupes (sort (concatenate xs ys) gt);;

(* TODO: Call concatenate_no_dupes with the two test cases that you developed for
   concatenate. *)

concatenate_no_dupes test_list_1 test_list_2;;
concatenate_no_dupes test_list_1 test_list_3;;

let position x the_list =
  let rec accpos i y the_list =
    match the_list with
      | [] -> -1
      | z::t -> if y = z then i else (accpos (i+1) y t)
  in (accpos 0 x the_list);;

let contains the_list x = (position x the_list) >= 0;;
let all_contained_in the_list xs = my_fold_left (&&) true (map (contains the_list) xs);;

let rec add_to_sorted_list x xs gt =
  match xs with
    | [] -> [x]
    | h::t ->
        if (gt h x) then x::h::t
        else if (gt x h) then h::(add_to_sorted_list x t gt)
        else h::t (* no duplicates *);;

let add_all_to_sorted_list xs gt =
  let rec add_all_tr xs acc =
    match xs with
      | [] -> acc
      | h::t -> add_all_tr t (add_to_sorted_list h acc gt)
  in add_all_tr xs [];;

(* TODO: Complete (and uncomment) the following two calls to add_all_to_sorted_list.
   Both calls should involve a list of strings, and the lists should have
   duplicates and should not be disjoint. *)
let my_list1 = add_all_to_sorted_list ["a";"a";"b";"c";"d";"e"] (>);;

let my_list2 = add_all_to_sorted_list ["b";"f";"f";"g";"h";"e"] (>);;

(* TODO: Call add_all_to_sorted_list using your two test cases from above, but use <
   instead of > . *)
let my_list3 = add_all_to_sorted_list ["a";"a";"b";"c";"d";"e"] (<);;

let my_list4 = add_all_to_sorted_list ["b";"f";"f";"g";"h";"e"] (<);;



let rec sorted_list_diff ys xs gt =
  match (ys, xs) with
    | (_, []) -> ys
    | ([], _) -> []
    | (hy::ty, hx::tx) ->
        if gt hx hy then hy::(sorted_list_diff ty (hx::tx) gt)
        else if gt hy hx then hx::(sorted_list_diff (hy::ty) tx gt)
        else sorted_list_diff ty tx gt;;

(* TODO: Uncomment the following call to sorted_list_diff. *)
sorted_list_diff my_list1 my_list2 (>);;

let sorted_list_diff_better ys xs gt =
  let rec sorted_list_diff_tr ys xs acc =
    match (ys, xs) with
      | (_, []) -> acc@ys
      | ([], _) -> acc
      | (hy::ty, hx::tx) ->
          if gt hx hy then sorted_list_diff_tr ty (hx::tx) (acc@[hy])
          else if gt hy hx then sorted_list_diff_tr (hy::ty) tx (acc@[hx])
          else sorted_list_diff_tr ty tx acc
  in sorted_list_diff_tr ys xs [];;

(* TODO: Uncomment the following call to sorted_list_diff_better. *)
sorted_list_diff_better my_list1 my_list2 (>);;


let rec explode_all_RHS productions =
  match productions with
    | [] -> []
    | head::tail ->
        match head with
          | (lhs, rhs) ->
              (lhs, (explode rhs))::(explode_all_RHS tail);;

(* TODO: Complete (and uncomment) the following two functions. *)
let is_nullable (lhs, rhs) = (rhs = "");;

let get_LHS (lhs, rhs) = lhs;;


let all_contained_in the_list xs = my_fold_left (&&) true (map (contains the_list) xs);;

let is_RHS_in_set_star set (_, rhs) =  all_contained_in set rhs;;

(* TODO: Uncomment the following code. *)
let add_new_nullable_level prev current rules gt =
  let filtered = filter (is_RHS_in_set_star current) rules in
    add_all_to_sorted_list ((map get_LHS filtered)@prev) gt;;

let nullable_nonterminals rules gt =
  let exploded = explode_all_RHS rules in
  let null_0 = directly_nullable exploded in
  let null_1 = add_new_nullable_level [] null_0 exploded gt in
  let rec nullable_nonterminals_helper prev current =
    if current = prev then current
    else
      let new_nullables = add_new_nullable_level prev current exploded gt in
        nullable_nonterminals_helper current new_nullables
  in nullable_nonterminals_helper null_0 null_1;; 

let ex_lots_nullable = [('S', "ABC"); ('S', "JKL"); ('J', "jJ"); ('J', "k");
                        ('K', "kK"); ('K', ""); ('L', "lL"); ('L', "");
                        ('A', "DE"); ('B', "FG"); ('C', "HI");
                        ('D', "dD"); ('D', ""); ('E', "eE"); ('E', "");
                        ('F', "fF"); ('F', ""); ('G', "gG"); ('G', "");
                        ('H', "hH"); ('H', ""); ('I', "iI"); ('I', "")];;
(* "" denotes lambda *)

let ex_lots_nullable_noS = [('S', "ABCJKL"); ('J', "jJ"); ('J', "k");
                            ('K', "kK"); ('K', ""); ('L', "lL"); ('L', "");
                            ('A', "DE"); ('B', "FG"); ('C', "HI");
                            ('D', "dD"); ('D', ""); ('E', "eE"); ('E', "");
                            ('F', "fF"); ('F', ""); ('G', "gG"); ('G', "");
                            ('H', "hH"); ('H', ""); ('I', "iI"); ('I', "")];;

nullable_nonterminals ex_lots_nullable (>);;

nullable_nonterminals ex_lots_nullable_noS (>);;

let is_lambda_valid rules start = contains (nullable_nonterminals rules (>)) start;;

is_lambda_valid ex_lots_nullable 'S';;

is_lambda_valid ex_lots_nullable_noS 'S';;

let tail (h::t) = t;;
(* TODO: Call tail twice: once on a list with a single element and once on a list with
   at least two elements.  What happens when you try to call tail on an empty list?
   Write your answer a comment. *)

let cons x ys = x::ys;;
(* TODO: Call cons twice with nested function calls to add two items to an empty list. *)

let rec power_set xs =
  match xs with
    | [] -> [[]]
    | h::t ->
        let ps_t = power_set t in ps_t@(map (cons h) ps_t);;
(* TODO: Call power_set on the empty list, a list with a single item, and a list with
   6 items. *)

(* TODO: Uncomment the following code. *)   
(* let nullify_all_nullables_in_RHS lhs rhs nullables =
   let rec helper rhs nullables acc =
   match rhs with
   | [] -> acc
   | h::t ->
   if (contains nullables h) then
   helper t nullables acc
   else
   helper t nullables (acc@[h])
   in (lhs, helper rhs nullables []);;

   let produce_nullified_rules lhs rhs nullables =
   let rec helper lhs rhs nullables acc =
   match nullables with
   | [] -> acc
   | h::t -> helper lhs rhs t (acc@[nullify_all_nullables_in_RHS lhs rhs h])
   in helper lhs rhs nullables [];;

   let bypass_all_nullables lhs rhs nullables =
   let nullables_power_set = power_set nullables in
   produce_nullified_rules lhs rhs nullables_power_set;;

   let is_not_a_nonessential_lambda_rule start (lhs, rhs) = (rhs != [] || lhs = start);;

   let make_essentially_noncontracting_grammar start rules =
   let nullables = nullable_nonterminals rules (>) in
   let rec helper rules acc =
   match rules with
   | [] -> acc
   | (lhs, rhs)::t ->
   helper t (acc@(bypass_all_nullables lhs rhs nullables)) in
   filter (is_not_a_nonessential_lambda_rule start) (helper (explode_all_RHS rules) []);;

   let produce_clean_rule (lhs, rhs) =
   (to_one_string [lhs])^" -> "^(to_one_string rhs);;

   let make_clean_essentially_noncontracting_grammar start rules =
   map produce_clean_rule (add_all_to_sorted_list (make_essentially_noncontracting_grammar start rules) (>));;

   make_clean_essentially_noncontracting_grammar 'S' ex_lots_nullable;;

   make_clean_essentially_noncontracting_grammar 'S' ex_lots_nullable_noS;; *)

(* TODO: the last one.  Yay!!!!! Develop a CFG with lambda rules that generates
   {(first)^n (last)^n (last)^n (first)^n | n >= 0}, assuming first
   represents your first name, and last represents your last name.  Do this
   for each team member. *)
