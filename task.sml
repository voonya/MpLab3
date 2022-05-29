(* if you use this function to compare two strings (returns true if the same
   string), then you avoid several of the functions in problem 1 having
   polymorphic types that may be confusing *)
fun same_string(s1 : string, s2 : string) =
    s1 = s2

(* put your solutions for problem 1 here *)
(* a *)
fun all_except_option(str, strlist) = 
    let fun recurse(strlist, resList, isFound) =
        case strlist of
            [] => (resList, isFound)
            |(hd::tl) => if (same_string(hd, str)) then 
                            recurse(tl, resList, true) 
                        else 
                            recurse(tl, hd::resList, isFound)
    in
        let fun rev_list(lst, resList) =
            case lst of
                [] => resList
                | hd::tl => rev_list(tl, hd::resList)
        in
            case recurse(strlist, [], false) of
                (hd::tl, true) => SOME(rev_list(hd::tl, []))
                |(list, false) => NONE
                |([], true) => SOME([])
        end
    end
;

fun get_substitutions1(list, s) =
  case list of
         [] => []
         | x :: x' => case all_except_option(s, x)
                        of SOME findList => findList @ get_substitutions1(x', s)
                        | NONE => get_substitutions1(x', s)
;
fun get_substitutions2(list, s) =
   let fun recurse(currList, find) = 
	    case currList of
		   [] => find
	      | x :: x' => case all_except_option(s, x) of
			       NONE => recurse(x', find)
			     | SOME findList => recurse(x', find @ findList )
   in
	   recurse(list, [])
   end
;
fun similar_names(list: string list list, {first = firstname, middle = middlename, last = lastname}) =
   let fun find_comb(list, comb) =
      case list of 
          [] => comb
          | x :: x' => find_comb(x',  comb @ ({first = x, middle = middlename, last = lastname} :: []))
   in
      {first = firstname, middle = middlename, last = lastname} :: find_comb(get_substitutions1(list, firstname), [])
   end
;
(* you may assume that Num is always used with values 2, 3, ..., 10
   though it will not really come up *)
datatype suit = Clubs | Diamonds | Hearts | Spades
datatype rank = Jack | Queen | King | Ace | Num of int 
type card = suit * rank

datatype color = Red | Black
datatype move = Discard of card | Draw 

exception IllegalMove

(* put your solutions for problem 2 here *)

(* 2.a *)
fun card_color(card) =
   case card of (Diamonds, _) => Red
            |(Hearts, _) => Red
            | _ => Black
;
(* 2.b *)

fun card_value(card) =
   case card of (_, Ace) => 11
            | (_, King) => 10
            | (_, Queen) => 10
            | (_, Jack) => 10
            | (_, Num x) => x
;
(* 2.c *)

fun remove_card(cs, c, e) =
   let fun recurse(cs, acc, isFound) =
	   case cs of
            [] => (acc, isFound)
            |(x::xs) => if x = c then (acc @ xs, true) else recurse(xs, x::acc, isFound)
    in
     case recurse(cs, [], false) of
            (_, false) => raise e
            | ([], true) => []
            | (x::xs, true) => x::xs
    end
;
(* 2.d *)

fun all_same_color(cards) =
   let val color = case cards of
                        [] => Red
                        | x :: x' => card_color(x)
   fun recurse(cards, color) =
      case cards of [] => true
                  | x :: x' => if card_color(x) <> color then
                                 false
                               else
                                 recurse(x', color)
   in 
      recurse(cards, color)
   end
;
fun sum_cards(cards) =
   let fun sum(cards, total) =
      case cards of 
          [] => total
          | x :: x' => sum(x', card_value(x) + total)
   in
      sum(cards, 0)
   end 
; 
fun score(cards, goal) =
   let val sum = sum_cards(cards)
       val pre_score = if sum > goal then
                        3 * (sum - goal)
                       else
                        goal - sum
   in 
      if not (all_same_color(cards)) then
         pre_score
      else
         pre_score div 2
   end
;
fun officiate(cards, moves, goal) =
   let fun next_move(playerCards, moves, allCards) = 
	    if sum_cards playerCards > goal
	    then score (playerCards, goal)
	    else
		case moves of
		    [] => score (playerCards, goal)
		  | x :: x' => case x of
				   Discard c => next_move(remove_card (playerCards, c, IllegalMove), x', allCards)
				 | Draw => case allCards of 
					       [] => score (playerCards, goal)
					     | j :: j' => next_move(j :: playerCards, x', j')
   in
      next_move([], moves, cards)
   end     
;