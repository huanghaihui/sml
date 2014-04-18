(******************part one*********four questions*********************)
fun same_string (s1 : string , s2 : string ) = 
    s1 = s2

(***fun all_except_option****)
fun all_except_option ( s , xs ) =
    case xs of
	[] => NONE
	   | x::xs' =>  case same_string(s,x) of
			   true => SOME xs'
			 | false =>  case ( all_except_option(s,xs')) of 
					 NONE => NONE
				       | SOME l =>  SOME (x::l);


(***fun get_substitutions1******)
fun get_substitutions1 ( str , s ) = 
    case str of 
	[] => []
     | str1::str' => case all_except_option( s, str1 ) of
			 NONE => []@get_substitutions1( str', s)
		      | SOME l => l@get_substitutions1( str', s);


(*****fun get_substitutions2*********)
fun get_substitutions2 (str , s ) = 
    let fun get_helper (str ,  acc ) = 
	    case str of
		[] => acc
	     | x::str' => case all_except_option ( s, x) of
			     NONE => get_helper(str',acc)
			  | SOME list => get_helper(str', acc@list)
    in
	get_helper(str, [])
    end;



(*******fun similar_names****************)
fun similar_names ( str , {first = x , middle = y, last = z } ) = 
    let fun similar_helper ( str1 ) = 
	    case str1 of 
		[] => []
	      | s::str1' => {first = s, last = z, middle = y}::similar_helper(str1')
    in
	{first = x, last = z, middle = y }::similar_helper( get_substitutions2( str ,x))
    end;



(*******************part  two*****************seven questions********************)
datatype suit = Clubs | Diamonds | Hearts | Spades
datatype rank = Jack | Queen | King | Ace | Num of int 
type card = suit * rank

datatype color = Red | Black
datatype move = Discard of card | Draw 

exception IllegalMove

(*******fun card_color**********)
fun card_color ( c ) = 
    case  c of
	(Clubs,_) => Black
     | (Spades,_) => Black
     | _ => Red;


(********fun card_value********)
fun card_value ( c  ) = 
    case c of
	(_,Ace) => 11
     | (_,Num(i)) => i
     | _ => 10 ;




(******fun remove_card*******)
fun remove_card ( [] , c  , e  ) = 
    raise e
  | remove_card  ( cs  , c  , e  ) = 
    case cs of 
	[] => []
     | cl::cs' => case cl = c of
		       true => cs'
		     | false => case remove_card( cs' ,c , e ) of
				     clist => cl::clist;


(******fun all_same_color*******)
fun all_same_color ( cs  ) = 
    case cs of
	[] => true
     | (color,rank1)::[] => true
     | (color1,rank1)::((color2,rank2)::rest) => ( color1 = color2 andalso all_same_color((color2,rank2)::rest));


(*******fun sum_cards**********)
fun sum_cards ( cs  ) = 
    let fun sum_helper ( cl : card list , acc : int ) = 
	    case cl of
		[] => acc 
	     | c::cl' => sum_helper( cl' , acc + card_value(c))
    in
	sum_helper(cs ,0 )
    end;


(********fun score************)
fun score ( cs  , goal) = 
    let val sum = sum_cards(cs)
	val priminary_score = case sum > goal of 
				  true => 3*( sum -goal )
			       | false => goal-sum
    in
	case all_same_color ( cs ) of
	    true => priminary_score div 2
	 | false => priminary_score
    end;


(******fun officiate********)
fun officiate ( cs  , ms  , goal  ) = 
    let val held_cards = [] : card list
	fun officiate_helper(cl  , ml  , g  , h_c  ) =
	    case ml of 
		[] => raise IllegalMove
	      | operation::ml' => (case operation of
				      Discard ( c ) => (case remove_card( h_c, c , IllegalMove) of
							   _ => officiate_helper( cl , ml' , g , h_c))   
				| Draw => (case cl of	
				           [] => raise IllegalMove
				    | cc::cl' => (case sum_cards(h_c@[cc]) > g of
						     true => score( h_c@[cc], g )
						   | false => (case h_c@[cc] of 
								  _ => officiate_helper( cl',ml', g, h_c@[cc])))))
    in
	officiate_helper( cs , ms , goal ,held_cards)
    end;


(****challenge problems****************)

(******fun score_challenge*****)
(*fun score_challenge ( cs , goal ) =  *)


fun add_list rankvalue  acc = 
    case rankvalue =  Ace of 
		true => ( case acc of 
	   			[] => 1::11::[]
				| head::next => (1+acc)::(11+acc)::add_list( rankvalue, next ) )
		| false => ( case acc of 
				[] => card_value( rankvalue )::[]
				| head::next => card_value(rankvalue)::add_list ( rankvalue ,next ))	

fun sum_list cs = 
    let fun sum_helper (cs, acc)  = 
        	case cs of 
			[] => acc
  		| (_,rank1)::next => sum_helper( next , add_list( rank1 ,acc ))
    in	
	sum_helper( cs , [] )
    end


fun challenge_score cs goal =
	let val alist =  sum_list cs 
             fun getmin cs min = 
		case cs of 
			[] => min
			| head::as' => ( case head > goal of
						true =>( case (head_goal)*3 > min andalso min > 0 of
								true =>  getmin (as' ,min)
								| false => getmin (as' ,(head-goal)*3 ))
						|false => ( case (goal-head) > min andalso min > 0 of 
								true => getmin ( as', min )
								| false => getmin ( as' , goal-head )))
	in 
	    getmin ( alist , 1 )
	end	


(***fun officiate_challnge****)
fun judge_bigger_all xs goal = 
	case xs of 
		[] => true
		| head::as' => (case head > goal of 
				true => judge_bigger_all ( as' , goal )
				| false => false)


fun officiate_challenge ( cs  , ms  , goal  ) = 
    let val held_cards = [] : card list
	fun officiate_helper(cl  , ml  , g  , h_c  ) =
	    case ml of 
		[] => raise IllegalMove
	      | operation::ml' => (case operation of
				      Discard ( c ) => (case remove_card( h_c, c , IllegalMove) of
							   _ => officiate_helper( cl , ml' , g , h_c))   
				| Draw => (case cl of	
				           [] => raise IllegalMove
				    | cc::cl' => (case judge_bigger_all( sum_list(h_c) , g ) of
						     true => score( h_c@[cc], g )
						   | false => (case h_c@[cc] of 
								  _ => officiate_helper( cl',ml', g, h_c@[cc])))))
    in
	officiate_helper( cs , ms , goal ,held_cards)
    end;
