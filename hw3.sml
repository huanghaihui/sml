(*******Part 1**********)

(******fun only_capitals********)
fun only_capitals  strlist = List.filter (fn s => Char.isUpper(String.sub(s,0)))  strlist;

only_capitals ["abc","A","Ddd"];
only_capitals [];


(***longest_string1,2,3,4******)
fun longest_string1 strlist = foldl (fn (x,y) => if String.size(x) > String.size(y)  then x else y)  ""  strlist;

longest_string1 [];
longest_string1 ["abc","A","Ddd"];


fun longest_string2 strlist = foldl (fn (x,y) => if String.size(y) >= String.size(x) then (if (String.size(x) = String.size(y)) then x else y) else x) "" strlist;

longest_string2 ["abc","A","Ddd"];

fun longest_string_helper f xs =
    foldl ( fn (x,y) => if f(String.size(x),String.size(y))  then x else y )  ""  xs;

val longest_string3 = longest_string_helper (fn (x,y) => if (x > y) then true else false ) ;

longest_string3 ["abc","A","Ddd"];

val longest_string4 = longest_string_helper (fn (x,y) => if (y>=x andalso y =x)  then false  else true );

longest_string4 ["abd","A","Ddd"];


(********longest_capitalized*****)
fun longest_capitalized strlist =
    let val result = longest_string1  o  only_capitals
    in
        result strlist
    end;
longest_capitalized ["abc", "A", "Ddd"];  (* "Ddd" *)

longest_capitalized ["abc", "a", "bbb"]; (* "" *)

longest_capitalized []; (* "" *)


(********fun rev_string***************)
fun rev_string str = ( String.implode o rev o String.explode ) str ;

rev_string ""; (* "" *)

rev_string "abcdef"; (* "fedcba" *)


(*******fun first_answer and all_answer****)
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
    end;


fun first_answer f xs =
    case xs of
        [] => raise NoAnswer
     | x::xs' => case f x  of
                    SOME v => v
                 | NONE  => first_answer f xs';

fun f1 x =
    case x of
	false => NONE
      | true => SOME 1;

(*first_answer f1 []; (* raise NoAnswer *)*)
(*first_answer f1 [false]; (* raise NoAnswer *)*)
first_answer f1 [false, false, true]; (* 1 *)


fun all_answers f xs =
    let fun all_helper (f, acc, xs) =
            case xs of
                [] => SOME acc
             | x::xs' => case f x of
                             NONE => NONE
                           | SOME lst => all_helper (f, acc@lst, xs')
    in
        all_helper( f, [], xs)
    end;

(*  case xs of
        [] => SOME []
     | x::xs' => case f x of
                     SOME lt => SOME( lt@(all_answers f xs'))
                   | NONE => NONE
*)
fun f2 x =
    case x of
	false => NONE
      | true => SOME [1,2];

all_answers f2 []; (* SOME [] *)
all_answers f2 [true, true, false]; (* NONE *)
all_answers f2 [true, true, true]; (* SOME [1,2,1,2,1,2] *)



(**********pattern matching******************)

(****count_wildcards******)
fun count_wildcards ptn =(g (fn () => 1 )  (fn x:string => 0 )) ptn;

fun count_wild_and_variable_lengths ptn = (g (fn () => 1) (fn x:string => String.size(x))) ptn;

fun count_some_var (str,ptn) = (g (fn () => 0)  (fn x:string => if str = x then 1 else 0)) ptn;

val p1 = Wildcard;
val p2 = Variable "var1";
val p3 = UnitP;
val p4 = ConstP  7;
val p5 = ConstructorP ("lala", p1);
val p6 = TupleP [p1,p1,p2,p3,p5]
val ps = [p1,p2,p3,p4,p5,p6];
List.map count_wildcards ps; (* [1, 0, 0, 0, 1, 3]*)
List.map count_wild_and_variable_lengths ps; (* [1, 4, 0, 0, 1, 7]*)
List.map (fn x => count_some_var ("var1",x)) ps; (* [0, 1, 0, 0, 0, 1]*)
List.map (fn x => count_some_var ("var2",x)) ps; (* [0, 0, 0, 0, 0, 0]*)


(******fun check_pat***********)
fun help_first ptn  =
    let fun help ptns =
            case ptns of
                [] => []
             | p::ptns' => case p of
                               Variable x => x::help ptns'
                             | TupleP ps => (help ps)@(help ptns')
                             | _  => help ptns'
    in
        case ptn of
            Variable x  => x::[]
         | TupleP ps => help ps
         | _  => []
    end


fun help_second str =
    case str of
        [] => true
     | s::str'  => case (List.exists (fn s => s = s ) str') of
                       true => false
                    | false  => help_second str'

fun check_pat ptn =
    let val result = help_second o help_first
    in
        result ptn
    end;


val p7 = TupleP [p6,p3, p2];
val pp = help_first p6;
val pp2 = help_first p7;
check_pat p7; (* false *)
check_pat p6; (* true *)



(****fun match**************)
fun match (value , ptn) =
    case (ptn ,value) of
        (Wildcard,_) => SOME []
     | (Variable p, v) => SOME [(p,value)]
     | (ConstP p ,Const v) => if p = v then SOME [] else NONE
     | (UnitP,Unit) => SOME []
     | (TupleP ps,Tuple vs) => if length ps = length vs 
                               then SOME [] (*all_answers (fn x => match x ) ListPair.zip(vs,ps)*)
                               else NONE
     | (ConstructorP(s1,p),Constructor(s2,v))=> if s2 <> s1 then NONE else match(v,p)
     | (_,_) => NONE


(*********fun first_match******)
fun first_match value ptns = 
    case first_answer (match value) ptns of
        SOME l => l
     | handle NoAnswer => NONE



(**** for the challenge problem only ****)

datatype typ = Anything
	     | UnitT
	     | IntT
	     | TupleT of typ list
	     | Datatype of string

fun typecheck_parttens ((str1,str2,typ1) , ptl) =
    
