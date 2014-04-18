(*1 function : is_older*)
fun is_older (date1 : int*int*int , date2 : int*int*int) =
    if ( #1 date1) < (#1 date2)
    then true
    else if ( #1 date1 ) > ( #1 date2)
    then false
    else if ( #2 date1) < (#2 date2)
    then true
    else if (#2 date1 ) > (#2 date2)
    then false
    else if (#3 date1) < (#3 date2)
    then true
    else false

(*2 number_in_month*)
fun number_in_month ( date: (int*int*int) list, month: int ) = 
    if null date
    then 0 
    else if  #2(hd date)  = month
    then 1 + number_in_month(tl(date), month) 
    else 0 + number_in_month(tl(date), month) 
   

(*3 number_in_months*)
fun number_in_months ( date: (int*int*int) list, months: int list)=
    if null months
    then 0
    else number_in_month( date, hd(months)) + number_in_months(date ,tl(months))

(*4 function : dates_in_month*)
fun dates_in_month ( date:(int*int*int) list, month:int)=
    if null date
    then []
    else if  #2(hd date) = month
    then hd(date)::dates_in_month(tl(date), month)
    else dates_in_month(tl(date),month)		      

(*5 function : dates_in_months*)
fun dates_in_months ( date:(int*int*int) list, month:int list)=
    if null month
    then []
    else dates_in_month(date,hd(month))@dates_in_months(date,tl(month))

(*6 function : get_nth*)
fun get_nth ( strs : string list, n: int ) = 
    if null strs
    then hd strs(*the presentation of  null string  that we need*)
    else if n > 1
    then get_nth( tl(strs), n-1)
    else hd strs
    

(*7 date_to_string*)
fun date_to_string ( date : int*int*int ) =  
    let val month = ["January","February","March", "April","May","June", "July","August","September", "October", "November", "December"]
    in 
	get_nth( month, #2 date)^" "^Int.toString(#3 date)^" , "^Int.toString(#1 date)		    
    end


(*8 number_before_reaching_sum*)
fun number_before_reaching_sum (sum : int , num : int list) =
    if hd num >= sum
    then 0
    else 1 + number_before_reaching_sum ( sum - hd num , tl(num) )

(*9 what_month*)
fun what_month ( day : int) =
    let val month = [31,28,31,30,31,30,31,31,30,31,30,31]
    in 1 + number_before_reaching_sum ( day , month)
    end

(*10 month_range*)
fun month_range ( day1 : int , day2 : int ) = 
    if day1 > day2
    then []
    else day1::month_range(day1+1,day2)


(*11 oldest*) 
fun oldest ( dates : (int*int*int) list ) = 
    if null dates
    then NONE
    else  
	let 
	    val d = oldest(tl dates)
        in
	    if isSome d andalso is_older(valOf d , hd dates)
	    then d
	    else SOME (hd dates)
	end



(*12 Challenge Problem  number_in_months_challenge  dates_in_months_challenge*)

fun add ( months: int list , a : int ) =  (*add a new number to the list and the number doesn't exist in the list*)
    if null months
    then a::months
    else if hd months <> a
    then (hd months)::add(tl(months),a)
    else months

fun remove_element_list ( months: int list) = (*remove the duplicate element of the list *)
    if null months
    then []
    else add(remove_element_list(tl(months)),hd months)
(*add([] ,hd months)@remove_element_list(tl(months)) *)


fun number_in_months_challenge ( dates : (int*int*int) list, months: int list ) =
    if null months
    then 0
    else number_in_months( dates , remove_element_list(months)) 
		   
fun dates_in_months_challenge ( dates : (int*int*int) list , months : int list) = 
    if null months
    then []
    else dates_in_months(dates, remove_element_list(months))



(*13 Challenge Problem   reasonable_date *)
fun leap_year ( year : int ) =
    if year <= 0
    then false
    else if year mod 400 = 0 
    then true
    else if year mod 4 = 0 andalso year mod 100 <> 0
    then true
    else false

fun get_days ( mo : int , l_year : bool ) = 
    if 1 = mo orelse mo = 3 orelse mo =5 orelse mo = 7 orelse mo = 8 orelse mo = 10  orelse mo =12
    then 31
    else if mo <> 2 
    then 30
    else if l_year 
    then 29
    else 28
	     
    (*end *)

fun reasonable_date ( date : int*int*int ) = 
    if #1 date <= 0
    then false
    else if #2 date <= 12 andalso #2 date > 0
    then if #3 date <= get_days(#2 date, leap_year(#1 date)) andalso #3 date > 0
	 then true
	 else false
    else false

