/* hw2-part4 - Mehmed Mustafa 131044084 */

%-------------PART 4.1--------------------
/* The sum of the empty list is zero */
add([], 0).

/* Adds the head of the current list to the SubSum 
which will be calculated in the next 
recursive call and binds the result to the Sum */
add([HeadElement | RestElements], Sum) :-
	add(RestElements, SubSum),
	Sum is SubSum + HeadElement.
	
%-------------PART 4.2--------------------
/* Removing from an empty list is an empty list */
remove(_, [], []).
	
/* If the head is not the element for removing (EFR), 
put the head into ClearedList and remove the element 
from the rest elements */
remove(EFR, [HeadElement | RestElements], [HeadElement | ClearedList]) :-
	EFR\=HeadElement,
	remove(EFR, RestElements, ClearedList).
	
/* If the head is the element for removing, 
remove the element from the rest elements */
remove(EFR, [EFR | RestElements], ClearedList) :-
	remove(EFR, RestElements, ClearedList).
	
/* An empty list is unique */
unique([], []).

/* Put the head element in the unique list and 
remove all occurences of the head element
from the rest elements and save the resulting 
list into ClearedList. */
unique([HeadElement | RestElements], [HeadElement | UniqueList]) :-
	remove(HeadElement, RestElements, ClearedList),
	unique(ClearedList, UniqueList).
	
%-------------PART 4.3--------------------

/* An empty list is flat. */
flatten([], []) :- !.	

/* Since the HeadList cannot contain lists as elements, 
append it with the tempResult from the recursive calls 
to the FlattenedList which is the final result.*/
flatten([HeadList | RestLists], FlattenedList) :-
	!,	%prevents_unwanted_backtracking
	flatten(HeadList, TempResultHead),
	flatten(RestLists, TempResultRest), %flatten_the_rest_of_the_lists
	append(TempResultHead, TempResultRest, FlattenedList).

/* Flatten single element into list */
flatten(Element, [Element]).
	
	
	


