/* hw2-part1-2 - Mehmed Mustafa 131044084 */
%-----------------------------------------

flight(edirne, edremit).
flight(erzincan, edremit).
flight(edremit, erzincan).
flight(edremit, edirne).
flight(istanbul, trabzon).
flight(istanbul, kars).
flight(istanbul, ankara).
flight(istanbul, gaziantep).
flight(istanbul, konya).
flight(istanbul, antalya).
flight(istanbul, izmir).
flight(ankara, istanbul).
flight(ankara, trabzon).
flight(ankara, kars).
flight(ankara, konya).
flight(ankara, izmir).
flight(trabzon, istanbul).
flight(trabzon, ankara).
flight(kars, istanbul).
flight(kars, ankara).
flight(gaziantep, istanbul).
flight(konya, istanbul).
flight(konya, ankara).
flight(antalya, istanbul).
flight(izmir, istanbul).
flight(izmir, ankara).


distance(edremit, erzincan, 1044).
distance(erzincan, edremit, 1044).
distance(edremit, edirne, 168).
distance(edirne, edremit, 168).

distance(istanbul, trabzon, 909).
distance(trabzon, istanbul, 909).
distance(istanbul, kars, 1196).
distance(kars, istanbul, 1196).
distance(istanbul, ankara, 372).
distance(ankara, istanbul, 372).
distance(istanbul, gaziantep, 872).
distance(gaziantep, istanbul, 872).
distance(istanbul, konya, 486).
distance(konya, istanbul, 486).
distance(istanbul, antalya, 504).
distance(antalya, istanbul, 504).
distance(istanbul, izmir, 335).
distance(izmir, istanbul, 335).
distance(ankara, trabzon, 578).
distance(trabzon, ankara, 578).
distance(ankara, kars, 862).
distance(kars, ankara, 862).
distance(ankara, konya, 231).
distance(konya, ankara, 231).
distance(ankara, izmir, 513).
distance(izmir, ankara, 513).


/* Part 1 - */
helper(Start, Finish, VisitedList) :- 
	flight(Start, X),
	not(member(X, VisitedList)),
	(Finish = X; 
	helper(X, Finish, [Start | VisitedList])).
	
route(Start, Finish) :- helper(Start, Finish, []).


/* Part 2*/
findLength(Start, Finish, Length) :-
	distance(Start, Finish, Length), !.
	
findLength(Start, Finish, Length) :-
	distance(Start, X, Y),
	distance(X, Finish, Y1),
	Length is Y + Y1.

findMin(Start, Finish, Length) :-
	findLength(Start, Finish, Length).

sroute(Start, Finish, Length) :- findMin(Start, Finish, Length).
	

	
	
	
	
	
	
	
	

