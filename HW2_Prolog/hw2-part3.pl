/* hw2-part3 - Mehmed Mustafa 131044084 */
%-----------------------------------------

when(102, 10).
when(108, 12).
when(341, 14).
when(455, 17).
when(452, 17).

where(102, z23).
where(108, z11).
where(341, z06).
where(455, 207).
where(452, 207).

enroll(a, 102).
enroll(a, 108).
enroll(b, 102).
enroll(c, 108).
enroll(d, 341).
enroll(e, 455).

/* Part 3.1 */
schedule(Student, Place, Time) :-
	enroll(Student, X), 
	where(X, Place), 
	when(X, Time), nl.

/* Part 3.2 */
usage(Place, Time) :- 
	where(X, Place), 
	when(X, Time).

	
/* Part 3.3 */
%-------------------------------
/* I have changed the time of course 455 from 16 to 17 to test this */
conflict(Course1, Course2) :-
	where(Course1, Place), 
	where(Course2, Place), 
	when(Course1, Time), 
	when(Course2, Time), 
	Course1\=Course2, nl.

/* Part 3.4 */
meet(Student1, Student2) :- 
	enroll(Student1, Course), 
	enroll(Student2, X),
	where(Course, Place), 
	where(X, Place),
	when(Course, Time1), 
	when(X, Time1),
	Student1\=Student2, nl.

	
	
	
	


