initiatedAt(fighting(X2,X1),X3) :- use(1,0),try(1,1,vars(X2,X3)),try(1,2,vars(X2,X3)),try(1,3,vars(X2,X3,X1)),try(1,4,vars(X2,X3,X1)),try(1,5,vars(X2,X3,X1)),try(1,6,vars(X2,X3,X1)),try(1,7,vars(X3,X1)),try(1,8,vars(X2,X3,X1)),try(1,9,vars(X3,X1)),person(X1),person(X2),time(X3).
try(1,1,vars(X2,X3)) :- use(1,1),holdsAt(visible(X2),X3),person(X2),time(X3).
try(1,1,vars(X2,X3)) :- not use(1,1),person(X2),time(X3).
try(1,2,vars(X2,X3)) :- use(1,2),happensAt(walking(X2),X3),person(X2),time(X3).
try(1,2,vars(X2,X3)) :- not use(1,2),person(X2),time(X3).
try(1,3,vars(X2,X3,X1)) :- use(1,3),distMoreThan(X2,X1,34,X3),person(X2),time(X3),person(X1).
try(1,3,vars(X2,X3,X1)) :- not use(1,3),person(X2),time(X3),person(X1).
try(1,4,vars(X2,X3,X1)) :- use(1,4),distMoreThan(X2,X1,27,X3),person(X2),time(X3),person(X1).
try(1,4,vars(X2,X3,X1)) :- not use(1,4),person(X2),time(X3),person(X1).
try(1,5,vars(X2,X3,X1)) :- use(1,5),distMoreThan(X2,X1,24,X3),person(X2),time(X3),person(X1).
try(1,5,vars(X2,X3,X1)) :- not use(1,5),person(X2),time(X3),person(X1).
try(1,6,vars(X2,X3,X1)) :- use(1,6),distMoreThan(X2,X1,25,X3),person(X2),time(X3),person(X1).
try(1,6,vars(X2,X3,X1)) :- not use(1,6),person(X2),time(X3),person(X1).
try(1,7,vars(X3,X1)) :- use(1,7),happensAt(walking(X1),X3),time(X3),person(X1).
try(1,7,vars(X3,X1)) :- not use(1,7),time(X3),person(X1).
try(1,8,vars(X2,X3,X1)) :- use(1,8),orientClose(X1,X2,45,X3),person(X2),time(X3),person(X1).
try(1,8,vars(X2,X3,X1)) :- not use(1,8),person(X2),time(X3),person(X1).
try(1,9,vars(X3,X1)) :- use(1,9),holdsAt(visible(X1),X3),time(X3),person(X1).
try(1,9,vars(X3,X1)) :- not use(1,9),time(X3),person(X1).
terminatedAt(fighting(X3,X1),X2) :- use(2,0),try(2,1,vars(X2,X1)),try(2,2,vars(X2,X1)),try(2,3,vars(X2,X3,X1)),try(2,4,vars(X2,X3,X1)),try(2,5,vars(X2,X3,X1)),try(2,6,vars(X2,X3,X1)),try(2,7,vars(X2,X3,X1)),try(2,8,vars(X2,X3)),try(2,9,vars(X2,X3,X1)),try(2,10,vars(X2,X3)),time(X2),person(X3),person(X1).
try(2,1,vars(X2,X1)) :- use(2,1),holdsAt(visible(X1),X2),time(X2),person(X1).
try(2,1,vars(X2,X1)) :- not use(2,1),time(X2),person(X1).
try(2,2,vars(X2,X1)) :- use(2,2),happensAt(walking(X1),X2),time(X2),person(X1).
try(2,2,vars(X2,X1)) :- not use(2,2),time(X2),person(X1).
try(2,3,vars(X2,X3,X1)) :- use(2,3),distMoreThan(X1,X3,34,X2),time(X2),person(X3),person(X1).
try(2,3,vars(X2,X3,X1)) :- not use(2,3),time(X2),person(X3),person(X1).
try(2,4,vars(X2,X3,X1)) :- use(2,4),distMoreThan(X1,X3,27,X2),time(X2),person(X3),person(X1).
try(2,4,vars(X2,X3,X1)) :- not use(2,4),time(X2),person(X3),person(X1).
try(2,5,vars(X2,X3,X1)) :- use(2,5),distMoreThan(X1,X3,24,X2),time(X2),person(X3),person(X1).
try(2,5,vars(X2,X3,X1)) :- not use(2,5),time(X2),person(X3),person(X1).
try(2,6,vars(X2,X3,X1)) :- use(2,6),distMoreThan(X1,X3,25,X2),time(X2),person(X3),person(X1).
try(2,6,vars(X2,X3,X1)) :- not use(2,6),time(X2),person(X3),person(X1).
try(2,7,vars(X2,X3,X1)) :- use(2,7),orientFar(X1,X3,45,X2),time(X2),person(X3),person(X1).
try(2,7,vars(X2,X3,X1)) :- not use(2,7),time(X2),person(X3),person(X1).
try(2,8,vars(X2,X3)) :- use(2,8),happensAt(walking(X3),X2),time(X2),person(X3).
try(2,8,vars(X2,X3)) :- not use(2,8),time(X2),person(X3).
try(2,9,vars(X2,X3,X1)) :- use(2,9),orientFar(X3,X1,45,X2),time(X2),person(X3),person(X1).
try(2,9,vars(X2,X3,X1)) :- not use(2,9),time(X2),person(X3),person(X1).
try(2,10,vars(X2,X3)) :- use(2,10),holdsAt(visible(X3),X2),time(X2),person(X3).
try(2,10,vars(X2,X3)) :- not use(2,10),time(X2),person(X3).



clauseIndex(1..2).

literalIndex(0..11).

{use(1,1..9)}.{use(2,1..10)}.



{use(I,J)}:-clauseIndex(I),literalIndex(J).

{use(I,J,K)}:-clauseIndex(I),clauseIndex(J),literalIndex(K).

posNotCovered(holdsAt(fighting(X0,X1),X2)) :-
    example(holdsAt(fighting(X0,X1),X2)), not holdsAt(fighting(X0,X1),X2).
posNotCovered(holdsAt(moving(X0,X1),X2)) :-
    example(holdsAt(moving(X0,X1),X2)), not holdsAt(moving(X0,X1),X2).
posNotCovered(holdsAt(meeting(X0,X1),X2)) :-
    example(holdsAt(meeting(X0,X1),X2)), not holdsAt(meeting(X0,X1),X2).

negsCovered(holdsAt(fighting(X0,X1),X2)) :-
    holdsAt(fighting(X0,X1),X2), not example(holdsAt(fighting(X0,X1),X2)).
negsCovered(holdsAt(moving(X0,X1),X2)) :-
    holdsAt(moving(X0,X1),X2), not example(holdsAt(moving(X0,X1),X2)).
negsCovered(holdsAt(meeting(X0,X1),X2)) :-
    holdsAt(meeting(X0,X1),X2), not example(holdsAt(meeting(X0,X1),X2)).

#minimize{use(I,J),use(I,J,K),posNotCovered(holdsAt(fighting(X0,X1),X2)),posNotCovered(holdsAt(moving(X0,X1),X2)),posNotCovered(holdsAt(meeting(X0,X1),X2)),negsCovered(holdsAt(fighting(X0,X1),X2)),negsCovered(holdsAt(moving(X0,X1),X2)),negsCovered(holdsAt(meeting(X0,X1),X2))}.

:- use(I,J), not use(I,0).

#hide.

#show use/2.

#show use/3.