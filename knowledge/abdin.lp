:- example(holdsAt(fighting(X0,X1),X2)), not holdsAt(fighting(X0,X1),X2).
:- holdsAt(fighting(X0,X1),X2), not example(holdsAt(fighting(X0,X1),X2)).
:- example(holdsAt(moving(X0,X1),X2)), not holdsAt(moving(X0,X1),X2).
:- holdsAt(moving(X0,X1),X2), not example(holdsAt(moving(X0,X1),X2)).
:- example(holdsAt(meeting(X0,X1),X2)), not holdsAt(meeting(X0,X1),X2).
:- holdsAt(meeting(X0,X1),X2), not example(holdsAt(meeting(X0,X1),X2)).
{initiatedAt(fighting(X0,X1),X2):person(X0):person(X1):time(X2)}.
{initiatedAt(moving(X0,X1),X2):person(X0):person(X1):time(X2)}.
{initiatedAt(meeting(X0,X1),X2):person(X0):person(X1):time(X2)}.
{terminatedAt(fighting(X0,X1),X2):person(X0):person(X1):time(X2)}.
{terminatedAt(moving(X0,X1),X2):person(X0):person(X1):time(X2)}.
{terminatedAt(meeting(X0,X1),X2):person(X0):person(X1):time(X2)}.
#minimize{ initiatedAt(fighting(X0,X1),X2),initiatedAt(moving(X0,X1),X2),initiatedAt(meeting(X0,X1),X2),terminatedAt(fighting(X0,X1),X2),terminatedAt(moving(X0,X1),X2),terminatedAt(meeting(X0,X1),X2)}.

mode(1,initiatedAt(fighting(X0,X1),X2),initiatedAt(fighting(X0,X1),X2)) :- person(X0),person(X1),time(X2).
mode(2,initiatedAt(fighting(X0,X1),X2),initiatedAt(moving(X0,X1),X2)) :- person(X0),person(X1),time(X2).
mode(3,initiatedAt(fighting(X0,X1),X2),initiatedAt(meeting(X0,X1),X2)) :- person(X0),person(X1),time(X2).
mode(4,initiatedAt(fighting(X0,X1),X2),terminatedAt(fighting(X0,X1),X2)) :- person(X0),person(X1),time(X2).
mode(5,initiatedAt(fighting(X0,X1),X2),terminatedAt(moving(X0,X1),X2)) :- person(X0),person(X1),time(X2).
mode(6,initiatedAt(fighting(X0,X1),X2),terminatedAt(meeting(X0,X1),X2)) :- person(X0),person(X1),time(X2).
mode(1,initiatedAt(moving(X0,X1),X2),initiatedAt(fighting(X0,X1),X2)) :- person(X0),person(X1),time(X2).
mode(2,initiatedAt(moving(X0,X1),X2),initiatedAt(moving(X0,X1),X2)) :- person(X0),person(X1),time(X2).
mode(3,initiatedAt(moving(X0,X1),X2),initiatedAt(meeting(X0,X1),X2)) :- person(X0),person(X1),time(X2).
mode(4,initiatedAt(moving(X0,X1),X2),terminatedAt(fighting(X0,X1),X2)) :- person(X0),person(X1),time(X2).
mode(5,initiatedAt(moving(X0,X1),X2),terminatedAt(moving(X0,X1),X2)) :- person(X0),person(X1),time(X2).
mode(6,initiatedAt(moving(X0,X1),X2),terminatedAt(meeting(X0,X1),X2)) :- person(X0),person(X1),time(X2).
mode(1,initiatedAt(meeting(X0,X1),X2),initiatedAt(fighting(X0,X1),X2)) :- person(X0),person(X1),time(X2).
mode(2,initiatedAt(meeting(X0,X1),X2),initiatedAt(moving(X0,X1),X2)) :- person(X0),person(X1),time(X2).
mode(3,initiatedAt(meeting(X0,X1),X2),initiatedAt(meeting(X0,X1),X2)) :- person(X0),person(X1),time(X2).
mode(4,initiatedAt(meeting(X0,X1),X2),terminatedAt(fighting(X0,X1),X2)) :- person(X0),person(X1),time(X2).
mode(5,initiatedAt(meeting(X0,X1),X2),terminatedAt(moving(X0,X1),X2)) :- person(X0),person(X1),time(X2).
mode(6,initiatedAt(meeting(X0,X1),X2),terminatedAt(meeting(X0,X1),X2)) :- person(X0),person(X1),time(X2).
mode(1,terminatedAt(fighting(X0,X1),X2),initiatedAt(fighting(X0,X1),X2)) :- person(X0),person(X1),time(X2).
mode(2,terminatedAt(fighting(X0,X1),X2),initiatedAt(moving(X0,X1),X2)) :- person(X0),person(X1),time(X2).
mode(3,terminatedAt(fighting(X0,X1),X2),initiatedAt(meeting(X0,X1),X2)) :- person(X0),person(X1),time(X2).
mode(4,terminatedAt(fighting(X0,X1),X2),terminatedAt(fighting(X0,X1),X2)) :- person(X0),person(X1),time(X2).
mode(5,terminatedAt(fighting(X0,X1),X2),terminatedAt(moving(X0,X1),X2)) :- person(X0),person(X1),time(X2).
mode(6,terminatedAt(fighting(X0,X1),X2),terminatedAt(meeting(X0,X1),X2)) :- person(X0),person(X1),time(X2).
mode(1,terminatedAt(moving(X0,X1),X2),initiatedAt(fighting(X0,X1),X2)) :- person(X0),person(X1),time(X2).
mode(2,terminatedAt(moving(X0,X1),X2),initiatedAt(moving(X0,X1),X2)) :- person(X0),person(X1),time(X2).
mode(3,terminatedAt(moving(X0,X1),X2),initiatedAt(meeting(X0,X1),X2)) :- person(X0),person(X1),time(X2).
mode(4,terminatedAt(moving(X0,X1),X2),terminatedAt(fighting(X0,X1),X2)) :- person(X0),person(X1),time(X2).
mode(5,terminatedAt(moving(X0,X1),X2),terminatedAt(moving(X0,X1),X2)) :- person(X0),person(X1),time(X2).
mode(6,terminatedAt(moving(X0,X1),X2),terminatedAt(meeting(X0,X1),X2)) :- person(X0),person(X1),time(X2).
mode(1,terminatedAt(meeting(X0,X1),X2),initiatedAt(fighting(X0,X1),X2)) :- person(X0),person(X1),time(X2).
mode(2,terminatedAt(meeting(X0,X1),X2),initiatedAt(moving(X0,X1),X2)) :- person(X0),person(X1),time(X2).
mode(3,terminatedAt(meeting(X0,X1),X2),initiatedAt(meeting(X0,X1),X2)) :- person(X0),person(X1),time(X2).
mode(4,terminatedAt(meeting(X0,X1),X2),terminatedAt(fighting(X0,X1),X2)) :- person(X0),person(X1),time(X2).
mode(5,terminatedAt(meeting(X0,X1),X2),terminatedAt(moving(X0,X1),X2)) :- person(X0),person(X1),time(X2).
mode(6,terminatedAt(meeting(X0,X1),X2),terminatedAt(meeting(X0,X1),X2)) :- person(X0),person(X1),time(X2).
modeCounter(1..6).
matchesMode(ModeCounter,Atom,Mode) :- 
       mode(ModeCounter,Atom, Mode), true(Atom), Atom = Mode.
true(initiatedAt(fighting(X0,X1),X2)) :- initiatedAt(fighting(X0,X1),X2).
true(initiatedAt(moving(X0,X1),X2)) :- initiatedAt(moving(X0,X1),X2).
true(initiatedAt(meeting(X0,X1),X2)) :- initiatedAt(meeting(X0,X1),X2).
true(terminatedAt(fighting(X0,X1),X2)) :- terminatedAt(fighting(X0,X1),X2).
true(terminatedAt(moving(X0,X1),X2)) :- terminatedAt(moving(X0,X1),X2).
true(terminatedAt(meeting(X0,X1),X2)) :- terminatedAt(meeting(X0,X1),X2).
#hide.
#show matchesMode/3.

