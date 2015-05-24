:- example(holdsAt(fighting(X0,X1),X2)), not holdsAt(fighting(X0,X1),X2).
:- holdsAt(fighting(X0,X1),X2), not example(holdsAt(fighting(X0,X1),X2)).
:- example(holdsAt(moving(X0,X1),X2)), not holdsAt(moving(X0,X1),X2).
:- holdsAt(moving(X0,X1),X2), not example(holdsAt(moving(X0,X1),X2)).
:- example(holdsAt(meeting(X0,X1),X2)), not holdsAt(meeting(X0,X1),X2).
:- holdsAt(meeting(X0,X1),X2), not example(holdsAt(meeting(X0,X1),X2)).