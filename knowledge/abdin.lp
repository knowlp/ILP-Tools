0{initiatedAt(moving(X0,X1),X2):person(X0):person(X1):time(X2)}10.
0{terminatedAt(moving(X0,X1),X2):person(X0):person(X1):time(X2)}10.

#minimize{initiatedAt(moving(X0,X1),X2),terminatedAt(moving(X0,X1),X2)}.


:- holdsAt(moving(X0,X1),X2), not example(holdsAt(moving(X0,X1),X2)).
:- example(holdsAt(moving(X0,X1),X2)), not holdsAt(moving(X0,X1),X2).


#hide.


#show initiatedAt(moving(X0,X1),X2).
#show terminatedAt(moving(X0,X1),X2).
