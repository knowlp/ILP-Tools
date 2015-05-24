{initiatedAt(fighting(X0,X1),X2):person(X1):person(X0):time(X2)}.
{initiatedAt(moving(X0,X1),X2):person(X1):person(X0):time(X2)}.
{initiatedAt(meeting(X0,X1),X2):person(X1):person(X0):time(X2)}.
{terminatedAt(fighting(X0,X1),X2):person(X1):person(X0):time(X2)}.
{terminatedAt(moving(X0,X1),X2):person(X1):person(X0):time(X2)}.
{terminatedAt(meeting(X0,X1),X2):person(X1):person(X0):time(X2)}.
#minimize{ initiatedAt(fighting(X0,X1),X2),initiatedAt(moving(X0,X1),X2),initiatedAt(meeting(X0,X1),X2),terminatedAt(fighting(X0,X1),X2),terminatedAt(moving(X0,X1),X2),terminatedAt(meeting(X0,X1),X2) }.

#hide.

#show initiatedAt(fighting(X0,X1),X2).
#show initiatedAt(moving(X0,X1),X2).
#show initiatedAt(meeting(X0,X1),X2).
#show terminatedAt(fighting(X0,X1),X2).
#show terminatedAt(moving(X0,X1),X2).
#show terminatedAt(meeting(X0,X1),X2).