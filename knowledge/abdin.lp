
:- example(holdsAt(available(X0),X1)), not holdsAt(available(X0),X1).


:- holdsAt(available(X0),X1), not example(holdsAt(available(X0),X1)).

{happens(use(X0),X1):sugar(X0):time(X1)}.
#minimize{ happens(use(X0),X1)}.

#hide.
#show happens(use(X0),X1).

