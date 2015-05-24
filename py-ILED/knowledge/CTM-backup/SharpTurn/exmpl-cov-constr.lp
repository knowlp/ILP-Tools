:- example(holdsAt(sharp_turn(X,Y,Z),T)), not holdsAt(sharp_turn(X,Y,Z),T).
:- holdsAt(sharp_turn(X,Y,Z),T), not example(holdsAt(sharp_turn(X,Y,Z),T)).
