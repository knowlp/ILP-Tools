
posNotCovered(holdsAt(fighting(X0,X1),X2)) :- example(holdsAt(fighting(X0,X1),X2)), not holdsAt(fighting(X0,X1),X2).

negsCovered(holdsAt(fighting(X0,X1),X2)):- holdsAt(fighting(X0,X1),X2), not example(holdsAt(fighting(X0,X1),X2)).


posCovered(holdsAt(fighting(X0,X1),X2)):- holdsAt(fighting(X0,X1),X2), example(holdsAt(fighting(X0,X1),X2)).


posNotCovered(holdsAt(moving(X0,X1),X2)) :- example(holdsAt(moving(X0,X1),X2)), not holdsAt(moving(X0,X1),X2).

negsCovered(holdsAt(moving(X0,X1),X2)):- holdsAt(moving(X0,X1),X2), not example(holdsAt(moving(X0,X1),X2)).


posCovered(holdsAt(moving(X0,X1),X2)):- holdsAt(moving(X0,X1),X2), example(holdsAt(moving(X0,X1),X2)).


posNotCovered(holdsAt(meeting(X0,X1),X2)) :- example(holdsAt(meeting(X0,X1),X2)), not holdsAt(meeting(X0,X1),X2).

negsCovered(holdsAt(meeting(X0,X1),X2)):- holdsAt(meeting(X0,X1),X2), not example(holdsAt(meeting(X0,X1),X2)).


posCovered(holdsAt(meeting(X0,X1),X2)):- holdsAt(meeting(X0,X1),X2), example(holdsAt(meeting(X0,X1),X2)).


#hide.
#show posCovered/1.
