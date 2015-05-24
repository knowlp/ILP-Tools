holdsAt(F,Te) :-
       fluent(F), 
       not sdFluent(F),
       initiatedAt(F,Ts),
       Te = Ts + 40,
       time(Ts),time(Te).

holdsAt(F,Te) :-
       fluent(F),
       not sdFluent(F),
       holdsAt(F,Ts),
       not terminatedAt(F,Ts),
       Te = Ts + 40,
       time(Ts),time(Te).

sdFluent(visible(X)):-person(X).

fluent(moving(X,Y)) :- person(X),person(Y),X != Y.
fluent(meeting(X,Y)) :- person(X),person(Y),X != Y.
fluent(fighting(X,Y)) :- person(X),person(Y),X != Y.




person(X):-
    happensAt(walking(X),_).
person(X):-
    happensAt(standing(X),_).
person(X):-
    happensAt(running(X),_).
person(X):-
    happensAt(active(X),_).
person(X):-
    happensAt(inactive(X),_).
person(X):-
    happensAt(abrupt(X),_).

time(T):-
    example(holdsAt(_,T)).
time(X):-
    happensAt(walking(_),X).
time(X):-
    happensAt(standing(_),X).
time(X):-
    happensAt(active(_),X).
time(X):-
    happensAt(inactive(_),X).
time(X):-
    happensAt(abrupt(_),X).


% Lua scripting language for arithmetic
#begin_lua
function eucldist(x1,y1,x2,y2)
   x = x1 - x2
   y = y1 - y2
   xDiff = math.abs(x)
   yDiff = math.abs(y)
   sideA = xDiff * xDiff
   sideB = yDiff * yDiff
   temp = sideA + sideB
   ypot = math.sqrt(temp)
   return ypot
end

function absval(x,y)
   z = x-y
   res = math.abs(z)
   return z
end
#end_lua. 

dist(Id1,Id2,T,Ypot):-
       coords(Id1,X1,Y1,T),
       coords(Id2,X2,Y2,T),
       Id1 != Id2,
       X := X1-X2,
       Y := Y1-Y2,
       %XDiff := #abs(X),
       %YDiff := #abs(Y),
       %SideA := XDiff * XDiff,
       %SideB := YDiff * YDiff,
       %Temp := SideA + SideB,
       %Ypot := sqrt(Temp).
       Ypot := @eucldist(X1,Y1,X2,Y2).

distLessThan(Id1,Id2,Threshold,Time):-
       dist(Id1,Id2,Time,Distance),
       Id1 != Id2,
       threshold_value(Threshold),
       Distance <= Threshold,
       person(Id1),person(Id2),time(Time).

distMoreThan(Id1,Id2,Threshold,Time):-
       dist(Id1,Id2,Time,Distance),
       Id1 != Id2,
       threshold_value(Threshold),
       Distance > Threshold,
       person(Id1),person(Id2),time(Time).

orientClose(Id1,Id2,Threshold,Time):-
       Id1 != Id2,
       orientation(Id1,X,Time),
       orientation(Id2,Y,Time),
       %Diff := @absval(X,Y),
       Diff = #abs(X-Y),
       orientation_threshold(Threshold), 
       Diff <= Threshold,
       person(Id1),person(Id2),time(Time). 

orientFar(Id1,Id2,Threshold,Time):-
       Id1 != Id2,
       orientation(Id1,X,Time),
       orientation(Id2,Y,Time),
       %Diff := @absval(X,Y),
       Diff = #abs(X-Y),
       orientation_threshold(Threshold), 
       Diff > Threshold,
       person(Id1),person(Id2),time(Time).

orientation_threshold(45).
%orientation_threshold(100).
%orientation_threshold(200).
%orientation_threshold(300).
%orientation_threshold(360).

threshold_value(25).
threshold_value(24).
threshold_value(27).
threshold_value(34).
%threshold_value(40).

%threshold_value(5..10). %move
%threshold_value(30). %meet
%threshold_value(20). %fight 

