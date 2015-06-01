terminatedAt(meeting(X0,X1),X2) :- 
      use(1,0),
      try(1,1,vars(X0,X2)),
      try(1,2,vars(X1,X2)),
      try(1,3,vars(X1,X0,X2)),
      try(1,4,vars(X0,X1,X2)),
      try(1,5,vars(X1,X0,X2)),
      try(1,6,vars(X0,X1,X2)),
      try(1,7,vars(X1,X0,X2)),
      try(1,8,vars(X0,X1,X2)),
      try(1,9,vars(X1,X0,X2)),
      try(1,10,vars(X0,X1,X2)),
      try(1,11,vars(X1,X0,X2)),
      try(1,12,vars(X0,X1,X2)),
      try(1,13,vars(X0,X2)),
      try(1,14,vars(X1,X2)),
      person(X0),
      time(X2),
      person(X1).
try(1,1,vars(X0,X2)) :- 
      use(1,1),
      happensAt(walking(X0),X2).
try(1,2,vars(X1,X2)) :- 
      use(1,2),
      happensAt(inactive(X1),X2).
try(1,3,vars(X1,X0,X2)) :- 
      use(1,3),
      distLessThan(X1,X0,34,X2).
try(1,4,vars(X0,X1,X2)) :- 
      use(1,4),
      distLessThan(X0,X1,34,X2).
try(1,5,vars(X1,X0,X2)) :- 
      use(1,5),
      distLessThan(X1,X0,27,X2).
try(1,6,vars(X0,X1,X2)) :- 
      use(1,6),
      distLessThan(X0,X1,27,X2).
try(1,7,vars(X1,X0,X2)) :- 
      use(1,7),
      distLessThan(X1,X0,25,X2).
try(1,8,vars(X0,X1,X2)) :- 
      use(1,8),
      distLessThan(X0,X1,25,X2).
try(1,9,vars(X1,X0,X2)) :- 
      use(1,9),
      distMoreThan(X1,X0,24,X2).
try(1,10,vars(X0,X1,X2)) :- 
      use(1,10),
      distMoreThan(X0,X1,24,X2).
try(1,11,vars(X1,X0,X2)) :- 
      use(1,11),
      orientClose(X1,X0,45,X2).
try(1,12,vars(X0,X1,X2)) :- 
      use(1,12),
      orientClose(X0,X1,45,X2).
try(1,13,vars(X0,X2)) :- 
      use(1,13),
      holdsAt(visible(X0),X2).
try(1,14,vars(X1,X2)) :- 
      use(1,14),
      holdsAt(visible(X1),X2).
try(1,1,vars(X0,X2)) :- 
      not use(1,1),
      person(X0),
      time(X2).
try(1,2,vars(X1,X2)) :- 
      not use(1,2),
      person(X1),
      time(X2).
try(1,3,vars(X1,X0,X2)) :- 
      not use(1,3),
      person(X1),
      person(X0),
      time(X2).
try(1,4,vars(X0,X1,X2)) :- 
      not use(1,4),
      person(X0),
      person(X1),
      time(X2).
try(1,5,vars(X1,X0,X2)) :- 
      not use(1,5),
      person(X1),
      person(X0),
      time(X2).
try(1,6,vars(X0,X1,X2)) :- 
      not use(1,6),
      person(X0),
      person(X1),
      time(X2).
try(1,7,vars(X1,X0,X2)) :- 
      not use(1,7),
      person(X1),
      person(X0),
      time(X2).
try(1,8,vars(X0,X1,X2)) :- 
      not use(1,8),
      person(X0),
      person(X1),
      time(X2).
try(1,9,vars(X1,X0,X2)) :- 
      not use(1,9),
      person(X1),
      person(X0),
      time(X2).
try(1,10,vars(X0,X1,X2)) :- 
      not use(1,10),
      person(X0),
      person(X1),
      time(X2).
try(1,11,vars(X1,X0,X2)) :- 
      not use(1,11),
      person(X1),
      person(X0),
      time(X2).
try(1,12,vars(X0,X1,X2)) :- 
      not use(1,12),
      person(X0),
      person(X1),
      time(X2).
try(1,13,vars(X0,X2)) :- 
      not use(1,13),
      person(X0),
      time(X2).
try(1,14,vars(X1,X2)) :- 
      not use(1,14),
      person(X1),
      time(X2).
terminatedAt(meeting(X0,X1),X2) :- 
      use(2,0),
      try(2,1,vars(X1,X2)),
      try(2,2,vars(X0,X2)),
      try(2,3,vars(X0,X1,X2)),
      try(2,4,vars(X1,X0,X2)),
      try(2,5,vars(X0,X1,X2)),
      try(2,6,vars(X1,X0,X2)),
      try(2,7,vars(X0,X1,X2)),
      try(2,8,vars(X1,X0,X2)),
      try(2,9,vars(X0,X1,X2)),
      try(2,10,vars(X1,X0,X2)),
      try(2,11,vars(X0,X1,X2)),
      try(2,12,vars(X1,X0,X2)),
      try(2,13,vars(X1,X2)),
      try(2,14,vars(X0,X2)),
      person(X1),
      time(X2),
      person(X0).
try(2,1,vars(X1,X2)) :- 
      use(2,1),
      happensAt(walking(X1),X2).
try(2,2,vars(X0,X2)) :- 
      use(2,2),
      happensAt(inactive(X0),X2).
try(2,3,vars(X0,X1,X2)) :- 
      use(2,3),
      distLessThan(X0,X1,34,X2).
try(2,4,vars(X1,X0,X2)) :- 
      use(2,4),
      distLessThan(X1,X0,34,X2).
try(2,5,vars(X0,X1,X2)) :- 
      use(2,5),
      distLessThan(X0,X1,27,X2).
try(2,6,vars(X1,X0,X2)) :- 
      use(2,6),
      distLessThan(X1,X0,27,X2).
try(2,7,vars(X0,X1,X2)) :- 
      use(2,7),
      distLessThan(X0,X1,25,X2).
try(2,8,vars(X1,X0,X2)) :- 
      use(2,8),
      distLessThan(X1,X0,25,X2).
try(2,9,vars(X0,X1,X2)) :- 
      use(2,9),
      distMoreThan(X0,X1,24,X2).
try(2,10,vars(X1,X0,X2)) :- 
      use(2,10),
      distMoreThan(X1,X0,24,X2).
try(2,11,vars(X0,X1,X2)) :- 
      use(2,11),
      orientClose(X0,X1,45,X2).
try(2,12,vars(X1,X0,X2)) :- 
      use(2,12),
      orientClose(X1,X0,45,X2).
try(2,13,vars(X1,X2)) :- 
      use(2,13),
      holdsAt(visible(X1),X2).
try(2,14,vars(X0,X2)) :- 
      use(2,14),
      holdsAt(visible(X0),X2).
try(2,1,vars(X1,X2)) :- 
      not use(2,1),
      person(X1),
      time(X2).
try(2,2,vars(X0,X2)) :- 
      not use(2,2),
      person(X0),
      time(X2).
try(2,3,vars(X0,X1,X2)) :- 
      not use(2,3),
      person(X0),
      person(X1),
      time(X2).
try(2,4,vars(X1,X0,X2)) :- 
      not use(2,4),
      person(X1),
      person(X0),
      time(X2).
try(2,5,vars(X0,X1,X2)) :- 
      not use(2,5),
      person(X0),
      person(X1),
      time(X2).
try(2,6,vars(X1,X0,X2)) :- 
      not use(2,6),
      person(X1),
      person(X0),
      time(X2).
try(2,7,vars(X0,X1,X2)) :- 
      not use(2,7),
      person(X0),
      person(X1),
      time(X2).
try(2,8,vars(X1,X0,X2)) :- 
      not use(2,8),
      person(X1),
      person(X0),
      time(X2).
try(2,9,vars(X0,X1,X2)) :- 
      not use(2,9),
      person(X0),
      person(X1),
      time(X2).
try(2,10,vars(X1,X0,X2)) :- 
      not use(2,10),
      person(X1),
      person(X0),
      time(X2).
try(2,11,vars(X0,X1,X2)) :- 
      not use(2,11),
      person(X0),
      person(X1),
      time(X2).
try(2,12,vars(X1,X0,X2)) :- 
      not use(2,12),
      person(X1),
      person(X0),
      time(X2).
try(2,13,vars(X1,X2)) :- 
      not use(2,13),
      person(X1),
      time(X2).
try(2,14,vars(X0,X2)) :- 
      not use(2,14),
      person(X0),
      time(X2).
terminatedAt(moving(X0,X1),X2) :- 
      use(3,0),
      try(3,1,vars(X0,X2)),
      try(3,2,vars(X1,X2)),
      try(3,3,vars(X1,X0,X2)),
      try(3,4,vars(X0,X1,X2)),
      try(3,5,vars(X1,X0,X2)),
      try(3,6,vars(X0,X1,X2)),
      try(3,7,vars(X1,X0,X2)),
      try(3,8,vars(X0,X1,X2)),
      try(3,9,vars(X1,X0,X2)),
      try(3,10,vars(X0,X1,X2)),
      try(3,11,vars(X1,X0,X2)),
      try(3,12,vars(X0,X1,X2)),
      try(3,13,vars(X0,X2)),
      try(3,14,vars(X1,X2)),
      person(X0),
      time(X2),
      person(X1).
try(3,1,vars(X0,X2)) :- 
      use(3,1),
      happensAt(active(X0),X2).
try(3,2,vars(X1,X2)) :- 
      use(3,2),
      happensAt(active(X1),X2).
try(3,3,vars(X1,X0,X2)) :- 
      use(3,3),
      distLessThan(X1,X0,34,X2).
try(3,4,vars(X0,X1,X2)) :- 
      use(3,4),
      distLessThan(X0,X1,34,X2).
try(3,5,vars(X1,X0,X2)) :- 
      use(3,5),
      distLessThan(X1,X0,27,X2).
try(3,6,vars(X0,X1,X2)) :- 
      use(3,6),
      distLessThan(X0,X1,27,X2).
try(3,7,vars(X1,X0,X2)) :- 
      use(3,7),
      distLessThan(X1,X0,24,X2).
try(3,8,vars(X0,X1,X2)) :- 
      use(3,8),
      distLessThan(X0,X1,24,X2).
try(3,9,vars(X1,X0,X2)) :- 
      use(3,9),
      distLessThan(X1,X0,25,X2).
try(3,10,vars(X0,X1,X2)) :- 
      use(3,10),
      distLessThan(X0,X1,25,X2).
try(3,11,vars(X1,X0,X2)) :- 
      use(3,11),
      orientClose(X1,X0,45,X2).
try(3,12,vars(X0,X1,X2)) :- 
      use(3,12),
      orientClose(X0,X1,45,X2).
try(3,13,vars(X0,X2)) :- 
      use(3,13),
      holdsAt(visible(X0),X2).
try(3,14,vars(X1,X2)) :- 
      use(3,14),
      holdsAt(visible(X1),X2).
try(3,1,vars(X0,X2)) :- 
      not use(3,1),
      person(X0),
      time(X2).
try(3,2,vars(X1,X2)) :- 
      not use(3,2),
      person(X1),
      time(X2).
try(3,3,vars(X1,X0,X2)) :- 
      not use(3,3),
      person(X1),
      person(X0),
      time(X2).
try(3,4,vars(X0,X1,X2)) :- 
      not use(3,4),
      person(X0),
      person(X1),
      time(X2).
try(3,5,vars(X1,X0,X2)) :- 
      not use(3,5),
      person(X1),
      person(X0),
      time(X2).
try(3,6,vars(X0,X1,X2)) :- 
      not use(3,6),
      person(X0),
      person(X1),
      time(X2).
try(3,7,vars(X1,X0,X2)) :- 
      not use(3,7),
      person(X1),
      person(X0),
      time(X2).
try(3,8,vars(X0,X1,X2)) :- 
      not use(3,8),
      person(X0),
      person(X1),
      time(X2).
try(3,9,vars(X1,X0,X2)) :- 
      not use(3,9),
      person(X1),
      person(X0),
      time(X2).
try(3,10,vars(X0,X1,X2)) :- 
      not use(3,10),
      person(X0),
      person(X1),
      time(X2).
try(3,11,vars(X1,X0,X2)) :- 
      not use(3,11),
      person(X1),
      person(X0),
      time(X2).
try(3,12,vars(X0,X1,X2)) :- 
      not use(3,12),
      person(X0),
      person(X1),
      time(X2).
try(3,13,vars(X0,X2)) :- 
      not use(3,13),
      person(X0),
      time(X2).
try(3,14,vars(X1,X2)) :- 
      not use(3,14),
      person(X1),
      time(X2).
terminatedAt(moving(X0,X1),X2) :- 
      use(4,0),
      try(4,1,vars(X1,X2)),
      try(4,2,vars(X0,X2)),
      try(4,3,vars(X0,X1,X2)),
      try(4,4,vars(X1,X0,X2)),
      try(4,5,vars(X0,X1,X2)),
      try(4,6,vars(X1,X0,X2)),
      try(4,7,vars(X0,X1,X2)),
      try(4,8,vars(X1,X0,X2)),
      try(4,9,vars(X0,X1,X2)),
      try(4,10,vars(X1,X0,X2)),
      try(4,11,vars(X0,X1,X2)),
      try(4,12,vars(X1,X0,X2)),
      try(4,13,vars(X1,X2)),
      try(4,14,vars(X0,X2)),
      person(X1),
      time(X2),
      person(X0).
try(4,1,vars(X1,X2)) :- 
      use(4,1),
      happensAt(active(X1),X2).
try(4,2,vars(X0,X2)) :- 
      use(4,2),
      happensAt(active(X0),X2).
try(4,3,vars(X0,X1,X2)) :- 
      use(4,3),
      distLessThan(X0,X1,34,X2).
try(4,4,vars(X1,X0,X2)) :- 
      use(4,4),
      distLessThan(X1,X0,34,X2).
try(4,5,vars(X0,X1,X2)) :- 
      use(4,5),
      distLessThan(X0,X1,27,X2).
try(4,6,vars(X1,X0,X2)) :- 
      use(4,6),
      distLessThan(X1,X0,27,X2).
try(4,7,vars(X0,X1,X2)) :- 
      use(4,7),
      distLessThan(X0,X1,24,X2).
try(4,8,vars(X1,X0,X2)) :- 
      use(4,8),
      distLessThan(X1,X0,24,X2).
try(4,9,vars(X0,X1,X2)) :- 
      use(4,9),
      distLessThan(X0,X1,25,X2).
try(4,10,vars(X1,X0,X2)) :- 
      use(4,10),
      distLessThan(X1,X0,25,X2).
try(4,11,vars(X0,X1,X2)) :- 
      use(4,11),
      orientClose(X0,X1,45,X2).
try(4,12,vars(X1,X0,X2)) :- 
      use(4,12),
      orientClose(X1,X0,45,X2).
try(4,13,vars(X1,X2)) :- 
      use(4,13),
      holdsAt(visible(X1),X2).
try(4,14,vars(X0,X2)) :- 
      use(4,14),
      holdsAt(visible(X0),X2).
try(4,1,vars(X1,X2)) :- 
      not use(4,1),
      person(X1),
      time(X2).
try(4,2,vars(X0,X2)) :- 
      not use(4,2),
      person(X0),
      time(X2).
try(4,3,vars(X0,X1,X2)) :- 
      not use(4,3),
      person(X0),
      person(X1),
      time(X2).
try(4,4,vars(X1,X0,X2)) :- 
      not use(4,4),
      person(X1),
      person(X0),
      time(X2).
try(4,5,vars(X0,X1,X2)) :- 
      not use(4,5),
      person(X0),
      person(X1),
      time(X2).
try(4,6,vars(X1,X0,X2)) :- 
      not use(4,6),
      person(X1),
      person(X0),
      time(X2).
try(4,7,vars(X0,X1,X2)) :- 
      not use(4,7),
      person(X0),
      person(X1),
      time(X2).
try(4,8,vars(X1,X0,X2)) :- 
      not use(4,8),
      person(X1),
      person(X0),
      time(X2).
try(4,9,vars(X0,X1,X2)) :- 
      not use(4,9),
      person(X0),
      person(X1),
      time(X2).
try(4,10,vars(X1,X0,X2)) :- 
      not use(4,10),
      person(X1),
      person(X0),
      time(X2).
try(4,11,vars(X0,X1,X2)) :- 
      not use(4,11),
      person(X0),
      person(X1),
      time(X2).
try(4,12,vars(X1,X0,X2)) :- 
      not use(4,12),
      person(X1),
      person(X0),
      time(X2).
try(4,13,vars(X1,X2)) :- 
      not use(4,13),
      person(X1),
      time(X2).
try(4,14,vars(X0,X2)) :- 
      not use(4,14),
      person(X0),
      time(X2).
initiatedAt(meeting(X0,X1),X2) :- 
      use(5,0),
      try(5,1,vars(X0,X2)),
      try(5,2,vars(X1,X2)),
      try(5,3,vars(X1,X0,X2)),
      try(5,4,vars(X0,X1,X2)),
      try(5,5,vars(X1,X0,X2)),
      try(5,6,vars(X0,X1,X2)),
      try(5,7,vars(X1,X0,X2)),
      try(5,8,vars(X0,X1,X2)),
      try(5,9,vars(X1,X0,X2)),
      try(5,10,vars(X0,X1,X2)),
      try(5,11,vars(X1,X0,X2)),
      try(5,12,vars(X0,X1,X2)),
      try(5,13,vars(X0,X2)),
      try(5,14,vars(X1,X2)),
      person(X0),
      time(X2),
      person(X1).
try(5,1,vars(X0,X2)) :- 
      use(5,1),
      happensAt(active(X0),X2).
try(5,2,vars(X1,X2)) :- 
      use(5,2),
      happensAt(active(X1),X2).
try(5,3,vars(X1,X0,X2)) :- 
      use(5,3),
      distLessThan(X1,X0,34,X2).
try(5,4,vars(X0,X1,X2)) :- 
      use(5,4),
      distLessThan(X0,X1,34,X2).
try(5,5,vars(X1,X0,X2)) :- 
      use(5,5),
      distLessThan(X1,X0,27,X2).
try(5,6,vars(X0,X1,X2)) :- 
      use(5,6),
      distLessThan(X0,X1,27,X2).
try(5,7,vars(X1,X0,X2)) :- 
      use(5,7),
      distLessThan(X1,X0,24,X2).
try(5,8,vars(X0,X1,X2)) :- 
      use(5,8),
      distLessThan(X0,X1,24,X2).
try(5,9,vars(X1,X0,X2)) :- 
      use(5,9),
      distLessThan(X1,X0,25,X2).
try(5,10,vars(X0,X1,X2)) :- 
      use(5,10),
      distLessThan(X0,X1,25,X2).
try(5,11,vars(X1,X0,X2)) :- 
      use(5,11),
      orientClose(X1,X0,45,X2).
try(5,12,vars(X0,X1,X2)) :- 
      use(5,12),
      orientClose(X0,X1,45,X2).
try(5,13,vars(X0,X2)) :- 
      use(5,13),
      holdsAt(visible(X0),X2).
try(5,14,vars(X1,X2)) :- 
      use(5,14),
      holdsAt(visible(X1),X2).
try(5,1,vars(X0,X2)) :- 
      not use(5,1),
      person(X0),
      time(X2).
try(5,2,vars(X1,X2)) :- 
      not use(5,2),
      person(X1),
      time(X2).
try(5,3,vars(X1,X0,X2)) :- 
      not use(5,3),
      person(X1),
      person(X0),
      time(X2).
try(5,4,vars(X0,X1,X2)) :- 
      not use(5,4),
      person(X0),
      person(X1),
      time(X2).
try(5,5,vars(X1,X0,X2)) :- 
      not use(5,5),
      person(X1),
      person(X0),
      time(X2).
try(5,6,vars(X0,X1,X2)) :- 
      not use(5,6),
      person(X0),
      person(X1),
      time(X2).
try(5,7,vars(X1,X0,X2)) :- 
      not use(5,7),
      person(X1),
      person(X0),
      time(X2).
try(5,8,vars(X0,X1,X2)) :- 
      not use(5,8),
      person(X0),
      person(X1),
      time(X2).
try(5,9,vars(X1,X0,X2)) :- 
      not use(5,9),
      person(X1),
      person(X0),
      time(X2).
try(5,10,vars(X0,X1,X2)) :- 
      not use(5,10),
      person(X0),
      person(X1),
      time(X2).
try(5,11,vars(X1,X0,X2)) :- 
      not use(5,11),
      person(X1),
      person(X0),
      time(X2).
try(5,12,vars(X0,X1,X2)) :- 
      not use(5,12),
      person(X0),
      person(X1),
      time(X2).
try(5,13,vars(X0,X2)) :- 
      not use(5,13),
      person(X0),
      time(X2).
try(5,14,vars(X1,X2)) :- 
      not use(5,14),
      person(X1),
      time(X2).
initiatedAt(meeting(X0,X1),X2) :- 
      use(6,0),
      try(6,1,vars(X1,X2)),
      try(6,2,vars(X0,X2)),
      try(6,3,vars(X0,X1,X2)),
      try(6,4,vars(X1,X0,X2)),
      try(6,5,vars(X0,X1,X2)),
      try(6,6,vars(X1,X0,X2)),
      try(6,7,vars(X0,X1,X2)),
      try(6,8,vars(X1,X0,X2)),
      try(6,9,vars(X0,X1,X2)),
      try(6,10,vars(X1,X0,X2)),
      try(6,11,vars(X0,X1,X2)),
      try(6,12,vars(X1,X0,X2)),
      try(6,13,vars(X1,X2)),
      try(6,14,vars(X0,X2)),
      person(X1),
      time(X2),
      person(X0).
try(6,1,vars(X1,X2)) :- 
      use(6,1),
      happensAt(active(X1),X2).
try(6,2,vars(X0,X2)) :- 
      use(6,2),
      happensAt(active(X0),X2).
try(6,3,vars(X0,X1,X2)) :- 
      use(6,3),
      distLessThan(X0,X1,34,X2).
try(6,4,vars(X1,X0,X2)) :- 
      use(6,4),
      distLessThan(X1,X0,34,X2).
try(6,5,vars(X0,X1,X2)) :- 
      use(6,5),
      distLessThan(X0,X1,27,X2).
try(6,6,vars(X1,X0,X2)) :- 
      use(6,6),
      distLessThan(X1,X0,27,X2).
try(6,7,vars(X0,X1,X2)) :- 
      use(6,7),
      distLessThan(X0,X1,24,X2).
try(6,8,vars(X1,X0,X2)) :- 
      use(6,8),
      distLessThan(X1,X0,24,X2).
try(6,9,vars(X0,X1,X2)) :- 
      use(6,9),
      distLessThan(X0,X1,25,X2).
try(6,10,vars(X1,X0,X2)) :- 
      use(6,10),
      distLessThan(X1,X0,25,X2).
try(6,11,vars(X0,X1,X2)) :- 
      use(6,11),
      orientClose(X0,X1,45,X2).
try(6,12,vars(X1,X0,X2)) :- 
      use(6,12),
      orientClose(X1,X0,45,X2).
try(6,13,vars(X1,X2)) :- 
      use(6,13),
      holdsAt(visible(X1),X2).
try(6,14,vars(X0,X2)) :- 
      use(6,14),
      holdsAt(visible(X0),X2).
try(6,1,vars(X1,X2)) :- 
      not use(6,1),
      person(X1),
      time(X2).
try(6,2,vars(X0,X2)) :- 
      not use(6,2),
      person(X0),
      time(X2).
try(6,3,vars(X0,X1,X2)) :- 
      not use(6,3),
      person(X0),
      person(X1),
      time(X2).
try(6,4,vars(X1,X0,X2)) :- 
      not use(6,4),
      person(X1),
      person(X0),
      time(X2).
try(6,5,vars(X0,X1,X2)) :- 
      not use(6,5),
      person(X0),
      person(X1),
      time(X2).
try(6,6,vars(X1,X0,X2)) :- 
      not use(6,6),
      person(X1),
      person(X0),
      time(X2).
try(6,7,vars(X0,X1,X2)) :- 
      not use(6,7),
      person(X0),
      person(X1),
      time(X2).
try(6,8,vars(X1,X0,X2)) :- 
      not use(6,8),
      person(X1),
      person(X0),
      time(X2).
try(6,9,vars(X0,X1,X2)) :- 
      not use(6,9),
      person(X0),
      person(X1),
      time(X2).
try(6,10,vars(X1,X0,X2)) :- 
      not use(6,10),
      person(X1),
      person(X0),
      time(X2).
try(6,11,vars(X0,X1,X2)) :- 
      not use(6,11),
      person(X0),
      person(X1),
      time(X2).
try(6,12,vars(X1,X0,X2)) :- 
      not use(6,12),
      person(X1),
      person(X0),
      time(X2).
try(6,13,vars(X1,X2)) :- 
      not use(6,13),
      person(X1),
      time(X2).
try(6,14,vars(X0,X2)) :- 
      not use(6,14),
      person(X0),
      time(X2).
initiatedAt(moving(X0,X1),X2) :- 
      use(7,0),
      try(7,1,vars(X0,X2)),
      try(7,2,vars(X1,X2)),
      try(7,3,vars(X1,X0,X2)),
      try(7,4,vars(X0,X1,X2)),
      try(7,5,vars(X1,X0,X2)),
      try(7,6,vars(X0,X1,X2)),
      try(7,7,vars(X1,X0,X2)),
      try(7,8,vars(X0,X1,X2)),
      try(7,9,vars(X1,X0,X2)),
      try(7,10,vars(X0,X1,X2)),
      try(7,11,vars(X1,X0,X2)),
      try(7,12,vars(X0,X1,X2)),
      try(7,13,vars(X0,X2)),
      try(7,14,vars(X1,X2)),
      person(X0),
      time(X2),
      person(X1).
try(7,1,vars(X0,X2)) :- 
      use(7,1),
      happensAt(walking(X0),X2).
try(7,2,vars(X1,X2)) :- 
      use(7,2),
      happensAt(walking(X1),X2).
try(7,3,vars(X1,X0,X2)) :- 
      use(7,3),
      distLessThan(X1,X0,34,X2).
try(7,4,vars(X0,X1,X2)) :- 
      use(7,4),
      distLessThan(X0,X1,34,X2).
try(7,5,vars(X1,X0,X2)) :- 
      use(7,5),
      distLessThan(X1,X0,27,X2).
try(7,6,vars(X0,X1,X2)) :- 
      use(7,6),
      distLessThan(X0,X1,27,X2).
try(7,7,vars(X1,X0,X2)) :- 
      use(7,7),
      distLessThan(X1,X0,24,X2).
try(7,8,vars(X0,X1,X2)) :- 
      use(7,8),
      distLessThan(X0,X1,24,X2).
try(7,9,vars(X1,X0,X2)) :- 
      use(7,9),
      distLessThan(X1,X0,25,X2).
try(7,10,vars(X0,X1,X2)) :- 
      use(7,10),
      distLessThan(X0,X1,25,X2).
try(7,11,vars(X1,X0,X2)) :- 
      use(7,11),
      orientClose(X1,X0,45,X2).
try(7,12,vars(X0,X1,X2)) :- 
      use(7,12),
      orientClose(X0,X1,45,X2).
try(7,13,vars(X0,X2)) :- 
      use(7,13),
      happensAt(enters(X0),X2).
try(7,14,vars(X1,X2)) :- 
      use(7,14),
      happensAt(enters(X1),X2).
try(7,1,vars(X0,X2)) :- 
      not use(7,1),
      person(X0),
      time(X2).
try(7,2,vars(X1,X2)) :- 
      not use(7,2),
      person(X1),
      time(X2).
try(7,3,vars(X1,X0,X2)) :- 
      not use(7,3),
      person(X1),
      person(X0),
      time(X2).
try(7,4,vars(X0,X1,X2)) :- 
      not use(7,4),
      person(X0),
      person(X1),
      time(X2).
try(7,5,vars(X1,X0,X2)) :- 
      not use(7,5),
      person(X1),
      person(X0),
      time(X2).
try(7,6,vars(X0,X1,X2)) :- 
      not use(7,6),
      person(X0),
      person(X1),
      time(X2).
try(7,7,vars(X1,X0,X2)) :- 
      not use(7,7),
      person(X1),
      person(X0),
      time(X2).
try(7,8,vars(X0,X1,X2)) :- 
      not use(7,8),
      person(X0),
      person(X1),
      time(X2).
try(7,9,vars(X1,X0,X2)) :- 
      not use(7,9),
      person(X1),
      person(X0),
      time(X2).
try(7,10,vars(X0,X1,X2)) :- 
      not use(7,10),
      person(X0),
      person(X1),
      time(X2).
try(7,11,vars(X1,X0,X2)) :- 
      not use(7,11),
      person(X1),
      person(X0),
      time(X2).
try(7,12,vars(X0,X1,X2)) :- 
      not use(7,12),
      person(X0),
      person(X1),
      time(X2).
try(7,13,vars(X0,X2)) :- 
      not use(7,13),
      person(X0),
      time(X2).
try(7,14,vars(X1,X2)) :- 
      not use(7,14),
      person(X1),
      time(X2).
initiatedAt(moving(X0,X1),X2) :- 
      use(8,0),
      try(8,1,vars(X1,X2)),
      try(8,2,vars(X0,X2)),
      try(8,3,vars(X0,X1,X2)),
      try(8,4,vars(X1,X0,X2)),
      try(8,5,vars(X0,X1,X2)),
      try(8,6,vars(X1,X0,X2)),
      try(8,7,vars(X0,X1,X2)),
      try(8,8,vars(X1,X0,X2)),
      try(8,9,vars(X0,X1,X2)),
      try(8,10,vars(X1,X0,X2)),
      try(8,11,vars(X0,X1,X2)),
      try(8,12,vars(X1,X0,X2)),
      try(8,13,vars(X1,X2)),
      try(8,14,vars(X0,X2)),
      person(X1),
      time(X2),
      person(X0).
try(8,1,vars(X1,X2)) :- 
      use(8,1),
      happensAt(walking(X1),X2).
try(8,2,vars(X0,X2)) :- 
      use(8,2),
      happensAt(walking(X0),X2).
try(8,3,vars(X0,X1,X2)) :- 
      use(8,3),
      distLessThan(X0,X1,34,X2).
try(8,4,vars(X1,X0,X2)) :- 
      use(8,4),
      distLessThan(X1,X0,34,X2).
try(8,5,vars(X0,X1,X2)) :- 
      use(8,5),
      distLessThan(X0,X1,27,X2).
try(8,6,vars(X1,X0,X2)) :- 
      use(8,6),
      distLessThan(X1,X0,27,X2).
try(8,7,vars(X0,X1,X2)) :- 
      use(8,7),
      distLessThan(X0,X1,24,X2).
try(8,8,vars(X1,X0,X2)) :- 
      use(8,8),
      distLessThan(X1,X0,24,X2).
try(8,9,vars(X0,X1,X2)) :- 
      use(8,9),
      distLessThan(X0,X1,25,X2).
try(8,10,vars(X1,X0,X2)) :- 
      use(8,10),
      distLessThan(X1,X0,25,X2).
try(8,11,vars(X0,X1,X2)) :- 
      use(8,11),
      orientClose(X0,X1,45,X2).
try(8,12,vars(X1,X0,X2)) :- 
      use(8,12),
      orientClose(X1,X0,45,X2).
try(8,13,vars(X1,X2)) :- 
      use(8,13),
      happensAt(enters(X1),X2).
try(8,14,vars(X0,X2)) :- 
      use(8,14),
      happensAt(enters(X0),X2).
try(8,1,vars(X1,X2)) :- 
      not use(8,1),
      person(X1),
      time(X2).
try(8,2,vars(X0,X2)) :- 
      not use(8,2),
      person(X0),
      time(X2).
try(8,3,vars(X0,X1,X2)) :- 
      not use(8,3),
      person(X0),
      person(X1),
      time(X2).
try(8,4,vars(X1,X0,X2)) :- 
      not use(8,4),
      person(X1),
      person(X0),
      time(X2).
try(8,5,vars(X0,X1,X2)) :- 
      not use(8,5),
      person(X0),
      person(X1),
      time(X2).
try(8,6,vars(X1,X0,X2)) :- 
      not use(8,6),
      person(X1),
      person(X0),
      time(X2).
try(8,7,vars(X0,X1,X2)) :- 
      not use(8,7),
      person(X0),
      person(X1),
      time(X2).
try(8,8,vars(X1,X0,X2)) :- 
      not use(8,8),
      person(X1),
      person(X0),
      time(X2).
try(8,9,vars(X0,X1,X2)) :- 
      not use(8,9),
      person(X0),
      person(X1),
      time(X2).
try(8,10,vars(X1,X0,X2)) :- 
      not use(8,10),
      person(X1),
      person(X0),
      time(X2).
try(8,11,vars(X0,X1,X2)) :- 
      not use(8,11),
      person(X0),
      person(X1),
      time(X2).
try(8,12,vars(X1,X0,X2)) :- 
      not use(8,12),
      person(X1),
      person(X0),
      time(X2).
try(8,13,vars(X1,X2)) :- 
      not use(8,13),
      person(X1),
      time(X2).
try(8,14,vars(X0,X2)) :- 
      not use(8,14),
      person(X0),
      time(X2).

posNotCovered(holdsAt(fighting(X0,X1),X2)) :- example(holdsAt(fighting(X0,X1),X2)), not holdsAt(fighting(X0,X1),X2).

negsCovered(holdsAt(fighting(X0,X1),X2)):- holdsAt(fighting(X0,X1),X2), not example(holdsAt(fighting(X0,X1),X2)).


posNotCovered(holdsAt(moving(X0,X1),X2)) :- example(holdsAt(moving(X0,X1),X2)), not holdsAt(moving(X0,X1),X2).

negsCovered(holdsAt(moving(X0,X1),X2)):- holdsAt(moving(X0,X1),X2), not example(holdsAt(moving(X0,X1),X2)).


posNotCovered(holdsAt(meeting(X0,X1),X2)) :- example(holdsAt(meeting(X0,X1),X2)), not holdsAt(meeting(X0,X1),X2).

negsCovered(holdsAt(meeting(X0,X1),X2)):- holdsAt(meeting(X0,X1),X2), not example(holdsAt(meeting(X0,X1),X2)).

{use(1,0..14)}.
{use(2,0..14)}.
{use(3,0..14)}.
{use(4,0..14)}.
{use(5,0..14)}.
{use(6,0..14)}.
{use(7,0..14)}.
{use(8,0..14)}.
#minimize{ use(I,J),posNotCovered(holdsAt(fighting(X0,X1),X2)),negsCovered(holdsAt(fighting(X0,X1),X2)),posNotCovered(holdsAt(moving(X0,X1),X2)),negsCovered(holdsAt(moving(X0,X1),X2)),posNotCovered(holdsAt(meeting(X0,X1),X2)),negsCovered(holdsAt(meeting(X0,X1),X2))}.
:- use(I,J),not use(I,0).

#hide.
#show use(I,J).
