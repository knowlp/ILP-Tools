initiatedAt(moving(id1,id2),14480).
initiatedAt(moving(id2,id1),14480).
initiatedAt(meeting(id1,id2),12680).
initiatedAt(meeting(id1,id2),29000).
initiatedAt(meeting(id2,id1),12680).
initiatedAt(meeting(id2,id1),29000).
terminatedAt(moving(id1,id2),29000).
terminatedAt(moving(id2,id1),29000).
terminatedAt(meeting(id1,id2),14480).
terminatedAt(meeting(id2,id1),14480).
#hide.
#show happensAt(walking(id2),14480).
#show happensAt(standing(id1),14480).
#show happensAt(active(id2),14480).
#show happensAt(inactive(id1),14480).
#show happensAt(abrupt(id2),14480).
#show distLessThan(id2,id1,X0,14480).
#show distMoreThan(id2,id2,X0,14480).
#show orientClose(id2,id1,X0,14480).
#show orientFar(id1,id1,X0,14480).
#show holdsAt(visible(id1),14480).
#show happensAt(enters(id2),14480).
#show happensAt(exits(id2),14480).
