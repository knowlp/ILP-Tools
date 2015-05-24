This folder contains background knowledge file and mode declarations
for the Punctuality high-level event. To experiment with learning Punctuality
copy the contents of bk.lp and modes.pl in the corresponding files in the 
/knowledge folder. Also, modify the core.py file in the source, so that the
example_patterns list is as follows:

example_patterns = [#'holdsAt(passenger_satisfaction(X,Y,Z),T)',
                    #'holdsAt(driving_quality(X,Y,Z),T)',
                    #'holdsAt(driving_style(X,Y,Z),T)',
                    'holdsAt(punctuality(X,Y,Z),T)',
                    #'holdsAt(sharp_turn(X,Y,Z),T)',
                    #'holdsAt(abrupt_acceleration(X,Y,Z),T)',
                    #'holdsAt(abrupt_deceleration(X,Y,Z),T)',
                    #'holdsAt(passenger_density(X,Y,Z),T)',
                    #'holdsAt(noise_level(X,Y,Z),T)',
                    #'holdsAt(internal_temperature(X,Y,Z),T)',
                    ] 
(so comment out all other high-level events). To run the application and learn
a definition for this high-level event run main.py from the source folder. The 
output hypothesis is written in the theory.lp file in the root folder. The target
(optimal) definition for the SharpTurn HLE is:

terminatedAt(punctuality(X3,X1,non_punctual),X2) :- 
        happensAt(my_stop_enter(X3,X1,stopId,early),X2).
initiatedAt(punctuality(X3,X1,punctual),X2) :- 
        happensAt(my_stop_enter(X3,X1,stopId,early),X2).
terminatedAt(punctuality(X3,X1,non_punctual),X2) :- 
        happensAt(my_stop_enter(X3,X1,stopId,scheduled),X2).
terminatedAt(punctuality(X2,X1,punctual),X3) :- 
        happensAt(my_stop_enter(X2,X1,stopId,late),X3).
initiatedAt(punctuality(X3,X1,non_punctual),X2) :- 
        happensAt(my_stop_leave(X3,X1,stopId,early),X2).
initiatedAt(punctuality(X2,X1,non_punctual),X3) :- 
        happensAt(my_stop_enter(X2,X1,stopId,late),X3).
terminatedAt(punctuality(X3,X1,punctual),X2) :- 
        happensAt(my_stop_leave(X3,X1,stopId,early),X2).
initiatedAt(punctuality(X3,X1,punctual),X2) :- 
        happensAt(my_stop_enter(X3,X1,stopId,scheduled),X2).

You can comment-out unnecessary mode declarations (or add extra ones) to experiment
with the quality of the output hypothesis. To comment-out a mode declariation, preceed
it with %

