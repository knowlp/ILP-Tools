This folder contains background knowledge file and mode declarations
for the SharpTurn high-level event. To experiment with learning SharpTurn
copy the contents of bk.lp and modes.pl in the corresponding files in the 
/knowledge folder. Also, modify the core.py file in the source, so that the
example_patterns list is as follows:

example_patterns = [#'holdsAt(passenger_satisfaction(X,Y,Z),T)',
                    #'holdsAt(driving_quality(X,Y,Z),T)',
                    #'holdsAt(driving_style(X,Y,Z),T)',
                    #'holdsAt(punctuality(X,Y,Z),T)',
                     'holdsAt(sharp_turn(X,Y,Z),T)',
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

terminatedAt(sharp_turn(X2,X1,sharp),X3) :- 
        happensAt(sharp_turn_end(X2,X1,sharp),X3).
terminatedAt(sharp_turn(X3,X1,very_sharp),X2) :- 
        happensAt(sharp_turn_end(X3,X1,very_sharp),X2).
initiatedAt(sharp_turn(X2,X1,sharp),X3) :- 
        happensAt(sharp_turn_start(X2,X1,sharp),X3).
initiatedAt(sharp_turn(X3,X1,very_sharp),X2) :- 
        happensAt(sharp_turn_start(X3,X1,very_sharp),X2).

You can comment-out unnecessary mode declarations (or add extra ones) to experiment
with the quality of the output hypothesis. To comment-out a mode declariation, preceed
it with %

