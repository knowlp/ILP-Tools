This folder contains background knowledge file and mode declarations
for the InternalTemperature high-level event. To experiment with learning InternalTemperature
copy the contents of bk.lp and modes.pl in the corresponding files in the 
/knowledge folder. Also, modify the core.py file in the source, so that the
example_patterns list is as follows:

example_patterns = [#'holdsAt(passenger_satisfaction(X,Y,Z),T)',
                    #'holdsAt(driving_quality(X,Y,Z),T)',
                    #'holdsAt(driving_style(X,Y,Z),T)',
                    #'holdsAt(punctuality(X,Y,Z),T)',
                    #'holdsAt(sharp_turn(X,Y,Z),T)',
                    #'holdsAt(abrupt_acceleration(X,Y,Z),T)',
                    #'holdsAt(abrupt_deceleration(X,Y,Z),T)',
                    #'holdsAt(passenger_density(X,Y,Z),T)',
                    #'holdsAt(noise_level(X,Y,Z),T)',
                    'holdsAt(internal_temperature(X,Y,Z),T)',
                    ] 
(so comment out all other high-level events). To run the application and learn
a definition for this high-level event run main.py from the source folder. The 
output hypothesis is written in the theory.lp file in the root folder. The target
(optimal) definition for the SharpTurn HLE is:

terminatedAt(internal_temperature(X3,X1,very_cold),X2) :- 
        happensAt(internal_temperature_change(X3,X1,very_warm),X2).
initiatedAt(internal_temperature(X3,X2,very_cold),X1) :- 
        happensAt(internal_temperature_change(X3,X2,very_cold),X1).
terminatedAt(internal_temperature(X3,X1,very_cold),X2) :- 
        happensAt(internal_temperature_change(X3,X1,cold),X2).
terminatedAt(internal_temperature(X3,X1,very_cold),X2) :- 
        happensAt(noise_level_change(X3,X1,low),X2),
        happensAt(internal_temperature_change(X3,X1,very_warm),X2).
terminatedAt(internal_temperature(X3,X1,very_cold),X2) :- 
        happensAt(internal_temperature_change(X3,X1,warm),X2).
terminatedAt(internal_temperature(X3,X1,very_cold),X2) :- 
        happensAt(internal_temperature_change(X3,X1,normal),X2).

You can comment-out unnecessary mode declarations (or add extra ones) to experiment
with the quality of the output hypothesis. To comment-out a mode declariation, preceed
it with %

