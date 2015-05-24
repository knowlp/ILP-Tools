This folder contains background knowledge file and mode declarations
for the DrivingStyle high-level event. To experiment with learning DrivingStyle
copy the contents of bk.lp and modes.pl in the corresponding files in the 
/knowledge folder. Also, modify the core.py file in the source, so that the
example_patterns list is as follows:

example_patterns = [#'holdsAt(passenger_satisfaction(X,Y,Z),T)',
                    #'holdsAt(driving_quality(X,Y,Z),T)',
                    'holdsAt(driving_style(X,Y,Z),T)',
                    #'holdsAt(punctuality(X,Y,Z),T)',
                    #'holdsAt(sharp_turn(X,Y,Z),T)',
                    #'holdsAt(abrupt_acceleration(X,Y,Z),T)',
                    #'holdsAt(abrupt_deceleration(X,Y,Z),T)',
                    #'holdsAt(passenger_density(X,Y,Z),T)',
                    #'holdsAt(noise_level(X,Y,Z),T)',
                    #'holdsAt(internal_temperature(X,Y,Z),T)',
                    ] 
(so comment out all other high-level events). To run the application and learn
a definition for this high-level event run main.py from the source folder. The 
output hypothesis is written in the theory.lp file in the root folder. The
DrivingStyle high-level event is not "flat", meaning that it does not depends on
low-level events only, but also on other high-level events. To make learning 
tractable, the background knowledge file contains definitions of some high-level
events, i.e. we utilize some "hierarchical bias" in this case, assuming knowledge
about the domain (see ILED's reference paper for details). It is possible to try to
learn a definition for DrivingStyle assuming no such knowledge at all. To this end,
remove all code included within "Extra Background Knowledge start/end" in the bk.lp
file. Also, you need to modify the mode declarations file (modes.pl) so that all 
available low/high-level events are allowed in the bodies of the hypothesized rules. 


 The target
(optimal) definition for the SharpTurn HLE is:

terminatedAt(driving_style(X3,X1,unsafe),X2) :- 
        terminatedAt(sharp_turn(X3,X1,very_sharp),X2),
        not holdsAt(abrupt_deceleration(X3,X1,very_abrupt),X2).
initiatedAt(driving_style(X3,X1,unsafe),X2) :- 
        initiatedAt(abrupt_deceleration(X3,X1,very_abrupt),X2).
initiatedAt(driving_style(X3,X1,unsafe),X2) :- 
        initiatedAt(sharp_turn(X3,X1,very_sharp),X2).
terminatedAt(driving_style(X3,X1,unsafe),X2) :- 
        terminatedAt(abrupt_acceleration(X3,X1,very_abrupt),X2),
        not holdsAt(abrupt_deceleration(X3,X1,very_abrupt),X2),
        not holdsAt(sharp_turn(X3,X1,very_sharp),X2).
initiatedAt(driving_style(X2,X1,unsafe),X3) :- 
        initiatedAt(abrupt_acceleration(X2,X1,very_abrupt),X3).
terminatedAt(driving_style(X2,X1,unsafe),X3) :- 
        terminatedAt(abrupt_deceleration(X2,X1,very_abrupt),X3),
        not holdsAt(abrupt_acceleration(X2,X1,very_abrupt),X3),
        not holdsAt(sharp_turn(X2,X1,very_sharp),X3).
terminatedAt(driving_style(X3,X1,unsafe),X2) :- 
        terminatedAt(sharp_turn(X3,X1,very_sharp),X2),
        holdsAt(abrupt_acceleration(X3,X1,abrupt),X2),
        not holdsAt(abrupt_deceleration(X3,X1,very_abrupt),X2).
terminatedAt(driving_style(X3,X1,unsafe),X2) :- 
        terminatedAt(sharp_turn(X3,X1,sharp),X2),
        terminatedAt(abrupt_acceleration(X3,X1,very_abrupt),X2),
        not holdsAt(abrupt_deceleration(X3,X1,very_abrupt),X2).

You can comment-out unnecessary mode declarations (or add extra ones) to experiment
with the quality of the output hypothesis. To comment-out a mode declariation, preceed
it with %

