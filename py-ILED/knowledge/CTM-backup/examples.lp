%% positives:

example(holdsAt(driving_style(id75,bus,unsafe),5)).
example(holdsAt(sharp_turn(id75,bus,very_sharp),5)).
example(holdsAt(driving_style(id75,bus,unsafe),6)).
example(holdsAt(punctuality(id75,bus,punctual),6)).
example(holdsAt(sharp_turn(id75,bus,very_sharp),6)).
example(holdsAt(abrupt_acceleration(id75,bus,very_abrupt),6)).
example(holdsAt(abrupt_deceleration(id75,bus,abrupt),6)).
example(holdsAt(driving_style(id75,bus,unsafe),7)).
example(holdsAt(punctuality(id75,bus,punctual),7)).
example(holdsAt(abrupt_deceleration(id75,bus,abrupt),7)).
example(holdsAt(driving_style(id75,bus,unsafe),8)).
example(holdsAt(punctuality(id75,bus,punctual),8)).
example(holdsAt(driving_style(id75,bus,unsafe),9)).
example(holdsAt(punctuality(id75,bus,punctual),9)).
example(holdsAt(passenger_satisfaction(id75,bus,reducing),10)).
example(holdsAt(driving_style(id75,bus,unsafe),10)).
example(holdsAt(punctuality(id75,bus,punctual),10)).
example(holdsAt(internal_temperature(id75,bus,very_cold),10)).

%% narrative:

happensAt(passenger_density_change(id75,bus,low),3).
happensAt(sharp_turn_start(id75,bus,very_sharp),4).
happensAt(stop_enter(id75,bus,"008",early),5).
happensAt(abrupt_acceleration_start(id75,bus,very_abrupt),5).
happensAt(abrupt_deceleration_start(id75,bus,abrupt),5).
happensAt(abrupt_acceleration_end(id75,bus,very_abrupt),6).
happensAt(sharp_turn_end(id75,bus,very_sharp),6).
happensAt(stop_leave(id75,bus,"008",scheduled),7).
happensAt(abrupt_deceleration_end(id75,bus,abrupt),7).
happensAt(internal_temperature_change(id75,bus,very_cold),9).
happensAt(noise_level_change(id75,bus,low),9).
starttime(0).

