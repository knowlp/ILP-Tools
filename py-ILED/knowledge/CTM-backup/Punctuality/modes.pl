:- op(500,fy,#).
:- op(500,fy,*).


modeh(*,initiatedAt(punctuality(+vehicleId,+vehicleType,#fv_punctuality),+ttime)).
modeh(*,terminatedAt(punctuality(+vehicleId,+vehicleType,#fv_punctuality),+ttime)).


modeb(*,happensAt(abrupt_acceleration_start(+vehicleId,+vehicleType,#evalue),+ttime)).
modeb(*,happensAt(abrupt_acceleration_end(+vehicleId,+vehicleType,#evalue),+ttime)).
modeb(*,happensAt(abrupt_deceleration_start(+vehicleId,+vehicleType,#evalue),+ttime)).
modeb(*,happensAt(abrupt_deceleration_end(+vehicleId,+vehicleType,#evalue),+ttime)).
modeb(*,happensAt(internal_temperature_change(+vehicleId,+vehicleType,#evalue),+ttime)).
modeb(*,happensAt(noise_level_change(+vehicleId,+vehicleType,#evalue),+ttime)).         
modeb(*,happensAt(passenger_density_change(+vehicleId,+vehicleType,#evalue),+ttime)).    
modeb(*,happensAt(sharp_turn_start(+vehicleId,+vehicleType,#fv_sharp_turn),+ttime)).
modeb(*,happensAt(sharp_turn_end(+vehicleId,+vehicleType,#fv_sharp_turn),+ttime)).
modeb(*,happensAt(my_stop_enter(+vehicleId,+vehicleType,#stopId,#evalue),+ttime)).       
modeb(*,happensAt(my_stop_leave(+vehicleId,+vehicleType,#stopId,#evalue),+ttime)).       

modeb(*,not_happensAt(abrupt_acceleration_start(+vehicleId,+vehicleType,#evalue),+ttime)).
modeb(*,not_happensAt(abrupt_acceleration_end(+vehicleId,+vehicleType,#evalue),+ttime)).
modeb(*,not_happensAt(abrupt_deceleration_start(+vehicleId,+vehicleType,#evalue),+ttime)).
modeb(*,not_happensAt(abrupt_deceleration_end(+vehicleId,+vehicleType,#evalue),+ttime)).
modeb(*,not_happensAt(internal_temperature_change(+vehicleId,+vehicleType,#evalue),+ttime)).
modeb(*,not_happensAt(noise_level_change(+vehicleId,+vehicleType,#evalue),+ttime)).
modeb(*,not_happensAt(passenger_density_change(+vehicleId,+vehicleType,#evalue),+ttime)).
modeb(*,not_happensAt(sharp_turn_start(+vehicleId,+vehicleType,#evalue),+ttime)).
modeb(*,not_happensAt(sharp_turn_end(+vehicleId,+vehicleType,#evalue),+ttime)).
modeb(*,not_happensAt(my_stop_enter(+vehicleId,+vehicleType,#stopId,#fv_my_stop_enter),+ttime)).
modeb(*,not_happensAt(my_stop_leave(+vehicleId,+vehicleType,#stopId,#fv_my_stop_leave),+ttime)).



