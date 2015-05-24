:- op(500,fy,#).
:- op(500,fy,*).

%% I'll have to find a solution for the flvalues representation

% driving_quality: only punctuality, driving style, stop_enter/leave
% or only with LLEs: acceleration, deceleration, turn, stop_enter, stop_leave.
% driving_style: NO: internal_temperature_change,noise_level_change,passenger_density_change,my_stop_enter,my_stop_leave


%modeh(*,initiatedAt(passenger_satisfaction(+vehicleId,+vehicleType,#fv_passenger_satisfaction),+ttime)).  
%modeh(*,terminatedAt(passenger_satisfaction(+vehicleId,+vehicleType,#fv_passenger_satisfaction),+ttime)).

%modeh(*,initiatedAt(driving_quality(+vehicleId,+vehicleType,#fv_driving_quality),+ttime)).  
%modeh(*,terminatedAt(driving_quality(+vehicleId,+vehicleType,#fv_driving_quality),+ttime)).

%modeh(*,initiatedAt(driving_style(+vehicleId,+vehicleType,#fv_driving_style),+ttime)).
%modeh(*,terminatedAt(driving_style(+vehicleId,+vehicleType,#fv_driving_style),+ttime)).

modeh(*,initiatedAt(punctuality(+vehicleId,+vehicleType,#fv_punctuality),+ttime)).
modeh(*,terminatedAt(punctuality(+vehicleId,+vehicleType,#fv_punctuality),+ttime)).
%modeh(*,initiatedAt(sharp_turn(+vehicleId,+vehicleType,#fv_sharp_turn),+ttime)).
%modeh(*,terminatedAt(sharp_turn(+vehicleId,+vehicleType,#fv_sharp_turn),+ttime)).
%modeh(*,initiatedAt(abrupt_acceleration(+vehicleId,+vehicleType,#fv_abrupt_acceleration),+ttime)).
%modeh(*,terminatedAt(abrupt_acceleration(+vehicleId,+vehicleType,#fv_abrupt_acceleration),+ttime)).
%modeh(*,initiatedAt(abrupt_deceleration(+vehicleId,+vehicleType,#fv_abrupt_deceleration),+ttime)).
%modeh(*,terminatedAt(abrupt_deceleration(+vehicleId,+vehicleType,#fv_abrupt_deceleration),+ttime)).
%modeh(*,initiatedAt(passenger_density(+vehicleId,+vehicleType,#fv_passenger_density),+ttime)).
%modeh(*,terminatedAt(passenger_density(+vehicleId,+vehicleType,#fv_passenger_density),+ttime)).
%modeh(*,initiatedAt(noise_level(+vehicleId,+vehicleType,#fv_noise_level),+ttime)).
%modeh(*,terminatedAt(noise_level(+vehicleId,+vehicleType,#fv_noise_level),+ttime)).
%modeh(*,initiatedAt(internal_temperature(+vehicleId,+vehicleType,#fv_internal_temperature),+ttime)).
%modeh(*,terminatedAt(internal_temperature(+vehicleId,+vehicleType,#fv_internal_temperature),+ttime)).


%modeb(*,happensAt(abrupt_acceleration_start(+vehicleId,+vehicleType,#evalue),+ttime)).
%modeb(*,happensAt(abrupt_acceleration_end(+vehicleId,+vehicleType,#evalue),+ttime)).
%modeb(*,happensAt(abrupt_deceleration_start(+vehicleId,+vehicleType,#evalue),+ttime)).
%modeb(*,happensAt(abrupt_deceleration_end(+vehicleId,+vehicleType,#evalue),+ttime)).
%modeb(*,happensAt(internal_temperature_change(+vehicleId,+vehicleType,#evalue),+ttime)).
%modeb(*,happensAt(noise_level_change(+vehicleId,+vehicleType,#evalue),+ttime)).         
%modeb(*,happensAt(passenger_density_change(+vehicleId,+vehicleType,#evalue),+ttime)).    
%modeb(*,happensAt(sharp_turn_start(+vehicleId,+vehicleType,#fv_sharp_turn),+ttime)).
%modeb(*,happensAt(sharp_turn_end(+vehicleId,+vehicleType,#fv_sharp_turn),+ttime)).
modeb(*,happensAt(my_stop_enter(+vehicleId,+vehicleType,#stopId,#evalue),+ttime)).       
modeb(*,happensAt(my_stop_leave(+vehicleId,+vehicleType,#stopId,#evalue),+ttime)).       

%modeb(*,not_happensAt(abrupt_acceleration_start(+vehicleId,+vehicleType,#evalue),+ttime)).
%modeb(*,not_happensAt(abrupt_acceleration_end(+vehicleId,+vehicleType,#evalue),+ttime)).
%modeb(*,not_happensAt(abrupt_deceleration_start(+vehicleId,+vehicleType,#evalue),+ttime)).
%modeb(*,not_happensAt(abrupt_deceleration_end(+vehicleId,+vehicleType,#evalue),+ttime)).
%modeb(*,not_happensAt(internal_temperature_change(+vehicleId,+vehicleType,#evalue),+ttime)).
%modeb(*,not_happensAt(noise_level_change(+vehicleId,+vehicleType,#evalue),+ttime)).
%modeb(*,not_happensAt(passenger_density_change(+vehicleId,+vehicleType,#evalue),+ttime)).
%modeb(*,not_happensAt(sharp_turn_start(+vehicleId,+vehicleType,#evalue),+ttime)).
%modeb(*,not_happensAt(sharp_turn_end(+vehicleId,+vehicleType,#evalue),+ttime)).
modeb(*,not_happensAt(my_stop_enter(+vehicleId,+vehicleType,#stopId,#fv_my_stop_enter),+ttime)).
modeb(*,not_happensAt(my_stop_leave(+vehicleId,+vehicleType,#stopId,#fv_my_stop_leave),+ttime)).


modeb(*,initiatedAt(punctuality(+vehicleId,+vehicleType,#fv_punctuality),+ttime)).
modeb(*,terminatedAt(punctuality(+vehicleId,+vehicleType,#fv_punctuality),+ttime)).
%modeb(*,not_initiatedAt(punctuality(+vehicleId,+vehicleType,#fv_punctuality),+ttime)).
%modeb(*,not_terminatedAt(punctuality(+vehicleId,+vehicleType,#fv_punctuality),+ttime)).
%modeb(*,initiatedAt(sharp_turn(+vehicleId,+vehicleType,#fv_sharp_turn),+ttime)).
%modeb(*,terminatedAt(sharp_turn(+vehicleId,+vehicleType,#fv_sharp_turn),+ttime)).
%modeb(*,initiatedAt(abrupt_acceleration(+vehicleId,+vehicleType,#fv_abrupt_acceleration),+ttime)).
%modeb(*,terminatedAt(abrupt_acceleration(+vehicleId,+vehicleType,#fv_abrupt_acceleration),+ttime)).
%modeb(*,initiatedAt(abrupt_deceleration(+vehicleId,+vehicleType,#fv_abrupt_deceleration),+ttime)).
%modeb(*,terminatedAt(abrupt_deceleration(+vehicleId,+vehicleType,#fv_abrupt_deceleration),+ttime)).

%modeb(*,initiatedAt(passenger_density(+vehicleId,+vehicleType,#fv_passenger_density),+ttime)).
%modeb(*,terminatedAt(passenger_density(+vehicleId,+vehicleType,#fv_passenger_density),+ttime)).
%modeb(*,initiatedAt(noise_level(+vehicleId,+vehicleType,#fv_noise_level),+ttime)).
%modeb(*,terminatedAt(noise_level(+vehicleId,+vehicleType,#fv_noise_level),+ttime)).
%modeb(*,initiatedAt(internal_temperature(+vehicleId,+vehicleType,#fv_internal_temperature),+ttime)).
%modeb(*,terminatedAt(internal_temperature(+vehicleId,+vehicleType,#fv_internal_temperature),+ttime)).

%modeb(*,initiatedAt(driving_style(+vehicleId,+vehicleType,#fv_driving_style),+ttime)).
%modeb(*,terminatedAt(driving_style(+vehicleId,+vehicleType,#fv_driving_style),+ttime)).
%modeb(*,not_initiatedAt(driving_style(+vehicleId,+vehicleType,#fv_driving_style),+ttime)).
%modeb(*,not_terminatedAt(driving_style(+vehicleId,+vehicleType,#fv_driving_style),+ttime)).
%modeb(*,initiatedAt(driving_quality(+vehicleId,+vehicleType,#fv_driving_style),+ttime)).
%modeb(*,terminatedAt(driving_quality(+vehicleId,+vehicleType,#fv_driving_style),+ttime)).



%modeb(*,holdsAt(sharp_turn(+vehicleId,+vehicleType,#fv_sharp_turn),+ttime)).
%modeb(*,holdsAt(abrupt_acceleration(+vehicleId,+vehicleType,#fv_abrupt_acceleration),+ttime)).
%modeb(*,holdsAt(abrupt_deceleration(+vehicleId,+vehicleType,#fv_abrupt_deceleration),+ttime)).
%modeb(*,holdsAt(passenger_density(+vehicleId,+vehicleType,#fv_passenger_density),+ttime)).
%modeb(*,holdsAt(noise_level(+vehicleId,+vehicleType,#fv_noise_level),+ttime)).
%modeb(*,holdsAt(internal_temperature(+vehicleId,+vehicleType,#fv_internal_temperature),+ttime)).

%modeb(*,holdsAt(driving_quality(+vehicleId,+vehicleType,#fv_driving_quality),+ttime)).
%modeb(*,holdsAt(driving_style(+vehicleId,+vehicleType,#fv_driving_style),+ttime)).
%modeb(*,holdsAt(punctuality(+vehicleId,+vehicleType,#fv_punctuality),+ttime)).

%modeb(*,not_holdsAt(sharp_turn(+vehicleId,+vehicleType,#fv_sharp_turn),+ttime)).
%modeb(*,not_holdsAt(abrupt_acceleration(+vehicleId,+vehicleType,#fv_abrupt_acceleration),+ttime)).
%modeb(*,not_holdsAt(abrupt_deceleration(+vehicleId,+vehicleType,#fv_abrupt_deceleration),+ttime)).
%modeb(*,not_holdsAt(passenger_density(+vehicleId,+vehicleType,#fv_passenger_density),+ttime)).
%modeb(*,not_holdsAt(noise_level(+vehicleId,+vehicleType,#fv_noise_level),+ttime)).
%modeb(*,not_holdsAt(internal_temperature(+vehicleId,+vehicleType,#fv_internal_temperature),+ttime)).

%modeb(*,holdsAt(passenger_satisfaction(+vehicleId,+vehicleType,#fv_passenger_satisfaction),+ttime)). %% this is top-level, it does not appear in any body



%modeb(*,not_holdsAt(driving_quality(+vehicleId,+vehicleType,#fv_driving_quality),+ttime)).
%modeb(*,not_holdsAt(driving_style(+vehicleId,+vehicleType,#fv_driving_style),+ttime)).
%modeb(*,not_holdsAt(punctuality(+vehicleId,+vehicleType,#fv_punctuality),+ttime)).

