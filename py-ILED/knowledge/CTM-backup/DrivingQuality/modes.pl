:- op(500,fy,#).
:- op(500,fy,*).



modeh(*,initiatedAt(driving_quality(+vehicleId,+vehicleType,#fv_driving_quality),+ttime)).  
modeh(*,terminatedAt(driving_quality(+vehicleId,+vehicleType,#fv_driving_quality),+ttime)).






modeb(*,initiatedAt(punctuality(+vehicleId,+vehicleType,#fv_punctuality),+ttime)).
modeb(*,terminatedAt(punctuality(+vehicleId,+vehicleType,#fv_punctuality),+ttime)).
modeb(*,not_initiatedAt(punctuality(+vehicleId,+vehicleType,#fv_punctuality),+ttime)).
modeb(*,not_terminatedAt(punctuality(+vehicleId,+vehicleType,#fv_punctuality),+ttime)).

modeb(*,initiatedAt(driving_style(+vehicleId,+vehicleType,#fv_driving_style),+ttime)).
modeb(*,terminatedAt(driving_style(+vehicleId,+vehicleType,#fv_driving_style),+ttime)).
modeb(*,not_initiatedAt(driving_style(+vehicleId,+vehicleType,#fv_driving_style),+ttime)).
modeb(*,not_terminatedAt(driving_style(+vehicleId,+vehicleType,#fv_driving_style),+ttime)).

modeb(*,holdsAt(driving_style(+vehicleId,+vehicleType,#fv_driving_style),+ttime)).
modeb(*,holdsAt(punctuality(+vehicleId,+vehicleType,#fv_punctuality),+ttime)).

%modeb(*,not_holdsAt(driving_style(+vehicleId,+vehicleType,#fv_driving_style),+ttime)).
%modeb(*,not_holdsAt(punctuality(+vehicleId,+vehicleType,#fv_punctuality),+ttime)).

