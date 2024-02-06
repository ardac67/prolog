%knowladges

%   deliver_personel(ID, capacity_to_carry, current_job
%   current_location)
%   delivery personel informatinos
delivery_personel(delivery_person1,20,o1,admin_office,12). % currenctly delivering o1 from admin_office to lecture_hall
delivery_personel(delivery_person2,40,empty,cafeteria,16).
delivery_personel(delivery_person3,60,empty,instutition_x,24).

% places(id,name)
% places informations
places(1,admin_office).
places(2,engineering_building).
places(3,cafeteria).
places(4,library).
places(5,social_sciences).
places(6,lecture_hall).
places(7,instutition_y).
placce(8,instutition_x).

%objects(id,weight,location,destination,urgency,delivery_personel_id)
%objects informations
objects(o1,10,admin_office,lecture_hall,high,delivery_person1). % currently delivering by delivery_person1
objects(o2,20,admin_office,instutition_x,low,none).
objects(o3,30,engineering_building,instutition_x,medium,none).
objects(o4,50,cafeteria,instutition_x,low,none).
objects(o5,40,engineering_building,cafeteria,low,none).

%routes
%route(start_place,end_place,time)
%route informations
%reverse and straight routes between two neighbor places
route(admin_office, cafeteria, 4).
route(admin_office, library, 1).
route(admin_office,engineering_building,3).
route(cafeteria, library, 5).
route(library, social_sciences, 2).
route(social_sciences, instutition_x, 8).
route(social_sciences,cafeteria,2).
route(library, instutition_y, 3).
route(library, engineering_building, 5).
route(engineering_building, lecture_hall, 2).
route(lecture_hall, instutition_y, 3).
route(cafeteria, admin_office, 4).
route(engineering_building,admin_office,3).
route(library, admin_office, 1).
route(cafeteria,social_sciences,2).
route(library, cafeteria, 5).
route(social_sciences, library, 2).
route(instutition_x, social_sciences, 8).
route(instutition_y, library, 3).
route(engineering_building, library, 5).
route(lecture_hall, engineering_building, 2).
route(instutition_y, lecture_hall, 3).

%current_delivery
current_delivery(delivery_person1,o1).


%objects(o5,40,engineering_building,cafeteria,low,none).
%is being delivered?
is_being_delivered(Delivery_Personnel, Object_id) :-
    (   current_delivery(Delivery_Personnel, Object_id)
    ->  delivery_personel(Delivery_Personnel, Capacity, Current_job, Location,Work_Hours),
        objects(Object_id, Weight, Obj_Location, Destination, Urgency, DeliveredBy),
        write('Object is being delivered by : '), write(Delivery_Personnel),
        write('\nWith capacity : '), write(Capacity),
        write('\nAt location : '), write(Location),
        write('\nCurrent job : '), write(Current_job),
        write('\nWork Hours : '), write(Work_Hours),
        write('\nObject information : '), write(Object_id),
        write('\nWeight : '), write(Weight),
        write('\nLocation : '), write(Obj_Location),
        write('\nDestination : '), write(Destination),
        write('\nUrgency : '), write(Urgency),
        write('\nDelivered by : '), write(DeliveredBy)
    ; 
        write('Object is not being delivered by anyone'), nl,
        objects(Object_id, Weight, Obj_Location, Destination, _, none),
        list_free_personnel(Weight, Obj_Location, Destination,Current_job)
    ).

%pick_up object if it is not being delivered or some properties is okey with the other delivery personels 
pick_up(Object_id) :-
    is_being_delivered(_, Object_id).

% list free personeles according compared  weight, work_hours and also if it is not working on another job 
list_free_personnel(Weight, Obj_Location, Obj_Destination,_) :-
    delivery_personel(Personnel, Capacity, empty, Location,Work_Hours),
    Capacity >= Weight,
    \+ current_delivery(Personnel, _),
    shortest_path(Location, Obj_Location, _, GetirTime),
    shortest_path(Obj_Location, Obj_Destination, _, GoturTime),
    Time is GetirTime+GoturTime,
    Time =< Work_Hours,
    write('\nFree Personel : '), 
    write(Personnel), 
    write('\nLocation : '), 
    write(Location), 
    write('\nCapacity : '), 
    write(Capacity), 
    write('\nWork Hours : '),
    write(Work_Hours),
    write('\nEstimated Time (Pick Up + Delivery) : '), 
    write(Time), nl,
    fail.
    list_free_personnel(_,_, _,_).

% Finding Path Time with shortest path algorithm
% just finds possible routes and it's values then selects smaller ones
find_paths(Start, Finish, Path) :-
    travel(Start, Finish, [Start], RevPath),
    reverse(RevPath, Path).
travel(Start, Finish, Visited, [Finish | Visited]) :-
    route(Start, Finish, _).
travel(Start, Finish, Visited, Path) :-
    route(Start, Next, _),
    Next \= Finish,
    \+ member(Next, Visited),
    travel(Next, Finish, [Next | Visited], Path).
path_time([_], 0).
path_time([A, B | Rest], Time) :-
    route(A, B, T),
    path_time([B | Rest], RestTime),
    Time is T + RestTime.
shortest_path(Start, Finish, ShortestPath, Time) :-
    findall(Path, find_paths(Start, Finish, Path), Paths),
    maplist(path_time, Paths, Times),
    min_list(Times, Time),
    nth1(Index, Times, Time),
    nth1(Index, Paths, ShortestPath).
travel_time_helper(Start, Finish, Visited, Acc, Time) :-
    route(Start, Next, TimeToNext),
    \+ member(Next, Visited),
    (   Next = Finish
    ->  Time is Acc + TimeToNext 
    ;   travel_time_helper(Next, Finish, [Next|Visited], Acc + TimeToNext, Time)
    ).
travel_time(Start, Finish, Time) :-
    travel_time_helper(Start, Finish, [Start], 0, Time).
