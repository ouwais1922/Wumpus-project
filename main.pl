:- dynamic([hunterAt/2,
            room/2,
            pit/1,
            wumpus/1
           ]).

% retracting all wumpuses and setting a random position for the wumpus

:- random(1,5,X), random(1,5,Y),
   \+hunterAt(r(X,Y), _),
   retractall(wumpus(r(_,_))),assert(wumpus(r(X,Y))).

% Setting all the rooms (adding them to the knowledge base)

:- retractall(room(_,_)),
   forall(member(X-Y, [1-1,1-2,1-3,1-4,2-1,2-2,2-3,2-4,3-1,3-2,3-3,3-4,4-1,4-2,4-3,4-4]),
   assert(room(X,Y))).


% initializing the game: Asking for the postion of the hunter, setting the pits positions, setting the wumpus position, and the gold's position.

start :- setHunterAt(),
         setPitsPos(),
         setWumpusPos(),
         setGoldPos().


% Asking the user to set the hunter's position

setHunterAt() :- (retractall(hunterAt(r(_,_),_)),
                 write('Please enter the X coordinate of the hunter: '), nl,read(X),
                 write('Please enter the Y coordinate of the hunter: '), nl, read(Y),
                 write('Please enter the initial direction of the wumpus'), nl, read(Z),
                 assert(hunterAt(r(X,Y),Z))).


% Setting the positions of the pits with the probabilitw of 0.2 for a pit to be in a room (wkth no hunter, gold, or wumpuses)

setPitsPos() :- retractall(pit(r(_,_))), pitsPos().
pitsPos() :- forall(member(X-Y, [1-1,1-2,1-3,1-4,2-1,2-2,2-3,2-4,3-1,3-2,3-3,3-4,4-1,4-2,4-3,4-4]), pitProb(X,Y)).
pitProb(X,Y):- (maybe(0.2),\+hunterAt(r(X,Y),_),\+wumpus(r(X,Y)) ,assert(pit(r(X,Y)))); true.


% Setting the position of the wumpus

setWumpusPos() :- \+wumpusPos() -> setWumpusPos(); true.

wumpusPos() :- random(1,5,X), random(1,5,Y),
                     \+hunterAt(r(X,Y),_),\+pit(r(X,Y)),
                     retractall(wumpus(r(_,_))),assert(wumpus(r(X,Y))).


% Setting the gold's position

setGoldPos():- \+goldPos() -> setGoldPos(); true.

goldPos() :- random(1,5,X), random(1,5,Y),
               \+hunterAt(r(X,Y),_),\+pit(r(X,Y)),
               retractall(gold(r(_,_))),assert(gold(r(X,Y))).


% Getting the list of all adjacent rooms to a specified room

getAdjacentRooms(r(X,Y),L) :-
    XL is X-1,
    XR is X+1,
    YD is Y-1,
    YU is Y+1,
    append([XL-Y, XR-Y, X-YU, X-YD],[],L).

% Checking whether a room is adjacent to another

adjacentTo(r(X,Y), r(Z,A)) :-  ((abs(X-Z) =:= 1 , Y =:= A); (abs(Y-A) =:= 1 , X =:= Z)).


% Checking whether there is a breeze at specific position. If invoked as breeze(r(X,Y)), it will list all the rooms having a breeze

breeze(r(X,Y)) :- room(X,Y), pit(r(Z,A)),
               adjacentTo(r(X,Y) , r(Z,A)).


% Checking whether there is a stench at specific position. If invoked as stench(r(X,Y)), it will list all the rooms having a stench

stench(r(X,Y)) :- room(X,Y), wumpus(r(Z,A)),
            adjacentTo(r(X,Y) , r(Z,A)).

% Checking whether a room is safe or not

safeRoomCheck(r(X,Y)) :- (room(X,Y),\+pit(r(X,Y)), \+wumpus(r(X,Y)), format('Room at X: ~d, Y:~d is safe\n', [X,Y])); true.

% Checking whether adjacent rooms to the hunter's current position are safe

safe() :- hunterAt(r(X,Y),_), getAdjacentRooms(r(X,Y),L), forall(member(Z-A, L), safeRoomCheck(r(Z,A))).

% If wumpus is not adjacent to a hunter, it is safe

safeWumpus() :- hunterAt(r(X,Y),_), wumpus(r(Z,A)), \+adjacentTo(r(X,Y),r(Z,A)).

wumpusAlive() :-  wumpus(r(_,_)).

hasArrow() :- wumpusAlive().

% Checking walls

wallCheck(r(X,Y)) :- X < 1 ; X > 4; Y < 1 ; Y>4.

% Shoot wumpus without checking the direction of the hunter and whether it faces the wumpus

shootWumpus() :- hunterAt(r(X,Y),_), getAdjacentRooms(r(X,Y),L),
                \+(foreach(member(Z-A, L), \+wumpus(r(Z,A)))),
                 retractall(wumpus(r(_,_))).


% Shhot wumpus while taking into account the direction of the hunter. If the hunter is facing the wumpus, they can kill the wumpus

shootWumpusWithDirection() :-  hunterAt(r(X,Y),D), getAdjacentRooms(r(X,Y),L),
                               \+(foreach(member(Z-A, L), \+wumpus(r(Z,A)))), wumpus(r(F,E)),
                               ((X-F =:= 1, D=w);(X-F =:= -1, D=e);(Y-E =:= 1, D=s);(Y-E =:= -1, D=n)),
                                retractall(wumpus(r(_,_))).


% Grabbing the gold if the hunter is at the gold's position

grabGold():- hunterAt(r(X,Y),_), gold(r(X,Y)), retractall(gold(r(_,_))).

% Turning the hunter to the left

turnLeft() :- hunterAt(r(X,Y),Z),
    retractall(hunterAt(r(X,Y),Z)),
    ((Z = n, assert(hunterAt(r(X,Y),w)));
    (Z = e,  assert(hunterAt(r(X,Y),n)));
    (Z = s, assert(hunterAt(r(X,Y),e)) );
    (Z = w, assert(hunterAt(r(X,Y),s))) ).

% Turning the hunter to the right

turnRight() :- hunterAt(r(X,Y),Z),
    retractall(hunterAt(r(X,Y),Z)),
    ((Z = n, assert(hunterAt(r(X,Y),e)));
    (Z = e,  assert(hunterAt(r(X,Y),s)));
    (Z = s, assert(hunterAt(r(X,Y),w)) );
    (Z = w, assert(hunterAt(r(X,Y),n))) ).