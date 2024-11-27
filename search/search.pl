%%%%%%%%%%%%%%%%%%%%%%%%%%
% Search Implementation
%%%%%%%%%%%%%%%%%%%%%%%%%%

% Initial state includes the starting position and initial key status
initial_state(state(Position, no_red_key, no_blue_key, no_black_key)) :-
    initial(Position).

% Final state is reached when the treasure room is found
final_state(state(Position, _, _, _)) :-
    treasure(Position).

% Movement actions through unlocked doors
move_action(state(From, RedKey, BlueKey, BlackKey), move(From, To), state(To, RedKey, BlueKey, BlackKey)) :-
    door(From, To).
move_action(state(From, RedKey, BlueKey, BlackKey), move(From, To), state(To, RedKey, BlueKey, BlackKey)) :-
    door(To, From).

% Picking up keys
move_action(state(From, no_red_key, BlueKey, BlackKey), move(From, To), state(To, has_red_key, BlueKey, BlackKey)) :-
    door(From, To),
    key(To, red).
move_action(state(From, RedKey, no_blue_key, BlackKey), move(From, To), state(To, RedKey, has_blue_key, BlackKey)) :-
    door(From, To),
    key(To, blue).
move_action(state(From, RedKey, BlueKey, no_black_key), move(From, To), state(To, RedKey, BlueKey, has_black_key)) :-
    door(From, To),
    key(To, black).

% Unlocking and passing through locked doors
move_action(state(From, RedKey, BlueKey, BlackKey), move(From, To), state(To, RedKey, BlueKey, BlackKey)) :-
    locked_door(From, To, blue),
    BlueKey = has_blue_key.
move_action(state(From, RedKey, BlueKey, BlackKey), move(From, To), state(To, RedKey, BlueKey, BlackKey)) :-
    locked_door(To, From, blue),
    BlueKey = has_blue_key.
move_action(state(From, RedKey, BlueKey, BlackKey), move(From, To), state(To, RedKey, BlueKey, BlackKey)) :-
    locked_door(From, To, red),
    RedKey = has_red_key.
move_action(state(From, RedKey, BlueKey, BlackKey), move(From, To), state(To, RedKey, BlueKey, BlackKey)) :-
    locked_door(To, From, red),
    RedKey = has_red_key.
move_action(state(From, RedKey, BlueKey, BlackKey), move(From, To), state(To, RedKey, BlueKey, BlackKey)) :-
    locked_door(From, To, black),
    BlackKey = has_black_key.
move_action(state(From, RedKey, BlueKey, BlackKey), move(From, To), state(To, RedKey, BlueKey, BlackKey)) :-
    locked_door(To, From, black),
    BlackKey = has_black_key.

% Breadth-first search (BFS)
% bfs([[CurrentPath]], FinalState, Actions)
bfs([[state(CurrentPosition, RedKey, BlueKey, BlackKey) | Path] | _], _, Actions) :-
    final_state(state(CurrentPosition, RedKey, BlueKey, BlackKey)), % If the treasure is found
    reverse([state(CurrentPosition, RedKey, BlueKey, BlackKey) | Path], Solution),
    extract_moves(Solution, Actions).

bfs([[State | Path] | Rest], FinalState, Actions) :-
    findall(
        [NextState, State | Path],
        (
            move_action(State, _, NextState),
            \+ member(NextState, [State | Path]) % Avoid revisiting states
        ),
        NewPaths),
    append(Rest, NewPaths, UpdatedQueue), % Add new paths to the BFS queue
    bfs(UpdatedQueue, FinalState, Actions).

% Extract moves from the solution
extract_moves([], []).
extract_moves([_], []). % The first state has no associated move.
extract_moves([state(From, _, _, _) | [state(To, _, _, _) | Rest]], [move(From, To) | Moves]) :-
    extract_moves([state(To, _, _, _) | Rest], Moves).

% Main search predicate
search(Actions) :-
    initial_state(InitialState), % Start with the initial state
    bfs([[InitialState]], _, Actions). % Perform BFS to find the shortest path
