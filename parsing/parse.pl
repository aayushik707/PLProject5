
%%%%%%%%%%%%%%%%%
% Your code here:
%%%%%%%%%%%%%%%%%

% Main predicate: parse/1 checks if the input matches the grammar.
parse(X) :- lines(X, []).

% Lines → Line ; Lines | Line
lines(Input, Rest) :-
    line(Input, [; | Mid]), % Line followed by ';'
    lines(Mid, Rest).       % Recursively process the remaining Lines.
lines(Input, Rest) :-
    line(Input, Rest).      % A single Line without ';'.

% Line → Num , Line | Num
line(Input, Rest) :-
    num(Input, [',' | Mid]), % Num followed by ','
    line(Mid, Rest).         % Recursively process the next Line.
line(Input, Rest) :-
    num(Input, Rest).        % A single Num.

% Num → Digit | Digit Num
num([Digit | Rest], Rest) :-
    digit(Digit).           % Single Digit.
num([Digit | Tail], Rest) :-
    digit(Digit),
    num(Tail, Rest).        % Digit followed by another Num.

% Digit → 0 | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9
digit('0').
digit('1').
digit('2').
digit('3').
digit('4').
digit('5').
digit('6').
digit('7').
digit('8').
digit('9').

% Example execution:
% ?- parse(['3', '2', ',', '0', ';', '1', ',', '5', '6', '7', ';', '2']).
% true.
% ?- parse(['3', '2', ',', '0', ';', '1', ',', '5', '6', '7', ';', '2', ',']).
% false.
% ?- parse(['3', '2', ',', ';', '0']).
% false.
