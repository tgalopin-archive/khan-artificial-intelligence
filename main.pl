% Ask user
askUser(E, X) :- write(E), nl, read(X).

% Initialize the board
initBoard(1, [ [2,1,3,2,2,1], [2,3,1,3,1,3], [3,1,2,1,3,2], [1,3,2,3,1,2], [2,1,3,1,3,1], [2,3,1,2,2,3] ]).
initBoard(2, [ [2,3,1,2,2,3], [2,1,3,1,3,1], [1,3,2,3,1,2], [3,1,2,1,3,2], [2,3,1,3,1,3], [2,1,3,2,2,1] ]).
initBoard(3, [ [2,3,1,2,2,3], [2,1,3,1,3,1], [1,3,2,3,1,2], [3,1,2,1,3,2], [2,3,1,3,1,3], [2,1,3,2,2,1] ]). % TODO
initBoard(4, [ [2,3,1,2,2,3], [2,1,3,1,3,1], [1,3,2,3,1,2], [3,1,2,1,3,2], [2,3,1,3,1,3], [2,1,3,2,2,1] ]). % TODO

% Possibles pieces
piece(sr1).
piece(sr2).
piece(sr3).
piece(sr4).
piece(sr5).
piece(kr).

% Display the board
displayRow([]).
displayRow([L|[]]) :- write(' | '), write(L), write(' | ').
displayRow([L|Q]) :- write(' | '), write(L), displayRow(Q).
displayRows([L|[]]) :- write(' -------------------------'), nl, displayRow(L), nl, write(' -------------------------'). 
displayRows([L|Q]) :- write(' -------------------------'), nl, displayRow(L), nl, displayRows(Q).

displayBoard(P) :- displayRows(P).

% Replace the element i from the the list T by X and return result in [X|T]
replace([_|T], 0, X, [X|T]).
replace([H|T], I, X, [H|R]):- I > -1, NI is I-1, replace(T, NI, X, R), !.
replace(L, _, _, L).

% Replace the piece is place occupied
addPiece(L2,Pie, L3) :- length(L2,2),nth0(0,L2,L2A), append([L2A],Pie,L3),!.

% Add the piece is place free
addPiece(L2, Pie, L3) :- append([L2],Pie,L3),!.

% Move a piece
movePiece(Pie,X,Y,NP, NP2) :- board(Z), Z = NP, nth0(X, Z, L1), nth0(Y, L1, L2), addPiece(L2, Pie, L3), replace(L1, Y, L3, L4), replace(Z, X, L4, NP2).
movePiece(Pie,X,Y,NP, NP2) :- nth0(X, NP, L1), nth0(Y, L1, L2), addPiece(L2, Pie, L3), replace(L1, Y, L3, L4), replace(NP, X, L4, NP2).

% Main process
main(_) :-
	write('Position initiale :'),
	nl,
	initBoard(2, I),
	displayBoard(I),
	nl, nl,
	askUser('Quelle position souhaitez-vous avoir ? (1 : haut, 2 : bas, 3 : gauche, 4 : droite):', S),
	initBoard(S, X),
	displayBoard(X).




% Idées:
% place([T|[]]) :- write(T), write(' = (X,Y)'), nl, read(X), read(Y), move(T,X,Y,).
% place([T|Q]) :- write(T), write(' = (X,Y)'), nl, read(X), read(Y), insertBoard(T,X,Y), place(Q).
% initBoard(_) :- board(X), place(X,Y), show_board(Y).