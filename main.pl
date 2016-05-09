% Get a specific element from a list
ith(0, [L|Q], L) :- !.
ith(N, [_|Q], L) :- M is N-1, ith(M, Q, L).

% Initialize the board
initBoard(1, [ [2,1,3,2,2,1], [2,3,1,3,1,3], [3,1,2,1,3,2], [1,3,2,3,1,2], [2,1,3,1,3,1], [2,3,1,2,2,3] ]).
initBoard(2, [ [2,3,1,2,2,3], [2,1,3,1,3,1], [1,3,2,3,1,2], [3,1,2,1,3,2], [2,3,1,3,1,3], [2,1,3,2,2,1] ]).
initBoard(3, [ [2,3,1,2,2,3], [2,1,3,1,3,1], [1,3,2,3,1,2], [3,1,2,1,3,2], [2,3,1,3,1,3], [2,1,3,2,2,1] ]). % TODO
initBoard(4, [ [2,3,1,2,2,3], [2,1,3,1,3,1], [1,3,2,3,1,2], [3,1,2,1,3,2], [2,3,1,3,1,3], [2,1,3,2,2,1] ]). % TODO

% Possibles pieces
pieces([sr1,sr2,sr3,sr4,sr5,kr,so1,so2,so3,so4,so5,ko]).
isPiece(P) :- pieces(A), member(P, A).

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
addPiece(L2,Pie, L3) :- length(L2,2), ith(0,L2,L2A), append([L2A],Pie,L3),!.

% Add the piece is place free
addPiece(L2, Pie, L3) :- append([L2],Pie,L3),!.

% Move a piece
movePiece(S, P, X, Y, B, NB) :-
	initBoard(S, Z),
	Z = B,
	ith(X, Z, L1),
	ith(Y, L1, L2),
	addPiece(L2, P, L3),
	replace(L1, Y, L3, L4),
	replace(Z, X, L4, NB).
movePiece(S, P, X, Y, B, NB) :-
	ith(X, B, L1),
	ith(Y, L1, L2),
	addPiece(L2, P, L3),
	replace(L1, Y, L3, L4),
	replace(B, X, L4, NB).

% Ask where to put pieces
askInitialPiece(_, [], _, _).
askInitialPiece(S, [L|Q], B, NB) :-
	nl, write('Coordonnées de la pièce '), write(L), write(' : '), nl,
	read(X),
	read(Y),
	movePiece(S, L, X, Y, B, TB),
	nl,
	displayBoard(TB),
	askInitialPiece(S, Q, TB, NB).

askInitialPieces(S, B, NB) :-
	pieces(A),
	askInitialPiece(S, A, B, NB).


% Main process
main(_) :-
	write('Position initiale :'),
	nl,
	initBoard(2, I),
	displayBoard(I),
	nl, nl,
	write('Quelle position souhaitez-vous avoir ? (1 : haut, 2 : bas, 3 : gauche, 4 : droite):'),
	nl,
	read(S),
	initBoard(S, B),
	displayBoard(B),
	askInitialPieces(S, B, NB),
	displayBoard(NB).




% Idées:
% place([T|[]]) :- write(T), write(' = (X,Y)'), nl, read(X), read(Y), move(T,X,Y,).
% place([T|Q]) :- write(T), write(' = (X,Y)'), nl, read(X), read(Y), insertBoard(T,X,Y), place(Q).
% initBoard(_) :- board(X), place(X,Y), show_board(Y).
