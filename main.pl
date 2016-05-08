% Ask user
askUser(E, X) :- write(E), nl, read(X).

% Initialize the board
initBoard(1, [ [2,1,3,2,2,1], [2,3,1,3,1,3], [3,1,2,1,3,2], [1,3,2,3,1,2], [2,1,3,1,3,1], [2,3,1,2,2,3] ]).
initBoard(2, [ [2,3,1,2,2,3], [2,1,3,1,3,1], [1,3,2,3,1,2], [3,1,2,1,3,2], [2,3,1,3,1,3], [2,1,3,2,2,1] ]).
initBoard(3, [ [2,3,1,2,2,3], [2,1,3,1,3,1], [1,3,2,3,1,2], [3,1,2,1,3,2], [2,3,1,3,1,3], [2,1,3,2,2,1] ]). % TODO
initBoard(4, [ [2,3,1,2,2,3], [2,1,3,1,3,1], [1,3,2,3,1,2], [3,1,2,1,3,2], [2,3,1,3,1,3], [2,1,3,2,2,1] ]). % TODO

% Rotate the board

% Display the board
displayRow([]).
displayRow([L|[]]) :- write(' | '), write(L), write(' | ').
displayRow([L|Q]) :- write(' | '), write(L), displayRow(Q).
displayRows([L|[]]) :- write(' -------------------------'), nl, displayRow(L), nl, write(' -------------------------'). 
displayRows([L|Q]) :- write(' -------------------------'), nl, displayRow(L), nl, displayRows(Q).

displayBoard(P) :- displayRows(P).

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





/*
printHelloWorld :-
  write('Hello world !'),
  nl.
  */
  
% initialisation du tableau  
 
 
 


% Avec appel initBoard(Y) on pourra utiliser Y comme la liste du board

/*text(1) :-.
text(2) :-.
text(3) :-.

choix_piece():- read(X), .*/
