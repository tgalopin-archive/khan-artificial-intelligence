
/*
printHelloWorld :-
  write('Hello world !'),
  nl.
  */
  
% initialisation du tableau  
 board([[2,3,1,2,2,3],[2,1,3,1,3,1],[1,3,2,3,1,2],[3,1,2,1,3,2],[2,3,1,3,1,3],[2,1,3,2,2,1]]).
 
 
 show_row([]).
 show_row([L|[]]) :- write(L).
 show_row([L|Q]) :- write(L), write('|'),show_row(Q).
 show_rows([L|[]]) :- show_row(L). 
show_rows([L|Q]) :-   show_row(L), nl, write('-----------'), nl,show_rows(Q).
show_board(P) :- show_rows(P).

% Avec appel initBoard(Y) on pourra utiliser Y comme la liste du board
initBoard(_) :- board(X), show_board(X).

/*text(1) :-.
text(2) :-.
text(3) :-.

choix_piece():- read(X), .*/
