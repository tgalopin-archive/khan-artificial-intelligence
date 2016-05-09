
/*
printHelloWorld :-
  write('Hello world !'),
  nl.
  */

piece(sr1).
piece(sr2).
piece(sr3).
piece(sr4).
piece(sr5).
piece(kr).
initial([sr1,sr2,sr3,sr4,sr5,kr]).
% test([[2],[3],[4],[5],[6],[7],[8],[9],[1]]).

% remplace le Ie élement de la liste T par X et résultat dasn [X|T]
replace([_|T], 0, X, [X|T]).
replace([H|T], I, X, [H|R]):- I > -1, NI is I-1, replace(T, NI, X, R), !.
replace(L, _, _, L).

% remplace la piece si case occupée
add_piece(L2,Pie, L3) :- length(L2,2),nth0(0,L2,L2A), append([L2A],Pie,L3),!.

% ajoute la piece si case vide
add_piece(L2, Pie, L3) :- append([L2],Pie,L3),!.

board3([1,2]).
  
% initialisation du tableau  
 board([[2,3,1,2,2,3],[2,1,3,1,3,1],[1,3,2,3,1,2],[3,1,2,1,3,2],[2,3,1,3,1,3],[2,1,3,2,2,1]]).

board2([[2,3,1,2,2,3],[2,[1, sr1],[3,sr2],[1,sr3],[3,sr4],1],[1,3,2,3,1,2],[3,1,2,1,3,2],[2,3,1,3,1,3],[2,1,3,2,2,1]]).


show_row([]).
show_row([L|[]]) :- write(L).
show_row([L|Q]) :- write(L), write('|'),show_row(Q).
show_rows([L|[]]) :- show_row(L).
show_rows([L|Q]) :-   show_row(L), nl, write('-----------'), nl,show_rows(Q).
show_board(P) :- show_rows(P).

% déplace un pion
move(Pie,X,Y,NP, NP2) :- board(Z), Z = NP, nth0(X,Z,L1), nth0(Y,L1,L2),add_piece(L2, Pie, L3), replace(L1,Y,L3,L4),replace(Z,X,L4,NP2).
move(Pie,X,Y,NP, NP2) :- nth0(X,NP,L1), nth0(Y,L1,L2),add_piece(L2, Pie, L3), replace(L1,Y,L3,L4),replace(NP,X,L4,NP2).


% place([T|[]]) :- write(T), write(' = (X,Y)'), nl, read(X), read(Y), move(T,X,Y,).
place([T|Q]) :- write(T), write(' = (X,Y)'), nl, read(X), read(Y), insertBoard(T,X,Y), place(Q).

% Avec appel initBoard(Y) on pourra utiliser Y comme la liste du board
initBoard(_) :- board(X), place(X,Y), show_board(Y).

/*text(1) :-.
text(2) :-.
text(3) :-.

choix_piece():- read(X), .*/
