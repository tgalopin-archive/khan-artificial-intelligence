% to test
testBoard([[3,1,2,2,3,1], [[2,sr5], 3,[1,sr1],[3,sr2],[1,sr3],[2,sr4]], [[2,kr],[1,so1],[3,so2],[1,so3],[3,so4],[2,so5]],[1,3,2,2,1,3], [3,[1,ko],3,1,3,1],  [2,2,1,3,2,2]]).

% Get a specific element from a list
ith(0, [L|Q], L) :- !.
ith(N, [_|Q], L) :- M is N-1, ith(M, Q, L).

% Initialize the board
board(1, [ [1,2,2,3,1,2], [3,1,3,1,3,2], [2,3,1,2,1,3], [2,1,3,2,3,1], [1,3,1,3,1,2], [3,2,2,1,3,2] ]).
board(2, [ [2,3,1,2,2,3], [2,1,3,1,3,1], [1,3,2,3,1,2], [3,1,2,1,3,2], [2,3,1,3,1,3], [2,1,3,2,2,1] ]).
board(3, [ [3,1,2,2,3,1], [2,3,1,3,1,2], [2,1,3,1,3,2], [1,3,2,2,1,3], [3,1,3,1,3,1], [2,2,1,3,2,2] ]).
board(4, [ [2,2,3,1,2,2], [1,3,1,3,1,3], [3,1,2,2,3,1], [2,3,1,3,1,2], [2,1,3,1,3,2], [1,3,2,2,1,3] ]).

% Possibles pieces
pieces(j1,[sr1,sr2,sr3,sr4,sr5,kr]).
pieces(j2,[so1,so2,so3,so4,so5,ko]).
isPiece(j1,P) :- pieces(j1,A), member(P, A), !.
isPiece(j2,P) :- pieces(j2,B), member(P, B), !.

% Display the board
displayRow([]) :- !.
displayRow([L|[]]) :- write(' | '), write(L), write(' | '), !.
displayRow([L|Q]) :- write(' | '), write(L), displayRow(Q).
displayRows([]):-!.
displayRows([L|[]]) :- write(' -------------------------'), nl, displayRow(L), nl, write(' -------------------------'), !.
displayRows([L|Q]) :- write(' -------------------------'), nl, displayRow(L), nl, displayRows(Q).

displayBoard(P) :- displayRows(P).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Replace the element i from the the list T by X and return result in [X|T]
replace([_|T], 0, X, [X|T]).
replace([H|T], I, X, [H|R]):- I > -1, NI is I-1, replace(T, NI, X, R), !.
replace(L, _, _, L).

% Check if case occupied
% occupied([X,Y], B) :- ith(X, B, L1), ith(Y, L1, L2), length(L2,2).
occupied([X,Y],B):- ith(X, B, L1), occupiedOnLine(Y,L1).
occupiedOnLine(Y,L):- ith(Y, L, L2), length(L2,2).


% Replace the piece is place occupied
addPiece(L2,Pie, L3, AP) :- length(L2,2), ith(0,L2,L2A), ith(1,L2,AP), append([L2A],[Pie],L3),!.

% Add the piece is place free
addPiece(L2, Pie, L3, _) :- append([L2],[Pie],L3).

% Move a piece
movePiece(P, X, Y, B, NB, AP) :-
	ith(X, B, L1),
	ith(Y, L1, L2),
	addPiece(L2, P, L3, AP),
	replace(L1, Y, L3, L4),
	replace(B, X, L4, NB), !.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Ask where to put pieces
askInitialPiece([], B, B):-!.
askInitialPiece([L|Q], B, NB) :-
    write(B),
	nl, write('Coordonnées de la pièce '), write(L), write(' : '), nl,
    read(X),
	read(Y),
    (occupied([X,Y], B)
        ->  askInitialPiece([L|Q], B, NB)
        ;   movePiece(L, X, Y, B, TB, AP),
            nl,
            displayBoard(TB),
            askInitialPiece(Q, TB, NB)
    ).

askInitialPieces(Joueur,B, NB) :-
    pieces(Joueur, A), write(A),
	askInitialPiece(A, B, NB).

initBoard(NB2) :-
    write('Position initiale :'),
    nl,
    board(2, I),
    displayBoard(I),
    nl, nl,
    write('Quelle position souhaitez-vous avoir ? (1 : Nord, 2 : Sud(actuel), 3 : Ouest, 4 : Est):'),
    nl,
    read(S),
    board(S, B),
    displayBoard(B),
    nl,
    write('Joueur1 ->'),
    nl,
    askInitialPieces(j1, B, NB1),
    nl,
    write('Joueur2 ->'),
    nl,
    askInitialPieces(j2, NB1, NB2).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Delete an element from a list
del(X,[X|Q], Q) :-!.
del(X,[Y|Q], [Y|NQ]):- del(X,Q,NQ).

%Extract and delete pieces from board
extractDelete([],_,[]) :- fail, !.

extractDelete([L|Ls], P, [SS|Ls]) :-
    member(P,L), !, del(P,L,S), ith(0,S,SS)
    % extractDelete(Ls, P, SubListsWithout)
.
extractDelete([L|Ls], P, [SubListsWithout|Ls]):- \+ member(P,L), extractDelete(L,P,SubListsWithout).
extractDelete([L|Ls], P, [L|SubListsWithout]) :- \+ member(P,L), extractDelete(Ls, P, SubListsWithout), !.

% AP old piece, TO DO ckeck Kalista
selectMove(J, X, Y, B, NB, AP) :-
    nl,
    write('Quelle pièce voulez-vous jouer ?'),
    read(P),
    (\+ isPiece(J,P)
        ->  write('Pièce invalide'),
            selectMove(J, X, Y, B, NB, AP)
        ;   % displayBoard(B),
            write('Deplacement de la pièce '),
            write(P),
            write(' : '),
            nl,
            write('X : '), read(X),
            write('Y : '), read(Y),
            extractDelete(B, P,TB),
            movePiece(P, X, Y, TB, NB, AP)
    ).

% TODO Vérifier que ca bouffe pas une piÈce de son propre camp
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
moveUp(OldPos,[ActualX, ActualY], [NewX,NewY], OldActualPos) :- NewX is ActualX - 1, NewY is ActualY, \+ member([NewX, NewY], OldPos), append(OldPos, [[ActualX,ActualY]], OldActualPos).
moveDown(OldPos,[ActualX, ActualY], [NewX,NewY], OldActualPos) :- NewX is ActualX + 1, NewY is ActualY, \+ member([NewX, NewY], OldPos), append(OldPos, [[ActualX,ActualY]], OldActualPos).
moveLeft(OldPos,[ActualX, ActualY], [NewX,NewY], OldActualPos) :- NewX is ActualX, NewY is ActualY - 1, \+ member([NewX, NewY], OldPos), append(OldPos, [[ActualX,ActualY]], OldActualPos).
moveRight(OldPos,[ActualX, ActualY], [NewX,NewY], OldActualPos) :- NewX is ActualX, NewY is ActualY + 1, \+ member([NewX, NewY], OldPos), append(OldPos, [[ActualX,ActualY]], OldActualPos).

place(I, Board, OldPos,ActualPos, NewPos, AllOldOldPos):- I1 is I-1, I1 = 0, moveUp(OldPos,ActualPos, NewPos, AllOldOldPos), allowedPlace(NewPos).
place(I, Board, OldPos,ActualPos, NewPos, AllOldOldPos):- I1 is I-1, I1 = 0, moveDown(OldPos,ActualPos, NewPos, AllOldOldPos), allowedPlace(NewPos).
place(I, Board, OldPos,ActualPos, NewPos, AllOldOldPos):- I1 is I-1, I1 = 0, moveLeft(OldPos,ActualPos, NewPos, AllOldOldPos), allowedPlace(NewPos).
place(I,  Board,OldPos,ActualPos, NewPos, AllOldOldPos):- I1 is I-1, I1 = 0, moveRight(OldPos,ActualPos, NewPos, AllOldOldPos), allowedPlace(NewPos), !.

% place(I, OldPos,ActualPos, NewPos, AllOldOldPos):- I1 is I-1, I1 > 0, !.

place(I, Board, OldPos,ActualPos, NewPos, AllOldOldPos):- moveUp(OldPos,ActualPos, NewActualPos, AllOldPos),allowedPlace(NewActualPos), \+ occupied(NewActualPos, Board), I1 is I-1,  place(I1,Board, AllOldPos,NewActualPos, NewPos, AllOldOldPos).
place(I, Board, OldPos,ActualPos, NewPos, AllOldOldPos):- moveDown(OldPos,ActualPos, NewActualPos, AllOldPos), allowedPlace(NewActualPos), \+ occupied(NewActualPos, Board),I1 is I-1, place(I1,Board, AllOldPos,NewActualPos, NewPos, AllOldOldPos).
place(I, Board, OldPos,ActualPos, NewPos, AllOldOldPos):- moveLeft(OldPos,ActualPos, NewActualPos, AllOldPos),allowedPlace(NewActualPos), \+ occupied(NewActualPos, Board), I1 is I-1, place(I1,Board, AllOldPos,NewActualPos, NewPos,AllOldOldPos).
place(I, Board,OldPos,ActualPos, NewPos, AllOldOldPos):- moveRight(OldPos,ActualPos, NewActualPos, AllOldPos), allowedPlace(NewActualPos), \+ occupied(NewActualPos, Board), I1 is I-1, place(I1,Board, AllOldPos,NewActualPos, NewPos, AllOldOldPos).


% remove duplication
filtrate([],[]).
filtrate([H|T], WithoutDup) :- member(H,T), filtrate(T,WithoutDup),!.
filtrate([H|T], [H|WithoutDup]) :- \+ member(H,T),filtrate(T,WithoutDup).


% get the pieces on the board at the moment, with the value of the box and the coordinates
getPiecesOnLine(Player,[],[],_,_):-!.
getPiecesOnLine(Player,[H|T],[L|PList],X,Y):- Y1 is Y+1, occupiedOnLine(0,[H|T]),
    ith(1,H,TL), isPiece(Player,TL), % only piece
    append(H,[X,Y],L),
    getPiecesOnLine(Player,T,PList, X,Y1),
    !.
getPiecesOnLine(Player,[H|T],PList, X,Y):- Y1 is Y+1, getPiecesOnLine(Player,T,PList,X,Y1).

getPiecesOnBoard(Player, [], [],_,_):-!.
getPiecesOnBoard(Player, [H|T], BPList,X,Y ):- getPiecesOnBoard(Player,T,LPList,X,Y),X1 is X+1,getPiecesOnLine(Player,H,L,X1,Y), append(L,LPList, BPList).

concatElementList(L,[],[]):-!.
concatElementList([X,Y],[H|T],[[H,X,Y]|L2]):- concatElementList([X,Y],T,L2).

% get all possible moves for each piece
getAllMoves([],Board,[]):-!.
getAllMoves([[Val,Piece, X,Y]|T], Board,[AllMoves1Piece|AllMoves]):-
    findall(L1,place(Val,Board,[],[X,Y],L1,L2),R), filtrate(R,R1),
    concatElementList([Val,Piece],R1,AllMoves1Piece),
    getAllMoves(T,Board,AllMoves).



% get all possible moves for all the pieces
possibleMoves(Player, Board,PossibleMoveList) :-
    getPiecesOnBoard(Player, Board, PList,0,0), % PList = [val, piece, X, Y]
    getAllMoves(PList,Board,PossibleMoveList).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Main process
main(_) :-
    testBoard(B),
    % initBoard(B),
	displayBoard(B),nl,
    write('Joueur1 ->'),
    selectMove(j1, X, Y, B, NB1, AP1),!, nl,
    displayBoard(NB1),
    write('Joueur2 ->'),
    selectMove(j2, W, Z, NB1, NB2, AP2),
    displayBoard(NB2).

test(_):- initBoard(B), displayBoard(B). % fonctionnne pas

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
allowedPlace([X,Y]):- X > -1, Y > -1, X < 6, Y < 6.

allowedMove([Piece, I, X,Y],[X1,Y1]):- moveUp([Piece, 1, X,Y], [X1,Y1]), allowedPlace([X1,Y1]).
allowedMove([Piece, I, X,Y],[X1,Y1]):- moveDown([Piece, 1, X,Y], [X1,Y1]), allowedPlace([X1,Y1]).
allowedMove([Piece, I, X,Y],[X1,Y1]):- moveLeft([Piece, 1, X,Y], [X1,Y1]), allowedPlace([X1,Y1]).
allowedMove([Piece, I, X,Y],[X1,Y1]):- moveRight([Piece, 1, X,Y], [X1,Y1]), allowedPlace([X1,Y1]).


% Idées:
% place([T|[]]) :- write(T), write(' = (X,Y)'), nl, read(X), read(Y), move(T,X,Y,).
% place([T|Q]) :- write(T), write(' = (X,Y)'), nl, read(X), read(Y), insertBoard(T,X,Y), place(Q).
% initBoard(_) :- board(X), place(X,Y), show_board(Y).
