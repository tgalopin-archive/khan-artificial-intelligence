
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Helpers
% 
% Prédicats utiles pour la suite
%

% Avoir un element à un indice spécifique dans une liste
% getElementByIndex(Index, List, Element)
getElementByIndex(0, [L|Q], L) :- !.
getElementByIndex(N, [_|Q], L) :- M is N-1, getElementByIndex(M, Q, L).

% Remplacer l'élément d'index I dans la liste T par l'élément X et retourne le resultat dans [X|T]
% Remarque : les index ici vont de 0 à n-1 pour une liste de taille n, c'est à dire que le premier élément est à la position 0
replace([_|T], 0, X, [X|T]) :- !. % modif
replace([H|T], I, X, [H|R]):-   I > -1,
                                NI is I-1,
                                replace(T, NI, X, R), !.
replace(L, _, _, L).

% Remplacer l'élément O de la liste "Liste" par l'élément R et enregistrer la nouvelle liste dans "ListeResultat" :
%   replaceElement(O, R, Liste, ListeResulat)
replaceElement(_, _, [], []).
replaceElement(O, R, [O|T], [R|T2]) :- replaceElement(O, R, T, T2), !.
replaceElement(O, R, [H|T], [H|T2]) :- H \= O, replaceElement(O, R, T, T2).

% Retirer UN element X d'une liste
delOneOccurrence(X, [], []).
delOneOccurrence(X, [X|Q], Q) :- !. % pour éviter le backtraking et vérifier seulement la première occurrence
delOneOccurrence(X, [T|Q], [T|R]) :- X \= T, delOneOccurrence(X,Q,R).

% Retirer TOUS les elements X d'une liste
delAllOccurrences(X, [], []).
delAllOccurrences(X, [X|Q], R) :- delAllOccurrences(X,Q,R), !.
delAllOccurrences(X, [T|Q], [T|R]) :- X \= T, delAllOccurrences(X,Q,R).

% remove duplication
filtrate([],[]).
filtrate([H|T], WithoutDup) :- member(H,T), filtrate(T,WithoutDup),!.
filtrate([H|T], [H|WithoutDup]) :- \+ member(H,T),filtrate(T,WithoutDup).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Etape 1
% 
% Initialisation du plateau et affichage
%

% Initialisation des différents affichage du plateau de jeu
board(1, [ [1,2,2,3,1,2], [3,1,3,1,3,2], [2,3,1,2,1,3], [2,1,3,2,3,1], [1,3,1,3,1,2], [3,2,2,1,3,2] ]).
board(2, [ [2,3,1,2,2,3], [2,1,3,1,3,1], [1,3,2,3,1,2], [3,1,2,1,3,2], [2,3,1,3,1,3], [2,1,3,2,2,1] ]).
board(3, [ [3,1,2,2,3,1], [2,3,1,3,1,2], [2,1,3,1,3,2], [1,3,2,2,1,3], [3,1,3,1,3,1], [2,2,1,3,2,2] ]).
board(4, [ [2,2,3,1,2,2], [1,3,1,3,1,3], [3,1,2,2,3,1], [2,3,1,3,1,2], [2,1,3,1,3,2], [1,3,2,2,1,3] ]).

% Liste des pièces possibles
pieces(j1,[sr1,sr2,sr3,sr4,sr5,kar]).
pieces(j2,[so1,so2,so3,so4,so5,kao]).

% renvoie vrai si P est une pièce de j1 ou j2.
isPiece(j1,P) :- pieces(j1,A), member(P, A), !.
isPiece(j2,P) :- pieces(j2,B), member(P, B), !.

% Aficher le plateau de jeu
displayPiece([SquareValue|[Piece|_]]) :- write(SquareValue), write('|'), write(Piece), !.
displayPiece(SquareValue) :- write('  '), write(SquareValue), write('  '), !.

displayRow([]) :- !.
displayRow([L|[]]) :-
        write(' | '),
        displayPiece(L),
        write(' | '), !.
displayRow([L|Q]) :-
        write(' | '),
        displayPiece(L),
        displayRow(Q).

displayRows([], _):-!.
displayRows([L|[]], Coord) :-
        PrevCoord is Coord - 1,
        DisplayCoord is 6 - Coord,
        write('     -------------------------------------------------'), nl,
        write(' '), write(DisplayCoord), write('  '), displayRow(L), nl,
        write('     -------------------------------------------------'), !.
displayRows([L|Q], Coord) :-
        PrevCoord is Coord - 1,
        DisplayCoord is 6 - Coord,
        write('     -------------------------------------------------'), nl,
        write(' '), write(DisplayCoord), write('  '), displayRow(L), nl,
        displayRows(Q, PrevCoord).

displayXCoord(0) :- !.
displayXCoord(Coord) :-
        PrevCoord is Coord - 1,
        displayXCoord(PrevCoord),
        write(PrevCoord),
        write('       ').

displayBoard(Board) :-
        length(Board, ColumnSize),
        getElementByIndex(0, Board, Line),
        length(Line, LineSize),
        write('Y▼ X ▶   '),
        displayXCoord(LineSize), nl,
        displayRows(Board, ColumnSize).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Vérifier si une case est occupée
squareTakenOnLine(Y, Line) :-
        getElementByIndex(Y, Line, Square), % Square = case Y sur la ligne L
        length(Square,2). % vérifier que la case a une taille de 2, ??ce qui signifie qu'elle est occopée??

squareTaken([X,Y],Board) :-
        getElementByIndex(X, Board, Line), % unifier Ligne avec la ligne d'indice X de Board
        squareTakenOnLine(Y,Line). % appeler le predicat squareTakenOnLine qui s'efface si la case du plateau est occupée par une pièce

% Ajouter une pièce sur une case
% Remplacer l'ancienne piece si la case est occupée
buildNewSquare(OldSquare, NewPiece, NewSquare, OldPiece) :-
        length(OldSquare,2), % si ce test est vrai, alors on est dans le cas où il y a déjà une pièce qu'il faut remplacer
        getElementByIndex(0,OldSquare,OldSquareA), % récupérer la valeur de la case dans OldSquareA
        getElementByIndex(1,OldSquare,OldPiece), % récupérer la valeur de l'ancienne pièce dans OldPiece
        append([OldSquareA],[NewPiece],NewSquare),!. % NewSquare = [valeur_de_la_case, nouvelle_piece]

% Ajouter la pièce directement si la case est libre
buildNewSquare(OldSquare, NewPiece, NewSquare, _) :-
        append([OldSquare],[NewPiece],NewSquare).

% Faire bouger une pièce
% Penser à supprimer la pièce de son ancienne case
movePiece(PieceMoved, X, Y, Board, NewBoard, OldPiece) :-
        getElementByIndex(X, Board, Line), % Ligne = Ligne d'indice X
        getElementByIndex(Y, Line, Square), % Square = case d'indice Y dans Ligne
        buildNewSquare(Square, PieceMoved, NewSquare, OldPiece), % NewSquare = case Square avec l'ajout ou le remplacement de la pièce PieceMoved
        replace(Line, Y, NewSquare, NewLine), % NewLine = Line avec le remplacement de la case d'indice Y par NewSquare
        replace(Board, X, NewLine, NewBoard), !. % NewBoard = Board avec le remplacement de la ligne d'indice X par NewLine

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Demander où placer les pièces
askPieces([], Board, Board) :- !.
askPieces([Piece|Q], Board, NewBoard) :-
        write('Coordonnées de la pièce '),
        write(Piece), % afficher la pièce à insérer
        write(' : '),
        nl,
        read(X),
        read(Y),
        % Utilisation d'une condition if else : "->" action_si_vrai, ";" action_si_faux
        (squareTaken([X,Y], Board)
            ->  write('Case déjà prise.'), askPieces([Piece|Q], Board, NewBoard) % la case est déjà prise, le notifier au joueur et lui redemmander où placer ses pièces
            ;   movePiece(Piece, X, Y, Board, TmpBoard, OldPiece), % si la case est libre, alors bouger la pièce sur la case demandée par le joueur
                nl,
                displayBoard(TmpBoard), % afficher le nouveau plateau temporaire avec la pièce nouvellement insérée
                askPieces(Q, TmpBoard, NewBoard) % demander à placer le reste des pièces
        ).

% Demander à un joueur de placer ses pièces
askPlayerPiecesSetUp(Player, Board, NewBoard) :-
        pieces(Player, PlayerPieces), % PlayerPieces = liste des pièces du joueur Player
        write(PlayerPieces), % afficher la liste des pièces du joueur
        askPieces(PlayerPieces, Board, NewBoard). % lui demander où les placer sur le plateau de jeu

% Initialiser le plateau de jeu
initBoard(Board) :-
        write('Position initiale :'),
        nl,
        board(2, InitialBoard), % position au Sud par défaut
        displayBoard(InitialBoard), % afficher le plateau
        nl, nl,
        write('Quelle position souhaitez-vous avoir ? (1 : Nord, 2 : Sud(actuel), 3 : Ouest, 4 : Est):'),
        nl,
        read(WantedPosition), 
        board(WantedPosition, WantedBoard), % Initialiser WantedBoard avec la disposition demandée
        displayBoard(WantedBoard), % Afficher le plateau avec la disposition demandée
        nl,
        write('Joueur1 ->'),
        nl,
        askPlayerPiecesSetUp(j1, WantedBoard, TmpBoard), % demander au Joueur1 de placer ses pièces et enregistrer le résulat dans TmpBoard
        nl,
        write('Joueur2 ->'), % demander au Joueur2 de placer ses pièces
        nl,
        askPlayerPiecesSetUp(j2, TmpBoard, Board). % demander au Joueur2 de placer ses pièces et enregistrer le résulat dans Board

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



%Extract and delete pieces from board
%removePieceFromBoard(OldBoard, PieceToExctract, NewBoard)
removePieceFromBoard([],_,[]) :-
        fail, !.

removePieceFromBoard([OldLine|Q], Piece, [NewLine|Q]) :-
        member([Num,Piece], OldLine), % si la pièce est sur la ligne, c'est à dire qu'il y a une case égale à [Num,Piece] (où Num s'unifie avec la valeur de la case)
        !, % permet d'empêcher le backtracking sur les instructions qui suivent le !
        % chaque pièce est unique, donc replaceElement suffit : on remplace [Num,Piece] par Num, exemple [1,sr1] sera remplacé par 1 si Piece = sr1
        replaceElement([Num,Piece], Num, OldLine, NewLine). % NewLine prend donc la valeur de OldLine sans la pièce qu'on voulait enlever

% Inutile je pense
%% removePieceFromBoard([L|Ls], Piece, [SubListsWithout|Ls]):-
%%         \+ member(Piece,L), % si 
%%         removePieceFromBoard(L,Piece,SubListsWithout).

removePieceFromBoard([OldLine|Q], Piece, [OldLine|R]) :- 
        \+ member([Num,Piece], OldLine), % si la pièce à enlever n'est pas dans la ligne OldLine du plateau 
        removePieceFromBoard(Q, Piece, R), !. % alors aller chercher dans les lignes suivantes du plateau




%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Etape 2
% 
% Coups possibles
%

%TODO ckeck Kalista
selectMove(Player, X, Y, Board, NewBoard, OldPiece) :-
        nl,
        write('Quelle pièce voulez-vous jouer (sr1, so1, ...) ?'),
        read(Piece),
        (\+ isPiece(Player,Piece) % si la pièce n'est pas une des pièces du joueur
            ->  write('Pièce invalide'),
                selectMove(Player, X, Y, Board, NewBoard, OldPiece)
            ;   % displayBoard(Board),
                write('Deplacement de la pièce '),
                write(Piece),
                write(' : '),
                nl,
                write('X : '), read(X),
                write('Y : '), read(Y),
                removePieceFromBoard(Board, Piece,TmpBoard), % enlever la pièce du plateau de jeu
                movePiece(Piece, X, Y, TmpBoard, NewBoard, OldPiece) % placer la pièce sur sa nouvelle position
        ).


% TODO Vérifier que ca bouffe pas une pièce de son propre camp
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
moveUp(OldPos,[ActualX, ActualY], [NewX,NewY], OldActualPos) :- 
        NewX is ActualX - 1,
        NewY is ActualY,
        \+ member([NewX, NewY], OldPos),
        append(OldPos, [[ActualX,ActualY]], OldActualPos).

moveDown(OldPos,[ActualX, ActualY], [NewX,NewY], OldActualPos) :- 
        NewX is ActualX + 1,
        NewY is ActualY,
        \+ member([NewX, NewY], OldPos),
        append(OldPos, [[ActualX,ActualY]], OldActualPos).

moveLeft(OldPos,[ActualX, ActualY], [NewX,NewY], OldActualPos) :- 
        NewX is ActualX,
        NewY is ActualY - 1,
        \+ member([NewX, NewY], OldPos),
        append(OldPos, [[ActualX,ActualY]], OldActualPos).

moveRight(OldPos,[ActualX, ActualY], [NewX,NewY], OldActualPos) :- 
        NewX is ActualX,
        NewY is ActualY + 1,
        \+ member([NewX, NewY], OldPos),
        append(OldPos, [[ActualX,ActualY]], OldActualPos).


place(I, Board, OldPos,ActualPos, NewPos, AllOldOldPos) :-
        I1 is I-1,
        I1 = 0,
        moveUp(OldPos,ActualPos, NewPos, AllOldOldPos),
        allowedPlace(NewPos).

place(I, Board, OldPos,ActualPos, NewPos, AllOldOldPos) :- 
        I1 is I-1,
        I1 = 0,
        moveDown(OldPos,ActualPos, NewPos, AllOldOldPos),
        allowedPlace(NewPos).

place(I, Board, OldPos,ActualPos, NewPos, AllOldOldPos) :- 
        I1 is I-1,
        I1 = 0,
        moveLeft(OldPos,ActualPos, NewPos, AllOldOldPos),
        allowedPlace(NewPos).

place(I,  Board,OldPos,ActualPos, NewPos, AllOldOldPos) :-
        I1 is I-1,
        I1 = 0,
        moveRight(OldPos,ActualPos, NewPos, AllOldOldPos),
        allowedPlace(NewPos), !.

% place(I, OldPos,ActualPos, NewPos, AllOldOldPos):- I1 is I-1, I1 > 0, !.

place(I, Board, OldPos,ActualPos, NewPos, AllOldOldPos):- 
            moveUp(OldPos,ActualPos, NewActualPos, AllOldPos),
            allowedPlace(NewActualPos),
            \+ squareTaken(NewActualPos, Board),
            I1 is I-1, 
            place(I1,Board, AllOldPos,NewActualPos, NewPos, AllOldOldPos).
place(I, Board, OldPos,ActualPos, NewPos, AllOldOldPos):- 
            moveDown(OldPos,ActualPos, NewActualPos, AllOldPos),
            allowedPlace(NewActualPos),
            \+ squareTaken(NewActualPos, Board),
            I1 is I-1,
            place(I1,Board, AllOldPos,NewActualPos, NewPos, AllOldOldPos).
place(I, Board, OldPos,ActualPos, NewPos, AllOldOldPos):- 
            moveLeft(OldPos,ActualPos, NewActualPos, AllOldPos),
            allowedPlace(NewActualPos),
            \+ squareTaken(NewActualPos, Board),
            I1 is I-1,
            place(I1,Board, AllOldPos,NewActualPos, NewPos,AllOldOldPos).
place(I, Board,OldPos,ActualPos, NewPos, AllOldOldPos):- 
            moveRight(OldPos,ActualPos, NewActualPos, AllOldPos),
            allowedPlace(NewActualPos),
            \+ squareTaken(NewActualPos, Board),
            I1 is I-1,
            place(I1,Board, AllOldPos,NewActualPos, NewPos, AllOldOldPos).

% get the pieces on the board at the moment, with the value of the box and the coordinates
getPiecesOnLine(Player,[],[],_,_):-!.
getPiecesOnLine(Player,[H|T],[L|PList],X,Y):- Y1 is Y+1, squareTakenOnLine(0,[H|T]),
    getElementByIndex(1,H,TL), isPiece(Player,TL), % only piece
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

allowedPlace([X,Y]):- X > -1, Y > -1, X < 6, Y < 6.

allowedMove([Piece, I, X,Y],[X1,Y1]):- moveUp([Piece, 1, X,Y], [X1,Y1]), allowedPlace([X1,Y1]).
allowedMove([Piece, I, X,Y],[X1,Y1]):- moveDown([Piece, 1, X,Y], [X1,Y1]), allowedPlace([X1,Y1]).
allowedMove([Piece, I, X,Y],[X1,Y1]):- moveLeft([Piece, 1, X,Y], [X1,Y1]), allowedPlace([X1,Y1]).
allowedMove([Piece, I, X,Y],[X1,Y1]):- moveRight([Piece, 1, X,Y], [X1,Y1]), allowedPlace([X1,Y1]).


% Idées:
% place([T|[]]) :- write(T), write(' = (X,Y)'), nl, read(X), read(Y), move(T,X,Y,).
% place([T|Q]) :- write(T), write(' = (X,Y)'), nl, read(X), read(Y), insertBoard(T,X,Y), place(Q).
% initBoard(_) :- board(X), place(X,Y), show_board(Y).




%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Etape 4
% 
% Boucle de jeu
%

testBoard([[3,1,2,2,3,1], [[2,sr5], 3,[1,sr1],[3,sr2],[1,sr3],[2,sr4]], [[2,kar],[1,so1],[3,so2],[1,so3],[3,so4],[2,so5]],[1,3,2,2,1,3], [3,[1,kao],3,1,3,1],  [2,2,1,3,2,2]]).

main(_) :-
    testBoard(B),
    % initBoard(B),
    displayBoard(B), nl,
    write('Joueur1 ->'),
    selectMove(j1, X, Y, B, NB1, OldPiece1),!, nl,
    displayBoard(NB1), nl,
    write('Joueur2 ->'),
    selectMove(j2, W, Z, NB1, NB2, OldPiece2),
    displayBoard(NB2).

test(_):- initBoard(B), displayBoard(B). % fonctionnne pas
