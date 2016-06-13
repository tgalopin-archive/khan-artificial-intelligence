
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Helpers génériques
%

% trouve le minimum dans une liste
min([X], X) :- !.
min([X,Y|Tail], N):-
    ( X > Y ->
        min([Y|Tail], N)
    ;
        min([X|Tail], N)
    ).

% Donne le joueur adverse
changePlayer(j1, j2).
changePlayer(j2, j1).

% declare khan comme un prédicat dynamique
:- dynamic(khan/1).

% Vérifie qu'un élément est vide
empty(T):- length(T,0).
empty([H|T]):- empty(H), empty(T).

% Récupére un element à un indice spécifique dans une liste
% getElementByIndex(Index, List, Element)
getElementByIndex(0, [L|Q], L) :- !.
getElementByIndex(N, [_|Q], L) :- M is N-1, getElementByIndex(M, Q, L).

% Remplace l'élément d'index I dans la liste T par l'élément X et retourne le resultat dans [X|T]
% Remarque : les index ici vont de 0 à n-1 pour une liste de taille n, c'est à dire que le premier élément est à la position 0
replace([_|T], 0, X, [X|T]) :- !.
replace([H|T], I, X, [H|R]):- I > -1, NI is I-1, replace(T, NI, X, R), !.
replace(L, _, _, L).

% Remplace l'élément O de la liste "Liste" par l'élément R et enregistrer la nouvelle liste dans "ListeResultat" :
% replaceElement(O, R, Liste, ListeResulat)
replaceElement(_, _, [], []).
replaceElement(O, R, [O|T], [R|T2]) :- replaceElement(O, R, T, T2), !.
replaceElement(O, R, [H|T], [H|T2]) :- H \= O, replaceElement(O, R, T, T2).

% Retire UN element X d'une liste
delOneOccurrence(X, [], []).
delOneOccurrence(X, [X|Q], Q) :- !. % pour éviter le backtraking et vérifier seulement la première occurrence
delOneOccurrence(X, [T|Q], [T|R]) :- X \= T, delOneOccurrence(X,Q,R).

% Retire TOUS les elements X d'une liste
delAllOccurrences(X, [], []).
delAllOccurrences(X, [X|Q], R) :- delAllOccurrences(X,Q,R), !.
delAllOccurrences(X, [T|Q], [T|R]) :- X \= T, delAllOccurrences(X,Q,R).

% Supprime les doublons
removeDuplicates([],[]).
removeDuplicates([H|T], WithoutDup) :- member(H,T), removeDuplicates(T,WithoutDup),!.
removeDuplicates([H|T], [H|WithoutDup]) :- \+ member(H,T),removeDuplicates(T,WithoutDup).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Etape 1
% 
% Initialisation du jeu
%

% Le khan est un prédicat dynamique
:- dynamic(khan/1).

% Joueurs
player(j1).
player(j2).
oponnent(j1, j2).
oponnent(j2, j1).

% Plateau
board(1, [ [1,2,2,3,1,2], [3,1,3,1,3,2], [2,3,1,2,1,3], [2,1,3,2,3,1], [1,3,1,3,1,2], [3,2,2,1,3,2] ]).
board(2, [ [2,3,1,2,2,3], [2,1,3,1,3,1], [1,3,2,3,1,2], [3,1,2,1,3,2], [2,3,1,3,1,3], [2,1,3,2,2,1] ]).
board(3, [ [3,1,2,2,3,1], [2,3,1,3,1,2], [2,1,3,1,3,2], [1,3,2,2,1,3], [3,1,3,1,3,1], [2,2,1,3,2,2] ]).
board(4, [ [2,2,3,1,2,2], [1,3,1,3,1,3], [3,1,2,2,3,1], [2,3,1,3,1,2], [2,1,3,1,3,2], [1,3,2,2,1,3] ]).

% Pièces
pieces(j1, [sr1,sr2,sr3,sr4,sr5,kar]).
pieces(j2, [so1,so2,so3,so4,so5,kao]).

% Kalistas
kalista(kar).
kalista(kao).
kalista(j1, kar).
kalista(j2, kao).

% Vérifie que l'élement donné est une pièce valide
isPiece(j1, P) :- pieces(j1, A), member(P, A), !.
isPiece(j2, P) :- pieces(j2, B), member(P, B), !.

% Afiche le plateau de jeu
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
    write('X▼ Y ▶   '),
    displayXCoord(LineSize), nl,
    displayRows(Board, ColumnSize).

% Demander la position du board
askPosition(PositionChoisie) :-
    write('Quelle position souhaitez-vous avoir ? [1 (Nord) - 2 (Sud, actuel) - 3 (Ouest) - 4 (Est)]'),
    nl,
    read(TempPosition),
    % Utilisation d'une condition if else : "->" action_si_vrai, ";" action_si_faux
    (member(TempPosition, [1,2,3,4])
    ->  PositionChoisie is TempPosition
    ;   askPosition(PositionChoisie)
    ).

% Demander où placer les pièces
isSetupPlaceAllowed(j1, 0).
isSetupPlaceAllowed(j1, 1).
isSetupPlaceAllowed(j2, 4).
isSetupPlaceAllowed(j2, 5).

askPieces(Player, [], Board, Board) :- !.
askPieces(Player, [Piece|Q], Board, NewBoard) :-
    nl,
    write('Coordonnées de la pièce '),
    write(Piece), % afficher la pièce à insérer
    write(' : '),
    nl,
    write('X : '),
    read(X),
    write('Y : '),
    read(Y),
    % Utilisation d'une condition if else : "->" action_si_vrai, ";" action_si_faux
    (isSetupPlaceAllowed(Player, X)
        ->  (\+ squareTaken([X,Y], Board)
            ->  movePiece(Player,Piece, X, Y, Board, TmpBoard, OldPiece), % si la case est libre, alors bouger la pièce sur la case demandée par le joueur
            nl,
            displayBoard(TmpBoard), % afficher le nouveau plateau temporaire avec la pièce nouvellement insérée
            askPieces(Player, Q, TmpBoard, NewBoard) % demander à placer le reste des pièces
            ;   write('Case déjà prise.'),
            askPieces(Player, [Piece|Q], Board, NewBoard) % la case est déjà prise, le notifier au joueur et lui redemmander où placer ses pièces
        )
        ;   write('Vous devez vous placer dans vos deux premières lignes en début de partie.'),
        askPieces(Player, [Piece|Q], Board, NewBoard) % la case est déjà prise, le notifier au joueur et lui redemmander où placer ses pièces
    ).

% Demander à un joueur de placer ses pièces
askPlayerPiecesSetUp(Player, Board, NewBoard) :-
    pieces(Player, PlayerPieces), % PlayerPieces = liste des pièces du joueur Player
    write(PlayerPieces),nl, % afficher la liste des pièces du joueur
    askPieces(Player, PlayerPieces, Board, NewBoard). % lui demander où les placer sur le plateau de jeu

% Initialiser le plateau de jeu
initHumanVsHumanBoard(Board) :-
    write('Position initiale :     1▼'),
    nl,
    board(2, InitialBoard), % position au Sud par défaut
    displayBoard(InitialBoard), % afficher le plateau
    nl,
    write('                        2▲'),
    nl, nl,
    askPosition(WantedPosition),
    board(WantedPosition, WantedBoard), % Initialiser WantedBoard avec la disposition demandée
    write('                    Joueur 1 ▼'),
    nl,
    displayBoard(WantedBoard), % Afficher le plateau avec la disposition demandée
    nl,
    write('                    Joueur 2 ▲'),
    nl,
    write('Placement initial du Joueur 1 ->'),
    nl,
    askPlayerPiecesSetUp(j1, WantedBoard, TmpBoard), % demander au Joueur1 de placer ses pièces et enregistrer le résulat dans TmpBoard
    nl,
    write('Placement initial du Joueur 2 ->'),
    nl,
    askPlayerPiecesSetUp(j2, TmpBoard, Board). % demander au Joueur2 de placer ses pièces et enregistrer le résulat dans Board



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Etape 2
% 
% Coups possibles
%

% Récupére les corrdonnées d'une pièce
getPiece(Board,[X,Y],Piece):-
    squareTaken([X,Y],Board),
    getElementByIndex(X,Board,XLine),
    getElementByIndex(Y,XLine,YLine),
    getElementByIndex(1,YLine,Piece), !.
getPiece(Board,[X,Y],Piece):- getInfoPiece(Board,[_,X,Y],Piece).

% Récupérer la valeur et les cordonnées d'une pièce
getInfoPiece(Board,[Val,X,Y],Piece):- isPiece(Player,Piece), getPiecesOnBoard(Player, Board, ListPieces,0,0), member([Val,Piece,X,Y],ListPieces),!.

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
buildNewSquare(OldSquare, NewPiece, NewSquare, []) :-
    append([OldSquare],[NewPiece],NewSquare).

% Faire bouger une pièce
% Penser à supprimer la pièce de son ancienne case
movePiece(Player, PieceMoved, X, Y, Board, NewBoard, OldPiece) :- !,
    getElementByIndex(X, Board, Line), % Ligne = Ligne d'indice X
    getElementByIndex(Y, Line, Square), % Square = case d'indice Y dans Ligne
    buildNewSquare(Square, PieceMoved, NewSquare, OldPiece), !, % NewSquare = case Square avec l'ajout ou le remplacement de la pièce PieceMoved
    \+ isPiece(Player,OldPiece),
    replace(Line, Y, NewSquare, NewLine), % NewLine = Line avec le remplacement de la case d'indice Y par NewSquare
    replace(Board, X, NewLine, NewBoard), !. % NewBoard = Board avec le remplacement de la ligne d'indice X par NewLine


%Extract and delete pieces from board
%removePieceFromBoard(OldBoard, PieceToExctract, NewBoard)
removePieceFromBoard([],_,[]) :-
    fail, !.

removePieceFromBoard([OldLine|Q], Piece, [NewLine|Q]) :-
    member([Num,Piece], OldLine), % si la pièce est sur la ligne, c'est à dire qu'il y a une case égale à [Num,Piece] (où Num s'unifie avec la valeur de la case)
    !, % permet d'empêcher le backtracking sur les instructions qui suivent le !
    % chaque pièce est unique, donc replaceElement suffit : on remplace [Num,Piece] par Num, exemple [1,sr1] sera remplacé par 1 si Piece = sr1
    replaceElement([Num,Piece], Num, OldLine, NewLine). % NewLine prend donc la valeur de OldLine sans la pièce qu'on voulait enlever

removePieceFromBoard([OldLine|Q], Piece, [OldLine|R]) :- 
    \+ member([Num,Piece], OldLine), % si la pièce à enlever n'est pas dans la ligne OldLine du plateau 
    removePieceFromBoard(Q, Piece, R), !. % alors aller chercher dans les lignes suivantes du plateau

moveUp(Board, Player,OldPos,[ActualX, ActualY], [NewX,NewY], OldActualPos) :-
    NewX is ActualX - 1,
    NewY is ActualY,
    \+ member([NewX, NewY], OldPos),
    \+ forbiddenMove(Board,Player,[NewX,NewY]),
    append(OldPos, [[ActualX,ActualY]], OldActualPos).

moveDown(Board, Player,OldPos,[ActualX, ActualY], [NewX,NewY], OldActualPos) :-
    NewX is ActualX + 1,
    NewY is ActualY,
    \+ member([NewX, NewY], OldPos),
    \+ forbiddenMove(Board,Player,[NewX,NewY]),
    append(OldPos, [[ActualX,ActualY]], OldActualPos).

moveLeft(Board, Player,OldPos,[ActualX, ActualY], [NewX,NewY], OldActualPos) :-
    NewX is ActualX,
    NewY is ActualY - 1,
    \+ member([NewX, NewY], OldPos),
    \+ forbiddenMove(Board,Player,[NewX,NewY]),
    append(OldPos, [[ActualX,ActualY]], OldActualPos).

moveRight(Board, Player,OldPos,[ActualX, ActualY], [NewX,NewY], OldActualPos) :-
    NewX is ActualX,
    NewY is ActualY + 1,
    \+ member([NewX, NewY], OldPos),
    \+ forbiddenMove(Board,Player,[NewX,NewY]),
    append(OldPos, [[ActualX,ActualY]], OldActualPos).


place(Player, I, Board, OldPos,ActualPos, NewPos, AllOldOldPos):-
    I1 is I-1,
    I1 = 0,
    moveUp(Board, Player,OldPos,ActualPos, NewPos, AllOldOldPos),
    allowedPlace(NewPos).

place(Player, I, Board, OldPos,ActualPos, NewPos, AllOldOldPos):-
    I1 is I-1,
    I1 = 0,
    moveDown(Board, Player,OldPos,ActualPos, NewPos, AllOldOldPos),
    allowedPlace(NewPos).

place(Player, I, Board, OldPos,ActualPos, NewPos, AllOldOldPos):-
    I1 is I-1,
    I1 = 0,
    moveLeft(Board, Player,OldPos,ActualPos, NewPos, AllOldOldPos),
    allowedPlace(NewPos).

place(Player, I,  Board,OldPos,ActualPos, NewPos, AllOldOldPos):-
    I1 is I-1,
    I1 = 0,
    moveRight(Board, Player,OldPos,ActualPos, NewPos, AllOldOldPos),
    allowedPlace(NewPos), !.

% place(I, OldPos,ActualPos, NewPos, AllOldOldPos):- I1 is I-1, I1 > 0, !.

place(Player, I, Board, OldPos,ActualPos, NewPos, AllOldOldPos):-
    moveUp(Board, Player,OldPos,ActualPos, NewActualPos, AllOldPos),
    allowedPlace(NewActualPos),
    \+ squareTaken(NewActualPos, Board),
    I1 is I-1,
    place(Player, I1,Board, AllOldPos,NewActualPos, NewPos, AllOldOldPos).

place(Player, I, Board, OldPos,ActualPos, NewPos, AllOldOldPos):-
    moveDown(Board, Player,OldPos,ActualPos, NewActualPos, AllOldPos),
    allowedPlace(NewActualPos), \+ squareTaken(NewActualPos, Board),
    I1 is I-1,
    place(Player, I1,Board, AllOldPos,NewActualPos, NewPos, AllOldOldPos).

place(Player, I, Board, OldPos,ActualPos, NewPos, AllOldOldPos):-
    moveLeft(Board, Player,OldPos,ActualPos, NewActualPos, AllOldPos),allowedPlace(NewActualPos),
    \+ squareTaken(NewActualPos, Board),
    I1 is I-1,
    place(Player, I1,Board, AllOldPos,NewActualPos, NewPos,AllOldOldPos).

place(Player, I, Board,OldPos,ActualPos, NewPos, AllOldOldPos):-
    moveRight(Board, Player,OldPos,ActualPos, NewActualPos, AllOldPos),
    allowedPlace(NewActualPos),
    \+ squareTaken(NewActualPos, Board),
    I1 is I-1,
    place(Player, I1,Board, AllOldPos,NewActualPos, NewPos, AllOldOldPos).

% get the pieces on the board at the moment, with the value of the box and the coordinates
getPiecesOnLine(Player,[],[],_,_):-!.
getPiecesOnLine(Player,[H|T],[L|PList],X,Y):- Y1 is Y+1, squareTakenOnLine(0,[H|T]),
    getElementByIndex(1,H,TL), isPiece(Player,TL), % only piece
    append(H,[X,Y],L),
    getPiecesOnLine(Player,T,PList, X,Y1),
    !.
getPiecesOnLine(Player,[H|T],PList, X,Y):- Y1 is Y+1, getPiecesOnLine(Player,T,PList,X,Y1).

getPiecesOnBoard(Player, [], [],_,_):-!.
getPiecesOnBoard(Player, [H|T], BPList,X,Y ):- getPiecesOnLine(Player,H,L,X,Y), X1 is X+1,getPiecesOnBoard(Player,T,LPList,X1,Y), append(L,LPList, BPList).

concatElementList(L,[],[]):-!.
concatElementList([X,Y],[H|T],[[H,X,Y]|L2]):- concatElementList([X,Y],T,L2).

% get all possible moves for each piece
getAllMoves(Player,[],Board,[]):-!.

getAllMoves(Player,[[Val,Piece, X,Y]|T], Board,AllMoves):-
    setof(K,khan(K),L),\+ member(Val,L),
    getAllMoves(Player,T, Board,AllMoves), !.

getAllMoves(Player,[[Val,Piece, X,Y]|T], Board,[AllMoves1Piece|AllMoves]):-
    setof(K,khan(K),L),member(Val,L),
    findall(L1,place(Player,Val,Board,[],[X,Y],L1,L2),R),
    removeDuplicates(R,R1),
    concatElementList([Val,Piece],R1,AllMoves1Piece),
    getAllMoves(Player,T,Board,AllMoves).

% get all possible moves for all the pieces
possibleMoves(Player, Board,PossibleMoveList) :-
    getPiecesOnBoard(Player, Board, PList,0,0),% PList = [val, piece, X, Y]
    getAllMoves(Player, PList,Board,PossibleMoveList).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
allowedPlace([X,Y]):- X > -1, Y > -1, X < 6, Y < 6.

allowedMove([[NewX,NewY],_,Piece],[H|[]]):- member([[NewX,NewY],_,Piece],H),!.
allowedMove([[NewX,NewY],_,Piece],[H|T]):- member([[NewX,NewY],_,Piece],H).
allowedMove([[NewX,NewY],_,Piece],[H|T]):- allowedMove([[NewX,NewY],_,Piece],T).

allowedMove(Player, Board,NewX,NewY,Piece):- possibleMoves(Player, Board,L),
    allowedMove([[NewX,NewY],_,Piece],L).

forbiddenMove(Board,Player,NewCoord) :- getPiece(Board,NewCoord, OldPiece), isPiece(Player,OldPiece).

selectMove(Player, X, Y, Board, NewBoard, OldPiece) :-
    nl,
    write('Khan positionné à '), setof(K,khan(K),R), write(R), nl,
    write('Pièces : '), pieces(Player, Pieces), write(Pieces), nl,
    write('Quelle pièce voulez-vous jouer ?'),
    read(Piece),
    (\+ isPiece(Player,Piece) % si la pièce n'est pas une des pièces du joueur
        ->  write('Pièce invalide'),
        selectMove(Player, X, Y, Board, NewBoard, OldPiece)
        ;   % displayBoard(Board),
        (getInfoPiece(Board,ValList,Piece), getElementByIndex(0,ValList,Val), member(Val,R) % on regarde que la pièce quon souhaite bouger respecte le khan actuel
        ->  write('Deplacement de la pièce '),
            write(Piece),
            write(' : '),
            nl,
            write('X : '), read(X),
            write('Y : '), read(Y),
            removePieceFromBoard(Board, Piece, TmpBoard),% enlever la pièce du plateau de jeu
            (allowedMove(Player, Board, X,Y,Piece),%le déplacement doit être présent dans la liste des coups possibles
                movePiece(Player,Piece, X, Y, TmpBoard, NewBoard, OldPiece) %si on arrive pas à placer la pièce sur sa nouvelle position
            ->
                getInfoPiece(NewBoard,[NewVal,_,_],Piece),
                retractall(khan(K)),nl,
                asserta(khan(NewVal)) % on met le khan à la nouvelle valeur
            ;
                nl,
                write('ERREUR : Déplacement non autorisé !!'),nl,
                canPlay(Player,Board),
                setof(R2,possibleMoves(Player, Board,R2),L),
                write('Liste des mouvements autorisés :'), write(L),nl,
                selectMove(Player,U,V,Board,NewBoard,OldPiece)

            )
        ;
            write('Le khan est possitionné sur '), khan(K), write(K), write(', choisir une autre pièce.'),nl,
            selectMove(Player, X, Y, Board, NewBoard, OldPiece)
        )
    ).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Etape 3
%
% Meilleur coup % checker si la kalista prise n'est pas la notre
%

% cherche la valeur min d'une liste'
min([],Nmin, Nmin) :- !.
min([[X,Y,N]|T], Nmin, Min):- % lancer avec la valeur Nmin 100
    N > 50, min(T,Nmin,Min),!.

min([[X,Y,N]|T], Nmin, Min):- % lancer avec la valeur Nmax 100
    (N < Nmin
        -> MinTmp is N
       ;  MinTmp is Nmin
    ),
    min(T,MinTmp,Min).

% cherche la valeur max d'une liste'
max([],Nmax, Nmax) :- !.
max([[X,Y,N]|T], Nmax, Max):- % lancer avec la valeur Nminmax 0
    N > 100, max(T,Nmax,Max),!. % tout sauf la valeur qui nous indique qu'on a perdu'

max([[X,Y,N]|T], Nmax, Max):- % lancer avec la valeur Nminmax 0
    (N > Nmax
        -> MaxTmp is N
       ;  MaxTmp is Nmax
    ),
    max(T,MaxTmp,Max).

% donne le meilleur déplacement final
giveRandomMove([[X,Y,N]|T], [X,Y]):- !. % pas vraiment random pour l'instant...'

giveMove(Nmin, [[X,Y,N]|[]],[X,Y]):- !.
giveMove(Nmin, [[X,Y,N]|T], [X,Y]):-
    Nmin = N, !.
giveMove(Nmin, [[X,Y,N]|T],[NX,NY]) :-
    giveMove(Nmin, T,[NX,NY]).

% retourne le meilleur coup selon une liste des meilleurs coups
minmax(BestMovesList, [X,Y,N]):-
    min(BestMovesList, 100, Nmin),
    (Nmin > 50 % soit la recherche n'a pas abouti', ou soit on se fait bouffer notre kalista
    -> max(BestMovesList, 0, Nmax), % on donnera alors le chemin le plus long possible
        (Nmax = 0
           -> giveRandomMove(BestMovesList, [X,Y]), N is Nmax
        ; giveMove(Nmax, BestMovesList, [X,Y])), N is Nmax
    ; giveMove(Nmin, BestMovesList, [X,Y]), N is Nmin % on donnera le chemin le plus court
    ).

%comme on cherche le chemin le plus court on va renvoyer une valeur infinie négative pour une victoire
evaluateSituation(Player, Piece, X, Y, Board, OldN, Khan, NewN) :-
    removePieceFromBoard(Board, Piece, TmpBoard),
    movePiece(Player,Piece, X, Y, TmpBoard, NewBoard, OldPiece),
    kalista(OldPiece),NewN is -100,!.


% si on ne peut pas prendre la kalista, il s'agit de donner une évaluation du coût du déplacement sur une case en fonction de la posibilité de victoire de l'adversaire -> s'il peut prendre ma kalista je ne bouge pas
evaluateSituation(Player, Piece, X, Y, Board, OldN, Khan, NewN) :-
    removePieceFromBoard(Board, Piece, TmpBoard),
    movePiece(Player,Piece, X, Y, TmpBoard, NewBoard, OldPiece),
    getInfoPiece(NewBoard,[NewVal,_,_],Piece),
    retractall(khan(Z)), nl,
    asserta(khan(NewVal)), % on met le khan à la nouvelle valeur
    changePlayer(Player, Opponent),
    N2 is OldN + 10,
    (N2 < 50
        -> displayBoard(NewBoard), bestMove(Opponent,NewBoard, N2, N3, _), NewN is N2 + N3
        ; NewN is N2),
    retractall(khan(W)),
    asserta(khan(Khan)).

% construction de la BestMovesList contenant tous les coups qui ne sont pas perdants (N < 30): [[X,Y,N]|T]
calculateMovesCost(_, _, [], _, []).
calculateMovesCost(Player, Board, [[[X,Y], Val, Piece]|Q], OldN, CostedPossibleMovesList) :-
    khan(Khan),
    calculateMovesCost(Player, Board, Q, OldN, TmpCostedPossibleMovesList),
    evaluateSituation(Player, Piece, X, Y, Board, OldN, Khan, NewN),
    append(TmpCostedPossibleMovesList, [[X, Y, NewN]], CostedPossibleMovesList).

flattenPossibleMoves([], []).
flattenPossibleMoves([PieceMoves|Q], PossibleMovesList) :-
    flattenPossibleMoves(Q, TmpPossibleMovesList),
    append(PieceMoves, TmpPossibleMovesList, PossibleMovesList).

% donne le meilleur coup à jouer pour un joueur et l'état du plateau à un instant donné
bestMove(Player, Board, OldN, NewN, [X,Y]):-
    OldN < 50, % on limite ici la recherche à au minimum 3 niveaux (un deplacement coûte 10)
    possibleMoves(Player,Board, MovablePieces),
        flattenPossibleMoves(MovablePieces, PossibleMovesList),
        calculateMovesCost(Player, Board, PossibleMovesList, OldN, CostedPossibleMovesList),nl,
        minmax(CostedPossibleMovesList, [X,Y,N]),nl,write('Best move for'), write(Player), write(' is '), write([X,Y]),nl,
        NewN is OldN,!.

bestMove(Player, Board, OldN, OldN, _).

bestMove(Player, Board, [X,Y]):- bestMove(Player, Board, 0, N, [X,Y]).



% Génère le meilleur coup avec minmax
generateMove(Player, X, Y, Board, NewBoard, OldPiece) :-
    possibleMoves(Player, Board, MovablePieces),
    evaluatePieceMoves(Player, MovablePieces, BestMovesList),
    minmax(BestMovesList, [X, Y]),

    write(MovablePieces).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Etape 4
% 
% Boucle de jeu
%

% Victory conditions
victory(Player,OldPiece) :-
    kalista(OldPiece),nl, % si la pièce prise est la kalista du joueur adverse
    write('*********************************************************************'),     nl,
    write('                FIN DU JEU : '), write(Player), write(' a gagné la partie !'),!,   nl,
    write('*********************************************************************'),     nl,
    break. % arrêt du jeu
victory(_,_).

testBoard([[3,[1,kao],2,2,3,1], [[2,sr5], 3,[1,sr1],[3,sr2],[1,sr3],[2,sr4]], [[2,kar],[1,so1],[3,so2],[1,so3],[3,so4],[2,so5]],[1,3,2,2,1,3], [3,1,3,1,3,1],  [2,2,1,3,2,2]]).

canPlay(Player, Board) :- possibleMoves(Player, Board,L),\+ empty(L),!.
canPlay(Player, Board) :- write('Configuration bloquée. Toutes les pièces peuvent être bougées : '), retractall(khan(Y)), asserta(khan(3)), asserta(khan(2)),asserta(khan(1)).

humanPlay(Board, Player, NewBoard) :-
    selectMove(Player, X, Y, Board, NewBoard, OldPiece),
    victory(Player,OldPiece).

computerPlay(Board, Player, NewBoard) :-
    generateMove(Player, X, Y, Board, NewBoard, OldPiece),
    victory(Player,OldPiece).

runHumanVsHuman(Board) :-
    canPlay(j1,Board), nl,
    write('Joueur1 ->'),
    humanPlay(Board, j1, NewBoard1), nl,
    displayBoard(NewBoard1), nl,
    canPlay(j2,NewBoard1), nl,
    write('Joueur2 ->'),
    humanPlay(NewBoard1, j2, NewBoard2),
    displayBoard(NewBoard2), nl,
    runHumanVsHuman(NewBoard2).

runHumanVsComputer(Board) :-
    canPlay(j1,Board), nl,
    write('Humain ->'),
    humanPlay(Board, j1, NewBoard1), nl,
    displayBoard(NewBoard1), nl,
    canPlay(j2,NewBoard1), nl,
    write('Ordinateur ->'),
    computerPlay(NewBoard1, j2, NewBoard2),
    displayBoard(NewBoard2), nl,
    runHumanVsComputer(NewBoard2).

runComputerVsComputer(Board) :-
    canPlay(j1,Board), nl,
    write('Ordinateur1 ->'),
    computerPlay(Board, j1, NewBoard1), nl,
    displayBoard(NewBoard1), nl,
    canPlay(j2,NewBoard1), nl,
    write('Ordinateur2 ->'),
    computerPlay(NewBoard1, j2, NewBoard2),
    displayBoard(NewBoard2), nl,
    runComputerVsComputer(NewBoard2).

start(1) :- % Humain vs Humain
    write('*********************************************************************'), nl,
    write('                          HUMAIN vs HUMAIN '),                           nl,
    write('*********************************************************************'), nl,
    initHumanVsHumanBoard(B), nl,
    write('*********************************************************************'), nl,
    write('                            DEBUT DU JEU '),                             nl,
    write('*********************************************************************'), nl,
    displayBoard(B), nl,
    runHumanVsHuman(B),
    nl.

start(2) :- % Humain vs Ordinateur
    write('*********************************************************************'), nl,
    write('                        HUMAIN vs ORDINATEUR '),                         nl,
    write('*********************************************************************'), nl,
    initHumanVsComputerBoard(B), nl,
    write('*********************************************************************'), nl,
    write('                            DEBUT DU JEU '),                             nl,
    write('*********************************************************************'), nl,
    displayBoard(B), nl,
    runHumanVsComputer(B),
    nl.

start(3) :- % Ordinateur vs Ordinateur
    write('*********************************************************************'), nl,
    write('                       ORDINATEUR vs ORDINATEUR '),                      nl,
    write('*********************************************************************'), nl,
    initComputerVsComputerBoard(B), nl, % Plateau au hasard
    write('*********************************************************************'), nl,
    write('                            DEBUT DU JEU '),                             nl,
    write('*********************************************************************'), nl,
    displayBoard(B), nl,
    runComputerVsComputer(B),
    nl.

main(_):-
    retractall(khan(Y)),
    asserta(khan(1)),
    asserta(khan(2)),
    asserta(khan(3)),
    write('*********************************************************************'), nl,
    write('                      BIENVENUE AU JEU DE KHAN '),                       nl,
    write('*********************************************************************'), nl,
    nl,
    write('Quel type de partie souhaitez vous démarrer ? (1 : Humain vs Humain, 2 : Humain vs Ordinateur, 3 : Ordinateur vs Ordinateur) : '),
    read(TypeOpponent), nl,
    start(TypeOpponent).
