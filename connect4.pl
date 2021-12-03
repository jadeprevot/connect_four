%Connect Four
% 4IF Project
% Enter 'playAIvsAI.' to watch an AI match.
% Enter 'playvsAI.' to play the game versus the computer.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%Code by H4114
%


% Loop to measure heuristics ----------------------

to_100_matchs(100,V,V2) :- foocounter(V),foocounter2(V2),nl.
to_100_matchs(0,V,V2) :-
   initfoo,
   initfoo2,
   playAIvsAI,
   Y is 1,
   to_100_matchs(Y,V,V2).


to_100_matchs(X,V,V2) :-
   playAIvsAI,

   Y is X + 1,
   to_100_matchs(Y,V,V2).

:- dynamic foocounter/1.

initfoo :-
    retractall(foocounter(_)),
    assertz(foocounter(0)).

incrfoo :-
    foocounter(V0),
    retractall(foocounter(_)),
    succ(V0, V),
    assertz(foocounter(V)).

:- dynamic foocounter2/1.

initfoo2 :-
    retractall(foocounter2(_)),
    assertz(foocounter2(0)).

incrfoo2 :-
    foocounter2(V0),
    retractall(foocounter2(_)),
    succ(V0, V),
    assertz(foocounter2(V)).



% make two AIs play together ----------------------
playAIvsAI :- getBlankBoard(Board), nextPlayAIvsAI(Board, 1,0).


nextPlayAIvsAI(Board, 1,Ind) :- getAIh2Move(Board, Move,1,2),
					  %nl,write('Yellow dropped piece into column '),write(Move),nl,nl,
					  getNextState(Board, 1, Move, NewBoard, NewPlayer, OutDropXY),
					  drawBoard(NewBoard),
                                          Ind1 is Ind + 1,
					  nextStateAI(NewBoard, NewPlayer, OutDropXY, Ind1).


nextPlayAIvsAI(Board, 2,Ind) :-getAIh1Move(Board, Move,2,1),
					  %nl,write('Red dropped piece into column '),write(Move),nl,nl

					  getNextState(Board, 2, Move, NewBoard, NewPlayer, OutDropXY),
					  drawBoard(NewBoard),
                                          Ind1 is Ind + 1,
					  nextStateAI(NewBoard, NewPlayer, OutDropXY,Ind1).

nextStateAI(Board, 2, DropXY,_) :-
   win(Board, 1, DropXY),nl, write('Yellow wins!'),
   %incrfoo,
   nl,endmessage.
nextStateAI(Board, 1, DropXY,_) :-
   win(Board, 2, DropXY),nl, write('Red wins!'),
   %incrfoo2,
   nl, endmessage.
nextStateAI(Board,_,_,_) :- isFull(Board),nl, write('It\'s a tie!'),nl, endmessage.
nextStateAI(Board, NewPlayer,_,Ind) :- nextPlayAIvsAI(Board, NewPlayer,Ind).



% AIs moves -----------------------------
getAIh2Move(Board, Move,P,Depth) :- 1 is mod(Depth,2),minMax2(Board,Depth,true,P,Move,_).
getAIh2Move(Board, Move,P,Depth) :- 0 is mod(Depth,2),minMax2(Board,Depth,false,P,Move,_).

getAIh1Move(Board, Move,P,Depth) :- 1 is mod(Depth,2),minMax1(Board,Depth,true,P,Move,_).
getAIh1Move(Board, Move,P,Depth) :- 0 is mod(Depth,2),minMax1(Board,Depth,false,P,Move,_).

getAIh3Move(Board, Move,P,Depth) :- 1 is mod(Depth,2),minMax3(Board,Depth,true,P,Move,_).
getAIh3Move(Board, Move,P,Depth) :- 0 is mod(Depth,2),minMax3(Board,Depth,false,P,Move,_).



% play the game vs an AI ---------

playvsAI :- getBlankBoard(Board),nextPlayvsAI(Board, 1).

nextPlayvsAI(Board, 1) :- nl,write('It is your turn.'),nl,nl,
					  drawBoard(Board),
					  read(Move),
					  getNextState(Board, 1, Move, NewBoard, NewPlayer, OutDropXY),
					  nextStateAI2(NewBoard, NewPlayer, OutDropXY).

nextPlayvsAI(Board, 2) :- nl,write('Computer is making move...'),nl,nl,
					  drawBoard(Board),
					  getAIh2Move(Board, Move,2,3),
					  nl,write('Computer dropped piece into column '),write(Move),nl,nl,
					  getNextState(Board, 2, Move, NewBoard, NewPlayer, OutDropXY),
					  nextStateAI2(NewBoard, NewPlayer, OutDropXY).

nextStateAI2(Board, 2, DropXY) :- win(Board, 1, DropXY),nl, write('You win!'),nl, endmessage.
nextStateAI2(Board, 1, DropXY) :- win(Board, 2, DropXY),nl, write('The computer wins!'),nl, endmessage.
nextStateAI2(Board,_,_) :- isFull(Board),nl, write('It\'s a tie!'),nl, endmessage.
nextStateAI2(Board, NewPlayer,_) :- nextPlayvsAI(Board, NewPlayer).


% pre existing code changed ------------
getDiagUpHelper(_, (8,_), []).
getDiagUpHelper(_, (_,0), []).
getDiagUpHelper(Board, (CurrX, CurrY), [H|T]) :-  CurrX < 8, CurrY > 0,
													is(NextX, CurrX + 1), is(NextY, CurrY - 1),
													getElem(Board, CurrX, CurrY, 1, 1, H),
												    getDiagUpHelper(Board, (NextX, NextY), T).


inversionPlayer(1,2).
inversionPlayer(2,1).

heuristic1(Board,Col,Player,false,-50001):- \+ isIllegal(Board,Col),wouldWin(Board, Col, Player).
heuristic1(Board,Col,Player,false,-50000):- \+ isIllegal(Board,Col),inversionPlayer(Player,NotPlayer),wouldWin(Board, Col, NotPlayer).
heuristic1(Board,Col,Player,true,50001):- \+ isIllegal(Board,Col),wouldWin(Board, Col, Player).
heuristic1(Board,Col,Player,true,50000):- \+ isIllegal(Board,Col),inversionPlayer(Player,NotPlayer),wouldWin(Board, Col, NotPlayer).
heuristic1(Board,Col,Player,_,Score):- \+ isIllegal(Board,Col),calcScore(Board,Col,Player,Score).
heuristic1(_,_,_,true,-50003).
heuristic1(_,_,_,false,50003).
%
% Return the Score when player plays in the col
calcScore(Board,Col,Player,Score):-getDropXY(Board,Col,(X,Y)),calcScore(Board,X,Y,Player,Score).
calcScore(Board,X,Y,Player,Score):-getRow(Board,(X,Y),Row),countLineScore(Row,X,Score1,Player),
			           getCol(Board,(X,Y),Col),countLineScore(Col,Y,Score2,Player),
				   posDiagDown(X,Y,I1),getDiagDown(Board,(X,Y),DiagDown),countLineScore(DiagDown,I1,Score3,Player),
				   posDiagUp(X,Y,I2),getDiagUp(Board,(X,Y),DiagUp),countLineScore(DiagUp,I2,Score4,Player),
				   Score is Score1+Score2+Score3+Score4.

%I is the position in the diagonal du point of coordinates X Y
posDiagDown(X,Y,I):-I is min(X,Y).
posDiagUp(X,Y,I):-I is min(7-Y,X).


% For a row and a position, Score is the number of Player elements
% in the 3 previous positions and 3 following positions
%(We evaluate a range of 3 around the initial position)
countLineScore(L,PosX,Score,Player):-length(L,Taille),countLineScore(L,PosX,1,Score,Player,Taille).
countLineScore(_,_,CurrentX,0,_,Taille):-CurrentX is Taille+1.
countLineScore(_,PosX,CurrentX,0,_,_):- CurrentX is PosX+4.
countLineScore([F|R],PosX,CurrentX,Score,Player,Taille):- PosX-CurrentX<4, F==Player, is(NextX, CurrentX+1), countLineScore(R,PosX,NextX,NextScore,Player,Taille),Score is NextScore+1.
countLineScore([_|R],PosX,CurrentX,Score,Player,Taille):-is(NextX, CurrentX+1),countLineScore(R,PosX,NextX,Score,Player,Taille).


% Weighted board
weight([[3,4,5,7,5,4,3],[4,6,8,10,8,6,4],[5,8,11,13,11,8,5],[5,8,11,13,11,8,5],[4,6,8,10,8,6,4],[3,4,5,7,5,4,3]]).

heuristic2(Board,Col,Player,false,-50001):- \+ isIllegal(Board,Col),wouldWin(Board, Col, Player).
heuristic2(Board,Col,Player,false,-50000):- \+ isIllegal(Board,Col),inversionPlayer(Player,NotPlayer),wouldWin(Board, Col, NotPlayer).
heuristic2(Board,Col,Player,true,50001):- \+ isIllegal(Board,Col),wouldWin(Board, Col, Player).
heuristic2(Board,Col,Player,true,50000):- \+ isIllegal(Board,Col),inversionPlayer(Player,NotPlayer),wouldWin(Board, Col, NotPlayer).
heuristic2(Board,Col,Player,_,Score):- \+ isIllegal(Board,Col),dropToken(Board,Player,Col,NewBoard),boardScoreWeights(NewBoard,1,1,Score,Player).
heuristic2(_,_,_,true,-50003).
heuristic2(_,_,_,false,50003).



% For each position, if the token belongs to Player, we add
% the weight of the box to the score
% If the token belongs to the opponent, we subtract this weight
boardScoreWeights([],_,_,0,_).
boardScoreWeights([[]|T],_,Y,Score,Player):- NewY is Y+1, boardScoreWeights(T,1,NewY,Score,Player).
boardScoreWeights([[Player|T]|R],X,Y,Score,Player):-NewX is X+1,boardScoreWeights([T|R],NewX,Y,NewScore,Player),
                                                    weight(W),getElem(W,X,Y,1,1,Out),Score is NewScore+Out.
boardScoreWeights([[0|T]|R],X,Y,Score,Player):-NewX is X+1,boardScoreWeights([T|R],NewX,Y,Score,Player).
boardScoreWeights([[_|T]|R],X,Y,Score,Player):-NewX is X+1,boardScoreWeights([T|R],NewX,Y,NewScore,Player),
                                                    weight(W),getElem(W,X,Y,1,1,Out),Score is NewScore-Out.



% Set-of-4 heuristic


heuristic3(Board,Col,Player,false,-50001):- \+ isIllegal(Board,Col),wouldWin(Board, Col, Player).
heuristic3(Board,Col,Player,false,-50000):- \+ isIllegal(Board,Col),inversionPlayer(Player,NotPlayer),wouldWin(Board, Col, NotPlayer).
heuristic3(Board,Col,Player,true,50001):- \+ isIllegal(Board,Col),wouldWin(Board, Col, Player).
heuristic3(Board,Col,Player,true,50000):- \+ isIllegal(Board,Col),inversionPlayer(Player,NotPlayer),wouldWin(Board, Col, NotPlayer).
heuristic3(Board,Col,Player,_,Score):- \+ isIllegal(Board,Col),calcScore2(Board,Col,Player,Score).
heuristic3(_,_,_,true,-50003).
heuristic3(_,_,_,false,50003).


% Return the Score when player plays in the col
calcScore2(Board,Col,Player,Score):-getDropXY(Board,Col,(X,Y)),calcScore2(Board,X,Y,Player,Score).
calcScore2(Board,X,Y,Player,Score):-getRow(Board,(X,Y),Row),countLineScore2(Row,X,Score1,Player),
			   getCol(Board,(X,Y),Col),countLineScore2(Col,Y,Score2,Player),
		    posDiagDown(X,Y,I1),getDiagDown(Board,(X,Y),DiagDown),countLineScore2(DiagDown,I1,Score3,Player),
		    posDiagUp(X,Y,I2),getDiagUp(Board,(X,Y),DiagUp),countLineScore2(DiagUp,I2,Score4,Player),
		    Score is Score1+Score2+Score3+Score4.

%(We calculate the score around the initial position, putting 0 if an enemy pawn is on our line, otherwise 1)
countLineScore2(L,PosX,Score,Player):-length(L,Taille),countLineScore2(L,PosX,1,Score,Player,Taille).
countLineScore2(_,PosX,CurrentX,0,_,_):- CurrentX is PosX+1.
countLineScore2([F|R],PosX,CurrentX,Score,Player,Taille):- PosX-CurrentX<4, is(NextX, CurrentX+1), is(EndX, CurrentX+3), EndX<Taille+1, countSubLineScore([F|R],EndX,CurrentX,SubScore,Player,Taille) , fixScore(SubScore,FixSubScore), countLineScore2(R,PosX,NextX,NextScore,Player,Taille), Score is FixSubScore+NextScore.
countLineScore2([_|R],PosX,CurrentX,Score,Player,Taille):-is(NextX, CurrentX+1),countLineScore2(R,PosX,NextX,Score,Player,Taille).


countSubLineScore(_,_,CurrentX,0,_,Taille):-CurrentX is Taille+1.
countSubLineScore(_,EndX,CurrentX,0,_,_):- CurrentX is EndX+1.
countSubLineScore([F|R],EndX,CurrentX,Score,Player,Taille):-(F==Player; F==0), is(NextX, CurrentX+1), countSubLineScore(R,EndX,NextX,NextScore,Player,Taille),Score is NextScore+1.
countSubLineScore([F|_],_,_,-99,Player,_):- F\=Player, F\=0.


% If the score is negative, it's 0, else, 1
fixScore(In,0):-In<0.
fixScore(_,1).

%getNextMovesScore, used with the MinMax to loop between each columns
getNextMovesScore(_,_,_,_,8,[]).

getNextMovesScore(Node,Depth1,true,Player,Ind, [V|L]):- \+ isIllegal(Node,Ind), Ind1 is Ind + 1, wouldWin(Node,Ind,Player),V is 99999,
	getNextMovesScore(Node,Depth1,true,Player,Ind1, L),!.

getNextMovesScore(Node,Depth1,true,Player,Ind, [V|L]) :-
	\+ isIllegal(Node,Ind),Ind1 is Ind + 1,getNextMovesScore(Node,Depth1,true,Player,Ind1, L),
	getNextState(Node,Player,Ind,NewNode1,NewPlayer1,_),minMax1(NewNode1,Depth1,false,NewPlayer1,_,V),!.
getNextMovesScore(Node,Depth1,true,Player,Ind, [V|L]) :-Ind1 is Ind + 1,getNextMovesScore(Node,Depth1,true,Player,Ind1, L),V is -99999,!.

getNextMovesScore(Node,Depth1,false,Player,Ind, [V|L]):- \+ isIllegal(Node,Ind), Ind1 is Ind + 1, wouldWin(Node,Ind,Player),V is -99999,
	getNextMovesScore(Node,Depth1,false,Player,Ind1, L),!.

getNextMovesScore(Node,Depth1,false,Player,Ind, [V|L]) :-
	\+ isIllegal(Node,Ind),Ind1 is Ind + 1,getNextMovesScore(Node,Depth1,false,Player,Ind1, L),
	getNextState(Node,Player,Ind,NewNode1,NewPlayer1,_),minMax1(NewNode1,Depth1,true,NewPlayer1,_,V),!.
getNextMovesScore(Node,Depth1,false,Player,Ind, [V|L]) :- Ind1 is Ind + 1,getNextMovesScore(Node,Depth1,false,Player,Ind1, L),V is 99999,!.

%MinMax Methods used for each heuristic
minMax2(Node,1,true,Player,Col,Value):-
    heuristic2(Node,1,Player,true,V1),heuristic2(Node,2,Player,true,V2),heuristic2(Node,3,Player,true,V3),heuristic2(Node,4,Player,true,V4),
    heuristic2(Node,5,Player,true,V5),heuristic2(Node,6,Player,true,V6),heuristic2(Node,7,Player,true,V7),
    getMaxMove([V1,V2,V3,V4,V5,V6,V7],Col,Value),!.

minMax2(Node,1,false,Player, Col, Value):-
    heuristic2(Node,1,Player,false,V1),heuristic2(Node,2,Player,false,V2),heuristic2(Node,3,Player,false,V3),heuristic2(Node,4,Player,false,V4),
    heuristic2(Node,5,Player,false,V5),heuristic2(Node,6,Player,false,V6),heuristic2(Node,7,Player,false,V7),
    getMinMove([V1,V2,V3,V4,V5,V6,V7],Col,Value),!.

minMax2(Node,Depth,true,Player,Col,Value):- Depth1 is Depth - 1,getNextMovesScore(Node,Depth1,true,Player,1,L),getMaxMove(L,Col,Value).

minMax2(Node,Depth,false,Player,Col,Value):- Depth1 is Depth - 1,getNextMovesScore(Node,Depth1,false,Player,1,L),getMinMove(L,Col,Value).

minMax1(Node,1,true,Player,Col,Value):-
    heuristic1(Node,1,Player,true,V1),heuristic1(Node,2,Player,true,V2),heuristic1(Node,3,Player,true,V3),heuristic1(Node,4,Player,true,V4),
    heuristic1(Node,5,Player,true,V5),heuristic1(Node,6,Player,true,V6),heuristic1(Node,7,Player,true,V7),
    getMaxMove([V1,V2,V3,V4,V5,V6,V7],Col,Value),!.

minMax1(Node,1,false,Player, Col, Value):-
    heuristic1(Node,1,Player,false,V1),heuristic1(Node,2,Player,false,V2),heuristic1(Node,3,Player,false,V3),heuristic1(Node,4,Player,false,V4),
    heuristic1(Node,5,Player,false,V5),heuristic1(Node,6,Player,false,V6),heuristic1(Node,7,Player,false,V7),
    getMinMove([V1,V2,V3,V4,V5,V6,V7],Col,Value),!.

minMax1(Node,Depth,true,Player,Col,Value):- Depth1 is Depth - 1,getNextMovesScore(Node,Depth1,true,Player,1,L),getMaxMove(L,Col,Value).

minMax1(Node,Depth,false,Player,Col,Value):- Depth1 is Depth - 1,getNextMovesScore(Node,Depth1,false,Player,1,L),getMinMove(L,Col,Value).

minMax3(Node,1,true,Player,Col,Value):-
    heuristic3(Node,1,Player,true,V1),heuristic3(Node,2,Player,true,V2),heuristic3(Node,3,Player,true,V3),heuristic3(Node,4,Player,true,V4),
    heuristic3(Node,5,Player,true,V5),heuristic3(Node,6,Player,true,V6),heuristic3(Node,7,Player,true,V7),
    getMaxMove([V1,V2,V3,V4,V5,V6,V7],Col,Value),!.

minMax3(Node,1,false,Player, Col, Value):-
    heuristic3(Node,1,Player,false,V1),heuristic3(Node,2,Player,false,V2),heuristic3(Node,3,Player,false,V3),heuristic3(Node,4,Player,false,V4),
    heuristic3(Node,5,Player,false,V5),heuristic3(Node,6,Player,false,V6),heuristic3(Node,7,Player,false,V7),
    getMinMove([V1,V2,V3,V4,V5,V6,V7],Col,Value),!.

minMax3(Node,Depth,true,Player,Col,Value):- Depth1 is Depth - 1,getNextMovesScore(Node,Depth1,true,Player,1,L),getMaxMove(L,Col,Value).

minMax3(Node,Depth,false,Player,Col,Value):- Depth1 is Depth - 1,getNextMovesScore(Node,Depth1,false,Player,1,L),getMinMove(L,Col,Value).

% X is the position of the highest element in the list L.
getMaxMove(L, Col, Y) :- max_list(L, Y), findall(N,nth1(N,L,Y),C),random_member(Col,C),!.

% X is the position of the lowest element in the list L.
getMinMove(L, Col, Y) :- min_list(L, Y), findall(N,nth1(N,L,Y),C),random_member(Col,C),!.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% pre-existing code -------------------------------
%
getAI1Move(_, Move) :- random_between(1,7, Move).
getAI2Move(Board, 1) :- \+ isIllegal(Board, 1).
getAI2Move(Board, 2) :- \+ isIllegal(Board, 2).
getAI2Move(Board, 3) :- \+ isIllegal(Board, 3).
getAI2Move(Board, 4) :- \+ isIllegal(Board, 4).
getAI2Move(Board, 5) :- \+ isIllegal(Board, 5).
getAI2Move(Board, 6) :- \+ isIllegal(Board, 6).
getAI2Move(Board, 7) :- \+ isIllegal(Board, 7).

getAI3Move(Board, 1) :- \+ isIllegal(Board, 1), wouldWin(Board, 1, 2).
getAI3Move(Board, 2) :- \+ isIllegal(Board, 2), wouldWin(Board, 2, 2).
getAI3Move(Board, 3) :- \+ isIllegal(Board, 3), wouldWin(Board, 3, 2).
getAI3Move(Board, 4) :- \+ isIllegal(Board, 4), wouldWin(Board, 4, 2).
getAI3Move(Board, 5) :- \+ isIllegal(Board, 5), wouldWin(Board, 5, 2).
getAI3Move(Board, 6) :- \+ isIllegal(Board, 6), wouldWin(Board, 6, 2).
getAI3Move(Board, 7) :- \+ isIllegal(Board, 7), wouldWin(Board, 7, 2).

getAI3Move(Board, 1) :- \+ isIllegal(Board, 1), wouldWin(Board, 1, 1).
getAI3Move(Board, 2) :- \+ isIllegal(Board, 2), wouldWin(Board, 2, 1).
getAI3Move(Board, 3) :- \+ isIllegal(Board, 3), wouldWin(Board, 3, 1).
getAI3Move(Board, 4) :- \+ isIllegal(Board, 4), wouldWin(Board, 4, 1).
getAI3Move(Board, 5) :- \+ isIllegal(Board, 5), wouldWin(Board, 5, 1).
getAI3Move(Board, 6) :- \+ isIllegal(Board, 6), wouldWin(Board, 6, 1).
getAI3Move(Board, 7) :- \+ isIllegal(Board, 7), wouldWin(Board, 7, 1).
getAI3Move(_, Move) :- random_between(1,7, Move).



% true if Player dropping piece at DropCol would win
wouldWin(Board, DropCol, Player) :- dropToken(Board, Player, DropCol, NewBoard), getDropXY(Board, DropCol, OutDropXY), win(NewBoard, Player, OutDropXY).

getNextState(InitBoard, InitPlayer, DropCol, InitBoard, InitPlayer,(1,1)) :- isIllegal(InitBoard, DropCol),nl, write('Illegal move.'),nl.
getNextState(InitBoard, 1, DropCol, NewBoard, NewPlayer, OutDropXY) :- dropToken(InitBoard, 1, DropCol, NewBoard), getDropXY(InitBoard, DropCol, OutDropXY), NewPlayer = 2.
getNextState(InitBoard, 2, DropCol, NewBoard, NewPlayer, OutDropXY) :- dropToken(InitBoard, 2, DropCol, NewBoard), getDropXY(InitBoard, DropCol, OutDropXY), NewPlayer = 1.

win(Board, Player, Move) :- getRow(Board, Move, OutRow), fourInARow(OutRow, Player, 0).
win(Board, Player, Move) :- getCol(Board, Move, OutCol), fourInARow(OutCol, Player, 0).
win(Board, Player, Move) :- getDiagDown(Board, Move, OutDiag), fourInARow(OutDiag, Player, 0).
win(Board, Player, Move) :- getDiagUp(Board, Move, OutDiag), fourInARow(OutDiag, Player, 0).

%check if moves are illegal
isIllegal(_, DropCol) :- DropCol > 7.
isIllegal(_, DropCol) :- DropCol < 1.
isIllegal(Board, DropCol) :- getElem(Board, DropCol, 1, 1, 1, OutElem), dif(OutElem, 0).

% take a board, a player, and a column and return the board with the player's token dropped at the column

dropToken(Board, Player, DropCol, OutBoard) :- getDropXY(Board, DropCol, DropXY), setElem(Board, Player, DropXY, OutBoard).
%testDropToken(Player, Column) :- getBlankBoard(Board), dropToken(Board, Player, Column, OutBoard), drawBoard(OutBoard).

% take a board, a coordinate, and a value and set the value at the coordinate in the board and return the new board

setElem(Board, ToSet, (SetX, SetY), NewBoard) :- setElemHelper(Board, ToSet, (SetX, SetY), NewBoard, 1, 1).

setElemHelper([[_|R]|C], ToSet, (SetX, SetY), [[ToSet|R]|C], SetX, SetY).
%walk down the columns
setElemHelper([H|T], ToSet, (SetX, SetY), [H|T1], _,CurrY) :- CurrY < SetY, is(NextY, CurrY + 1), setElemHelper(T, ToSet, (SetX, SetY), T1, 1,NextY).
%walk down the rows
setElemHelper([[H|R]|C], ToSet, (SetX, SetY), [[H|R1]|C1], CurrX, SetY) :- CurrX < SetX, is(NextX, CurrX + 1), setElemHelper([R|C], ToSet, (SetX, SetY), [R1|C1], NextX, SetY).

%testSetElem(SetX, SetY, ToSet, OutBoard) :- getBlankBoard(Board), setElem(Board, ToSet, (SetX, SetY), OutBoard), drawBoard(OutBoard).

% take a board and a column and return the board coordinates of where it will drop

getDropXY(Board, DropCol, (DropCol, 6)) :- getElem(Board, DropCol, 6, 1, 1, OutElem), is(OutElem,0).
getDropXY(Board, DropCol, (DropCol, 5)) :- getElem(Board, DropCol, 5, 1, 1, OutElem), getElem(Board, DropCol, 6, 1, 1, OutElem1), is(OutElem,0), dif(OutElem1, 0).
getDropXY(Board, DropCol, (DropCol, 4)) :- getElem(Board, DropCol, 4, 1, 1, OutElem), getElem(Board, DropCol, 5, 1, 1, OutElem1), is(OutElem,0), dif(OutElem1, 0).
getDropXY(Board, DropCol, (DropCol, 3)) :- getElem(Board, DropCol, 3, 1, 1, OutElem), getElem(Board, DropCol, 4, 1, 1, OutElem1), is(OutElem,0), dif(OutElem1, 0).
getDropXY(Board, DropCol, (DropCol, 2)) :- getElem(Board, DropCol, 2, 1, 1, OutElem), getElem(Board, DropCol, 3, 1, 1, OutElem1), is(OutElem,0), dif(OutElem1, 0).
getDropXY(Board, DropCol, (DropCol, 1)) :- getElem(Board, DropCol, 1, 1, 1, OutElem), getElem(Board, DropCol, 2, 1, 1, OutElem1), is(OutElem,0), dif(OutElem1, 0).

%testGetDropXY(DropCol, (OutX, OutY)) :- getBlankBoard(Board), getDropXY(Board, DropCol, (OutX, OutY)).

% Detect if a list has 4 in a row of type Player

fourInARow(_,_,4).
fourInARow([H|T], H, Count) :- dif(Count, 4), is(NewCount, Count + 1), fourInARow(T, H, NewCount).
fourInARow([H|T], Player, Count) :- dif(Count, 4), dif(H, Player), fourInARow(T, Player, 0).

% get the row from position at Move from Board
getRow([H|_], (_,1), H).
getRow([_|T], (_, MoveY), OutRow) :- MoveY > 1, is(NextMoveY, MoveY - 1), getRow(T, (0,NextMoveY), OutRow).

% get the col from position at Move from Board
getCol(Board, (X,_), OutCol) :- getElem(Board, X, 1, 1, 1, Out1),
								getElem(Board, X, 2, 1, 1, Out2),
								getElem(Board, X, 3, 1, 1, Out3),
								getElem(Board, X, 4, 1, 1, Out4),
								getElem(Board, X, 5, 1, 1, Out5),
								getElem(Board, X, 6, 1, 1, Out6),
								OutCol = [Out1, Out2, Out3, Out4, Out5, Out6].

% get the DiagDown from position at Move from Board
getDiagDown(Board, (X, Y), Out) :- getUpLeftStartingPos((X,Y),(StartX,StartY)), getDiagDownHelper(Board, (StartX,StartY), Out).

getUpLeftStartingPos((X,Y),(StartX,StartY)) :- X < Y, is(StartX, 1), is(StartY, Y - X + 1).
getUpLeftStartingPos((X,Y),(StartX,StartY)) :- Y < X, is(StartX, X - Y + 1), is(StartY, 1).
getUpLeftStartingPos((A, A), (StartX, StartY)) :- StartX = 1, StartY = 1.

getDiagDownHelper(_, (8,_), []).
getDiagDownHelper(_, (_,7), []).
getDiagDownHelper(Board, (CurrX, CurrY), [H|T]) :-  CurrX < 8, CurrY < 7,
													is(NextX, CurrX + 1), is(NextY, CurrY + 1),
													getElem(Board, CurrX, CurrY, 1, 1, H),
												    getDiagDownHelper(Board, (NextX, NextY), T).

% get the DiagUp from position at Move from Board
getDiagUp(Board, (X, Y), Out) :- getDownLeftStartingPos((X,Y),(StartX,StartY)), getDiagUpHelper(Board, (StartX,StartY), Out).

getDownLeftStartingPos((X,Y),(StartX,StartY)) :- is(X1,-X + 7), Y > X1, is(StartX, X-6+Y), is(StartY, 6).
getDownLeftStartingPos((X,Y),(StartX,StartY)) :- is(X1,-X + 7), Y < X1, is(StartX, 1), is(StartY, Y + X - 1).
getDownLeftStartingPos((X,Y),(StartX,StartY)) :- is(X1,-X + 7), is(Y, X1), StartX = 1, StartY = 6.



%found the element
getElem([[H|_]|_], X, Y, X, Y, H).

%walk down the rows if not on the right one
getElem([[_|_]|R], X, Y,_, CurrY, Out) :- dif(CurrY, Y), is(NextY, CurrY + 1), getElem(R, X, Y, 1, NextY, Out).

%walk down the columns to find the right one
getElem([[_|C]|R], X, Y, CurrX, Y, Out) :- dif(CurrX, X), is(NextX, CurrX + 1), getElem([C|R], X, Y, NextX, Y, Out).

%detect if board if full
isFull(Board):- isIllegal(Board, 1), isIllegal(Board, 2), isIllegal(Board, 3), isIllegal(Board, 4), isIllegal(Board, 5), isIllegal(Board, 6), isIllegal(Board, 7).

getBlankBoard(Board) :- Board =    [[0,0,0,0,0,0,0],
									[0,0,0,0,0,0,0],
									[0,0,0,0,0,0,0],
									[0,0,0,0,0,0,0],
									[0,0,0,0,0,0,0],
									[0,0,0,0,0,0,0]].


drawBoard1([]).
drawBoard1([H|T]) :- drawBoardRow(H),nl,drawBoard1(T).
drawBoardRow([]).
drawBoardRow([2|T]) :- ansi_format([bold,fg(red)], 'O ~w', [' ']), drawBoardRow(T).
drawBoardRow([1|T]) :- ansi_format([bold,fg(yellow)], 'O ~w', [' ']), drawBoardRow(T).
drawBoardRow([0|T]) :- ansi_format([bold,fg(black)], 'O ~w', [' ']), drawBoardRow(T).
drawBoard(Board) :- ansi_format([bold,fg(black)], '1  2  3  4  5  6  7 ~w', [' ']),nl,nl, drawBoard1(Board).

drawBlankBoard :- getBlankBoard(Board), drawBoard(Board).
endmessage :- write('Enter \'play.\' to play a 2 player game or \'playAI[1-3].\' to play against the computer.').

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% GUI

:- use_module(library(pce)). % loads xpce lib
:- pce_begin_class(connect4, dialog, "Connect 4 Game").

initialise(W, Turn:string) :->
        "Initialise the window and fill it"::
        send_super(W, initialise("Game")),
send(W, append(text("Player Turn:"),right)),
send(W, append(text(Turn), right)),
send(W, append(text("Enter the row number here:"))),
send(W, append(text_item(number))),
send(W, append(button(ok))),
send(W, default_button(ok)).


ok(W) :->
        "User pressed the OK button"::
        get(W, member(number), NumberItem),
        get(NumberItem, selection, Typed),
        send(W, return, Typed).

prompt(W, Value:name) :<-
        "Open it, destroy it and return the result"::
        get(W, confirm, Value),
        free(W).

:- pce_end_class.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



