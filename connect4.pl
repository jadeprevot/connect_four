%Connect Four
% CS 312 Project
% Enter 'play.' to play the 2-player game.
% Enter 'playAI1.' to play the game versus the level 1 computer.

% play the game vs level 1 AI ----------------------------------------------------------------------------
playAI :- tutorial, getBlankBoard(Board), nextPlayAI(Board, 1).

nextPlayAI(Board, 1) :- nl,write('It is your turn.'),nl,nl,
					  getAI3Move(Board, Move),
					  nl,write('Yellow dropped piece into column '),write(Move),nl,nl,
					  getNextState(Board, 1, Move, NewBoard, NewPlayer, OutDropXY),
					  drawBoard(NewBoard),

					  nextStateAI(NewBoard, NewPlayer, OutDropXY).

nextPlayAI(Board, 2) :- nl,write('Computer is making move...'),nl,nl,
					  drawBoard(Board),
					  getAI1Move(Board, Move),
					  nl,write('Red dropped piece into column '),write(Move),nl,nl,

					  getNextState(Board, 2, Move, NewBoard, NewPlayer, OutDropXY),
					  drawBoard(NewBoard),

					  nextStateAI(NewBoard, NewPlayer, OutDropXY).

nextStateAI(Board, 2, DropXY) :- win(Board, 1, DropXY),nl, write('Yellow wins!'),nl, endmessage.
nextStateAI(Board, 1, DropXY) :- win(Board, 2, DropXY),nl, write('Red wins!'),nl, endmessage.
nextStateAI(Board,_,_) :- isFull(Board),nl, write('It\'s a tie!'),nl, endmessage.
nextStateAI(Board, NewPlayer,_) :- nextPlayAI(Board, NewPlayer).

getAI1Move(_, Move) :- random_between(1,7, Move).

% play the game vs level 2 AI ----------------------------------------------------------------------------
playAI2 :- tutorial, getBlankBoard(Board), nextPlayAI2(Board, 1).

nextPlayAI2(Board, 1) :- nl,write('It is your turn.'),nl,nl,
					  drawBoard(Board),
					  read(Move),
					  getNextState(Board, 1, Move, NewBoard, NewPlayer, OutDropXY),
					  nextStateAI2(NewBoard, NewPlayer, OutDropXY).

nextPlayAI2(Board, 2) :- nl,write('Computer is making move...'),nl,nl,
					  drawBoard(Board),
					  getAI2Move(Board, Move),
					  nl,write('Computer dropped piece into column '),write(Move),nl,nl,
					  getNextState(Board, 2, Move, NewBoard, NewPlayer, OutDropXY),
					  nextStateAI2(NewBoard, NewPlayer, OutDropXY).

nextStateAI2(Board, 2, DropXY) :- win(Board, 1, DropXY),nl, write('You win!'),nl, endmessage.
nextStateAI2(Board, 1, DropXY) :- win(Board, 2, DropXY),nl, write('The computer wins!'),nl, endmessage.
nextStateAI2(Board,_,_) :- isFull(Board),nl, write('It\'s a tie!'),nl, endmessage.
nextStateAI2(Board, NewPlayer,_) :- nextPlayAI2(Board, NewPlayer).

getAI2Move(Board, 1) :- \+ isIllegal(Board, 1).
getAI2Move(Board, 2) :- \+ isIllegal(Board, 2).
getAI2Move(Board, 3) :- \+ isIllegal(Board, 3).
getAI2Move(Board, 4) :- \+ isIllegal(Board, 4).
getAI2Move(Board, 5) :- \+ isIllegal(Board, 5).
getAI2Move(Board, 6) :- \+ isIllegal(Board, 6).
getAI2Move(Board, 7) :- \+ isIllegal(Board, 7).

% play the game vs level 3 AI ----------------------------------------------------------------------------
playAI3 :- tutorial, getBlankBoard(Board), nextPlayAI3(Board, 1).

nextPlayAI3(Board, 1) :- nl,write('It is your turn.'),nl,nl,
					  drawBoard(Board),
					  read(Move),
					  getNextState(Board, 1, Move, NewBoard, NewPlayer, OutDropXY),
					  nextStateAI3(NewBoard, NewPlayer, OutDropXY).

nextPlayAI3(Board, 2) :- nl,write('Computer is making move...'),nl,nl,
					  drawBoard(Board),
					  getAI3Move(Board, Move),
					  nl,write('Computer dropped piece into column '),write(Move),nl,nl,
					  getNextState(Board, 2, Move, NewBoard, NewPlayer, OutDropXY),
					  nextStateAI3(NewBoard, NewPlayer, OutDropXY).

nextStateAI3(Board, 2, DropXY) :- win(Board, 1, DropXY),nl, write('You win!'),nl, endmessage.
nextStateAI3(Board, 1, DropXY) :- win(Board, 2, DropXY),nl, write('The computer wins!'),nl, endmessage.
nextStateAI3(Board,_,_) :- isFull(Board),nl, write('It\'s a tie!'),nl, endmessage.
nextStateAI3(Board, NewPlayer,_) :- nextPlayAI3(Board, NewPlayer).

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

% play the game in 2 player mode ------------------------------------------------------------------------
play :- tutorial, getBlankBoard(Board), nextPlay(Board, 1).

nextPlay(Board, 1) :- %whosturn(1),
			    drawBoard(Board),
			    get(connect4(1), prompt, X),
			    atom_number(X, Move),
			    getNextState(Board, 1, Move, NewBoard, NewPlayer, OutDropXY),
					  nextState(NewBoard, NewPlayer, OutDropXY).

nextPlay(Board, 2) :- %whosturn(2),
			    drawBoard(Board),
			    get(connect4(2), prompt, X),
			    atom_number(X, Move),
					  getNextState(Board, 2, Move, NewBoard, NewPlayer, OutDropXY),
					  nextState(NewBoard, NewPlayer, OutDropXY).

getNextState(InitBoard, InitPlayer, DropCol, InitBoard, InitPlayer,(1,1)) :- isIllegal(InitBoard, DropCol),nl, write('Illegal move.'),nl.
getNextState(InitBoard, 1, DropCol, NewBoard, NewPlayer, OutDropXY) :- dropToken(InitBoard, 1, DropCol, NewBoard), getDropXY(InitBoard, DropCol, OutDropXY), NewPlayer = 2.
getNextState(InitBoard, 2, DropCol, NewBoard, NewPlayer, OutDropXY) :- dropToken(InitBoard, 2, DropCol, NewBoard), getDropXY(InitBoard, DropCol, OutDropXY), NewPlayer = 1.

nextState(Board, 2, DropXY) :- win(Board, 1, DropXY),nl, write('Player '),write(1),write(' wins!'),nl, endmessage.
nextState(Board, 1, DropXY) :- win(Board, 2, DropXY),nl, write('Player '),write(2),write(' wins!'),nl, endmessage.
nextState(Board,_,_) :- isFull(Board),nl, write('It\'s a tie!'),nl, endmessage.
nextState(Board, NewPlayer,_) :- nextPlay(Board, NewPlayer).

win(Board, Player, Move) :- getRow(Board, Move, OutRow), fourInARow(OutRow, Player, 0).
win(Board, Player, Move) :- getCol(Board, Move, OutCol), fourInARow(OutCol, Player, 0).
win(Board, Player, Move) :- getDiagDown(Board, Move, OutDiag), fourInARow(OutDiag, Player, 0).
win(Board, Player, Move) :- getDiagUp(Board, Move, OutDiag), fourInARow(OutDiag, Player, 0).

%check if moves are illegal
isIllegal(_, DropCol) :- DropCol > 7.
isIllegal(_, DropCol) :- DropCol < 1.
isIllegal(Board, DropCol) :- getElem(Board, DropCol, 1, 1, 1, OutElem), dif(OutElem, 0).

% say whos turn it is
whosturn(Player) :- nl,nl,write('It is player '), write(Player), write('\'s turn.'),nl,nl.

%print the tutorial
tutorial :- write('Play by entering an integer position of the column to drop your piece into.'),nl,nl.

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

getDownLeftStartingPos((X,Y),(StartX,StartY)) :- is(X1,-X + 7), Y > X1, is(StartX, 6 - X1 + 1), is(StartY, 6).
getDownLeftStartingPos((X,Y),(StartX,StartY)) :- is(X1,-X + 7), Y < X1, is(StartX, 1), is(StartY, Y + X - 1).
getDownLeftStartingPos((X,Y),(StartX,StartY)) :- is(X1,-X + 7), is(Y, X1), StartX = 1, StartY = 6.

getDiagUpHelper(_, (8,_), []).
getDiagUpHelper(_, (_,0), []).
getDiagUpHelper(Board, (CurrX, CurrY), [H|T]) :-  CurrX < 8, CurrY > 0,
													is(NextX, CurrX + 1), is(NextY, CurrY - 1),
													getElem(Board, CurrX, CurrY, 1, 1, H),
												    getDiagUpHelper(Board, (NextX, NextY), T).

%testGetRow(Out) :- getBlankBoard(Board), getRow(Board, (2,3), Out).
%testGetCol(Out) :- getBlankBoard(Board), getCol(Board, (5,3), Out).
%testGetDiagDown(Out) :- getBlankBoard(Board), getDiagDown(Board, (7,1), Out).
%testGetDiagUp((InX, InY), Out) :- getBlankBoard(Board), getDiagUp(Board, (InX,InY), Out).
%testGetStart((InX, InY), (StartX, StartY)) :- getDownLeftStartingPos((InX, InY), (StartX, StartY)).

% get element of Board at X and Y
% Board is a list of Rows
%getElem(Board, X, Y, CurrX, CurrY, Out).

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
drawBoard(Board) :- ansi_format([bold,fg(black)], '1   2   3   4   5   6   7 ~w', [' ']),nl,nl, drawBoard1(Board).

drawBlankBoard :- getBlankBoard(Board), drawBoard(Board).
endmessage :- write('Enter \'play.\' to play a 2 player game or \'playAI[1-3].\' to play against the computer.').

%testGetElem(Out) :- getBlankBoard(Board), getElem(Board, 2, 4, 1, 1, Out).

% ----------------------------------------------------------------------------
% Code par H4114
% Return the Score when player plays in the col
calcScore(Board,Col,Player,Score):-getDropXY(Board,Col,(X,Y)),calcScore(Board,X,Y,Player,Score).
calcScore(Board,X,Y,Player,Score):-getRow(Board,(X,Y),Row),countLineScore(Row,X,Score1,Player),
			           getCol(Board,(X,Y),Col),countLineScore(Col,Y,Score2,Player),
				   posDiagDown(X,Y,I1),getDiagDown(Board,(X,Y),DiagDown),countLineScore(DiagDown,I1,Score3,Player),
				   posDiagUp(X,Y,I2),calcPosBug(X,Y,X2,Y2),getDiagUp(Board,(X2,Y2),DiagUp),countLineScore(DiagUp,I2,Score4,Player),
				   Score is Score1+Score2+Score3+Score4.

%I est la position dans la diagonale du point de coordonn�es X Y
posDiagDown(X,Y,I):-I is min(X,Y).
posDiagUp(X,Y,I):-I is min(7-Y,X).

% La clause deja implement� pour avoir les digonale montante ne marche
% pas toujours, cette clause calcule une nouvelle paire de coordonn�es
% qui marchent.
calcPosBug(X,Y,X,Y):-(X-(6-Y))<1.
calcPosBug(X,Y,X2,6):-X2 is X-(6-Y).

% Pour un ligne et une position, Score est le nombre d'element Player
% dans les 3 positions precedentes et 3 positions suivantes
%(On evalue une plage de 3 autour de la position initiale)
countLineScore(L,PosX,Score,Player):-length(L,Taille),countLineScore(L,PosX,1,Score,Player,Taille).
countLineScore(_,_,CurrentX,0,_,Taille):-CurrentX is Taille+1.
countLineScore(_,PosX,CurrentX,0,_,_):- CurrentX is PosX+4.
countLineScore([F|R],PosX,CurrentX,Score,Player,Taille):- PosX-CurrentX<4, F==Player, is(NextX, CurrentX+1), countLineScore(R,PosX,NextX,NextScore,Player,Taille),Score is NextScore+1.
countLineScore([_|R],PosX,CurrentX,Score,Player,Taille):-is(NextX, CurrentX+1),countLineScore(R,PosX,NextX,Score,Player,Taille).

%%% Annie's test
% The max value of sets of 4 a piece can be in
getMaxList(X) :- X = [[3,4,5,7,5,4,3],
				[4,6,8,10,8,6,4],
				[5,8,11,13,11,8,5],
				[5,8,11,13,11,8,5],
				[4,6,8,13,8,6,4],
				[3,4,5,7,5,4,3]].

			
				
% get value of a matrix	
cellVal([], _, []).
cellVal([[R, C]| L], X, [Y|Z]) :-
    nth0(R, X, Row),
    nth0(C, Row, Y),
    cellVal(L, X, Z).

%%%


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
