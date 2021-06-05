%grid(5,5).

%dot('R',[3,1]).
%dot('R',[0,3]).
%dot('B',[4,1]).
%dot('B',[0,0]).
%dot('G',[4,2]).
%dot('G',[3,4]).
%dot('O',[3,2]).
%dot('O',[0,4]).
%dot('Y',[1,3]).
%dot('Y',[2,2]).


% Grid = 5*5
test([["B","_","_","R","O"],
      ["_","_","_","Y","_"],
      ["_","_","Y","_","_"],
      ["_","R","O","_","G"],
      ["_","B","G","_","_"]]).

test2([["_","_","_","_","_"],
      ["_","_","_","_","_"],
      ["_","_","_","_","_"],
      ["_","_","_","_","_"],
      ["_","_","_","_","_"]]).


testSolution([["B","_","_","R","O"],
              ["_","_","_","Y","_"],
              ["_","_","Y","_","_"],
              ["_","R","O","_","G"],
              ["_","B","G","_","_"]]).






go(Start):-
   dfs([Start],[]).

flowFree():-
   test(Y),
   dfs([Y],[]).

isGoal(Grid):-
   not(getFirstEmpty(Grid,0,_,_)).

printGrid([]):-!.
printGrid([H|T]):-
   printRow(H),
   printCol(T),
   printGrid(T).


printRow([]):-
   put(5),!.
printRow([H|T]):-
   write(H),
   write(' '),
   write(' '),
   printRow(T).

printCol([]):-
   put(5),!.
printCol([H|T]):-
   write(H),
   write(' '),
   write(' '),
   printCol(T).


%dfs to get the valid board
dfs([],_):-
    write('no Solution'),!.

dfs([State|_],_):-
    isGoal(State),
    printGrid(State),
    write('We Reached the goal').

dfs([State|T],Closed):-
    getValidChildren(State,[State|T],Closed,Children),
    append(Children,T,NewOpen),
    dfs(NewOpen,[State|Closed]).


getValidChildren(State, Open ,Closed , Children):-
		bagof(X,moves(State, X,Open, Closed), Children), ! .
getValidChildren(_,_,_,[]).

%moves( State, Open, Closed,[Next,State])
%Movments of the grid facts
move(S,Snew):-
	up(S,Snew).
up([R1,R2,R3,R4,R5|T],Snew):-
	move_up([R1,R2,R3,R4,R5|T],Snew).
move_up(X,S):-
% get position of coler cell
	nth0(N,X,'R'),
	Z is N-5,
% get element in pos Z
        (   Z > 5 ->
	nth0(Z,X,R),
        substitute(R,X,R,Q)).

move(S,Snew):-
	down(S,Snew).
down(X,Snew):-
	move_down(X,Snew).
move_down(X,S):-
	nth0(N,X,'R'),
	Z is N+3,
	nth0(Z,X,R),
	substitute(R,X,'R',Q).

move(S,Snew):-
	left(S,Snew).

left(X,Snew):-
	move_left(X,Snew).

move_left(X,S):-
	nth0(N,X,'R'),
	Z is N-1,
	nth(Z,X,R),
	substitute(R,X,'R',Q).

move(S,Snew):-
	right(S,Snew).

right(X,Snew):-
	move_right(X,Snew).

move_right(X,S):-
	nth0(N,X,'R'),
	Z is N+1,
	nth0(Z,X,R),
	substitute(R,X,'R',Q).





indexOfG([Element|_], Element, 0).
indexOfG([_|Tail], Element, Index):-
  indexOfG(Tail, Element, Index1),
  Index is Index1+1.

moves(State,[Next|State],Open,Closed):-
    indexOfG(State,'B',I),
    move(State,Next,I),
    \+ member([Next,_],Open),
    \+ member([Next,_],Closed).



getFirstEmpty([First|_],Pos,Col,Pos):-
   nth0(Col,First,0),!.
getFirstEmpty([_|Rest],Pos,Col,Row):-
   NPos is Pos+1,
   getFirstEmpty(Rest,NPos,Col,Row).
