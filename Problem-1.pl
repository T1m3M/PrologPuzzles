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

  %moves(State,Next,Open,Closed):-
   % Mohamed Khaled = bro (:
   % work in this Function.



getFirstEmpty([First|_],Pos,Col,Pos):-
   nth0(Col,First,0),!.
getFirstEmpty([_|Rest],Pos,Col,Row):-
   NPos is Pos+1,
   getFirstEmpty(Rest,NPos,Col,Row).


