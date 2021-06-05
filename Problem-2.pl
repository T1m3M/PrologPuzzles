%grid(4,3).

%dot('G', [3,1]).
%dot('G', [2,2]).

append([], X, X).
append([X|Y], Z, [X|W]):- append(Y, Z, W).

makeGrid(0,R,R):-!.
makeGrid(A, G, R):-
	append(G, ['-'], X),
	A1 is A - 1,
	makeGrid(A1, X, R).

grid(W, H, R):-
	L1 is W*H - 1, 
	makeGrid(L1, ['-'], R).

% printing with style
printGrid(GW,GH,C,[]):-!.
printGrid(GW,GH,C,[H|T]):-
	write(H), write('  '),
	A is C + 1,
	((A mod GW =:= 0)
		-> nl, printGrid(GW,GH,A,T)
		;  printGrid(GW,GH,A,T)
	).


% reversing list
rev(L, R):- rev(L, R, []).

rev([], L, L).
rev([H|T], L, Tmp):-
rev(T, L, [H|Tmp]).

% spliting interface
splt(L, N, R1, R2):-
splt_(L, N, L1, R2),
rev(L1, R1).

% splitting function
splt_(L, 0, [], L).

splt_([H|T], N, L1, L2):-
NewN is N - 1,
splt_(T, NewN, LR, L2),
append(LR, [H], L1).

% add a dot
dot(L,P,W,G,G2):-
	getGIndex(P,W,M),
	splt(G, M, L1, [H|T]),
	append(L1,[L],G1),
	append(G1,T,G2).

dotIndex(L,I,W,G,G2):-
	splt(G, I, L1, [H|T]),
	append(L1,[L],G1),
	append(G1,T,G2).

getGIndex(P,W,R):-
	nth0(0,P,PH), nth0(1,P,PW),
	R is W*PH+PW.

indexOfG([Element|_], Element, 0).
indexOfG([_|Tail], Element, Index):-
  indexOfG(Tail, Element, Index1),
  Index is Index1+1.


% test go(4,3,[3,1],[2,2]).
go(GH,GW,A,B):-
	grid(GW,GH,G),
	dot('G',A,GW,G,Goal),
	dot('G',B,GW,G,Start),
	dot('G',B,GW,Goal,All),

	write("Game:"), nl,
	printGrid(GW,GH,0,All), nl,

	% solution steps using A*
	getHeuristic(Start, H, Goal),
	path([[Start,null, 0, H, H]],[],Goal,GW,GH,G).%open, closed, goal, path_cost, heuristic, total cost


getHeuristic([], 0, []):-!.
getHeuristic([H|T1],V,[H|T2]):-!,
	getHeuristic(T1,V, T2).

getHeuristic([_|T1],H,[_|T2]):-!,
	H is 1.


getBestChild([Child], Child, []).
getBestChild(Open, Best, RestOpen):-
	getBestChild1(Open, Best),
	removeFromList(Best, Open, RestOpen).

getBestChild1([State], State):-!.
getBestChild1([State|Rest], Best):-
	getBestChild1(Rest, Temp),
	getBest(State, Temp, Best).

getBest([State, Parent, PC, H, TC], [_, _, _, _, TC1], [State, Parent, PC, H, TC]):-
	TC < TC1, !.
getBest([_, _, _, _, _], [State1, Parent1, PC1, H1, TC1], [State1, Parent1, PC1, H1, TC1]).


getchildren(State, Open ,Closed , Children, PC, Goal,GW,GH,EmptyG):-
		bagof(X, moves( State, Open, Closed, X, PC, Goal,GW,GH,EmptyG), Children).
getchildren(_,_,_, [],_,_,GW,GH,EmptyG).


moves(State, Open, Closed,[Next,State, NPC, H, TC], PC, Goal,GW,GH,EmptyG):-
		indexOfG(State,'G',I),
		move(State,Next,I,GW,GH,EmptyG),
		\+ member([Next, _, _, _, _],Open),
		\+ member([Next, _, _, _, _],Closed),
		NPC is PC + 1,
		getHeuristic(Next, H, Goal),
		TC is NPC + H.

move(S,Snew,I,GW,GH,EmptyG):-
  	right(S,Snew,I,GW,EmptyG).

right(S,Snew,I,GW,EmptyG):-
  	\+(I mod GW =:= 2),
  	NewI is I + 1,
	dotIndex('G',NewI,GW,EmptyG,Snew).

move(S,Snew,I,GW,GH,EmptyG):-
  	left(S,Snew,I,GW,EmptyG).

left(S,Snew,I,GW,EmptyG):-
  	\+(I mod GW = 0),
  	NewI is I - 1,
	dotIndex('G',NewI,GW,EmptyG,Snew).

move(S,Snew,I,GW,GH,EmptyG):-
  	down(S,Snew,I,GW,GH,EmptyG).

down(S,Snew,I,GW,GH,EmptyG):-
  	I < (GW*GH)-GW,
  	NewI is I + GW,
	dotIndex('G',NewI,GW,EmptyG,Snew).

move(S,Snew,I,GW,GH,EmptyG):-
  	up(S,Snew,I,GW,GH,EmptyG).

up(S,Snew,I,GW,GH,EmptyG):-
  	\+(I < GW),
  	NewI is I - GW,
	dotIndex('G',NewI,GW,EmptyG,Snew).

path([], _, _,GW,GH,EmptyG):-
		write('No solution'),nl,!.
path(Open, Closed, Goal,GW,GH,EmptyG):-
		getBestChild(Open, [Goal, Parent, PC, H, TC], RestOfOpen),
		write('Steps:'),  nl ,
		printsolution([Goal,Parent, PC, H, TC],GW,GH, Closed),!.
path(Open, Closed, Goal,GW,GH,EmptyG):-
		getBestChild(Open, [State, Parent, PC, H, TC], RestOfOpen),
		%write('Best child chosen is '),write(State),write(' with TC= '),write(TC), nl,
		getchildren(State, Open, Closed, Children, PC, Goal,GW,GH,EmptyG),
		addListToOpen(Children , RestOfOpen, NewOpen),
		path(NewOpen, [[State, Parent, PC, H, TC] | Closed], Goal,GW,GH,EmptyG).


addListToOpen(Children, [], Children).
addListToOpen(Children, [H|Open], [H|NewOpen]):-
		addListToOpen(Children, Open, NewOpen).

removeFromList(_, [], []):-!.
removeFromList(H, [H|T], V):-
	!, removeFromList(H, T, V).
removeFromList(H, [H1|T], [H1|T1]):-
	removeFromList(H, T, T1).

printsolution([State, null, PC, H, TC],GW,GH,_):-!,
		printGrid(GW,GH,0,State),nl.
printsolution([State, Parent, PC, H, TC],GW,GH, Closed):-
		member([Parent, GrandParent, PC1, H1, TC1], Closed),
		printsolution([Parent, GrandParent, PC1, H1, TC1],GW,GH, Closed),
		printGrid(GW,GH,0,State),nl.
