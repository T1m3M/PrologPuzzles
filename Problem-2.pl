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
	write(H), write(' '),
	A is C + 1,
	((A mod GW =:= 0)
		-> nl, printGrid(GW,GH,A,T)
		;  printGrid(GW,GH,A,T)
	).

% test go(4,3,[3,1],[2,2]).
go(GW,GH,A,B):-
	grid(GW,GH,G),
	write("Game:"), nl,
	printGrid(GW,GH,0,G).