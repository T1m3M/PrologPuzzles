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
dot(L,[],G,G2):-!.
dot(L,[H|T],G,G2):-
	splt([1, 2, 3, 4, 5], H, L1, L2).

% test go(4,3,[3,1],[2,2]).
go(GH,GW,A,B):-
	grid(GW,GH,G),
	write("Game:"), nl,
	printGrid(GW,GH,0,G).
	%dot('G', A, G, NewG).