% Author: Thomas Haddy 5/3/19

%--------------------------------Database---------------------------

% facts on attr1
attr1(black, red).
attr1(black, blue).
attr1(black, white).
attr1(blue, red).
attr1(white, blue).
attr1(white, red).

% facts on attr2
attr2(electric, X) :- X = four; X = six.
attr2(hybrid, X) :- X = four; X = six.
attr2(four, six).

% facts on attr3
attr3(tesla, X) :- X = skoda; X = alfa; X = bmw.
attr3(bmw, X) :- X = skoda; X = alfa.
attr3(alfa, skoda).

attr4(a, b).
attr4(b, c).
ttr4(c, d).

% The choices
carlist([ 
	(red, electric, tesla),
	(black, hybrid, bmw),
	(blue, electric, bmw),
	(red, hybrid, bmw),
	(red, four, alfa),
	(black, four, alfa),
	(black, electric, skoda)
]).

%---------------------------End Database----------------------------

% Get the preference between two values of an attribute whether its
% directly related or indirectly
% Ex: pref1(black, red). 	--> true
% Ex: pref3(alfa, tesla). 	--> false 
pref1(A1, B1) :- attr1(A1, B1).
pref1(A1, B1) :- attr1(A1, C1), pref1(C1, B1).
pref2(A2, B2) :- attr2(A2, B2).
pref2(A2, B2) :- attr2(A2, C2), pref2(C2, B2).
pref3(A3, B3) :- attr3(A3, B3).
pref3(A3, B3) :- attr3(A3, C3), pref3(C3, B3).

% Get the preference or identity between two values of an attribute
% Ex: identicalOrPreferred1(black, black).	--> true
% Ex: identicalOrPreferred1(black, red).	--> true
% Ex: identicalOrPreferred3(bmw, tesla).	--> false
identicalOrPreferred1(A1, B1) :- A1 == B1.
identicalOrPreferred1(A1, B1) :- pref1(A1, B1).
identicalOrPreferred2(A2, B2) :- A2 == B2.
identicalOrPreferred2(A2, B2) :- pref2(A2, B2).
identicalOrPreferred3(A3, B3) :- A3 == B3.
identicalOrPreferred3(A3, B3) :- pref3(A3, B3).

% Gets the preferred choice between one choice and another
% Ex: preferredChoice((black, hybrid, bmw), (red, four, alfa)).
% 		--> true
% Ex: preferredChoice((black, hybrid, bmw), (black, electric, skoda)).
% 		--> false
preferredChoice((A1, A2, A3), (B1, B2, B3)) :- 
	identicalOrPreferred1(A1, B1),
	identicalOrPreferred2(A2, B2),
	identicalOrPreferred3(A3, B3),
		(pref1(A1, B1);
		 pref2(A2, B2);
		 pref3(A3, B3))
.

% Gets the choices that are not in level 0 of preferences 
% (level 0 = Most preferred)
% Ex: choicesNotInLevel_0(ChoiceX, carlist(L)).
choicesNotInLevel_0(ChoiceX, L) :- 
    member(ChoiceX, L),
    member(ChoiceY, L),
    (ChoiceX \= ChoiceY),
    preferredChoice(ChoiceY, ChoiceX).

% Gets the choices that are in level 0 of preferences 
% (level 0 = Most preferred)
% Ex: choicesInLevel_0(ChoiceX, carlist(L)).
choicesInLevel_0(X, L) :- 
    member(X, L),
    not(choicesNotInLevel_0(X, L)).

% Constructs a list that contains the top level of preferences
% Ex: carlist(L), getTopLevelChoices(X, L).
getTopLevelChoices(L, All) :-
    findall(ChoiceX, choicesInLevel_0(ChoiceX, All), L).

% Gets the difference between two lists L1 and L2 and returns Difference as
% the resulting list
% Ex: getListDifference(([1, 2, 3]), ([1]), Difference).
% 	--> Difference = [2, 3]
getListDifference(L1, L2, Difference) :-
    findall(X, (member(X, L1), not(member(X, L2))), Difference).

% constructs the level given a list of choices
% weakorder will call this until the given list of choices are empty
% Ex: carlist(L), constructLevels(L, Result).
constructLevel(_, []) :- !.
constructLevel(L, ALL) :-
    getTopLevelChoices(TOPLEVEL, ALL),
    getListDifference(ALL, TOPLEVEL, DIFFERENCE),
    ((L = TOPLEVEL) ; constructLevel(L, DIFFERENCE)),
    subset(L, ALL),
    L \= []
.

% Constructs a list containing the most and least preferred choices
% Ex: carlist(L), weakorder(L, Result).
weakorder(L, Result) :- findall(LEVELX, (constructLevel(LEVELX, L)), Result).