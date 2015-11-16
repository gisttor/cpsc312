% Load ProNTo_Morph module
:- ensure_loaded('Schlachter/pronto_morph_engine').
:- ensure_loaded('312-pess-grammar.pl').
:- ensure_loaded('312-pess.pl').

% Import the g&s files.
:- ensure_loaded('wn_s').
:- ensure_loaded('wn_g').

% (q6 main function) if you input a word into the program, (1) it will first produce a list of all the stems. 
% The second step is that it will go through the list of stems and produce a list of 
% stems with all their parts of speech. In the last step, I will just have to assert 
% everything in the list.

addword(_word,_listofstemswithpartofspeech):-
allstems(_word,_stemlist),
allstemswithpartofspeech(_stemlist,_listofstemswithpartofspeech).


% (step1.main) get all the possible stems from a word without extra parts.
allstems(W,L2):-
morph_atoms_bag(W, L),
stem_helper(L,L2).

% (helper) remove extra parts from the the possible stems.
% ex. [[[testing]],[[test, -ing]],[teste, -ing]] --> [testing, test, teste]
stem_helper([],[]).
stem_helper([[[X]|[]]|T], [X|L2]):-
stem_helper(T,L2),!.
stem_helper([[[X,Y]|[]]|T], [X|L2]):-
stem_helper(T,L2),!.


% (helper) get every possible part of speech for a word as a list from wordnet
getpartofspeech(X, Y):- 
findall(_partofspeech, s(_,_,X,_partofspeech,_,_), Y).

% (helper) making a list of the word with all its parts of speech in the form of partofspeech(word).
parse_list(W,[],[]).
parse_list(W,[H|T],[A|T2]):-
build_vocab(W, H, A).
parse_list(W,T,T2),!.

% convert word and its part of speech into the form partofspeech(X).
build_vocab(X, n, n(X)).
build_vocab(X, v, v(X)).
build_vocab(X, adj, adj(X)).
build_vocab(X, s, adj(X)).
build_vocab(X, adv, adv(X)).

% (helper) converts arg X,Y into a functor X(Y)
parse_xy(X,Y,A) :- functor(A,X,1), arg(1,A,Y).

% (main) build a list of same stem with different parts of speech.
build_vocab_list(W, L):-
getpartofspeech(W, Y),
parse_list(W,Y,L).

% (helper) appends lists
append([],L,L). 
append([H|T],L2,[H|L3])  :-  append(T,L2,L3).

% (step2.main) all the stems with all their parts of speech as a list
% ex. [testing, teste, test] --> [n(testing), n(teste), v(teste), n(test), v(test)]
allstemswithpartofspeech([],[]).
allstemswithpartofspeech([H|T],L2):-
build_vocab_list(H, L),
append(L,Acc,L2),
allstemswithpartofspeech(T,Acc),!.

% (step3. main) asserting all the elements of the list to expand the vocabulary
assert_all([H|T]):-
assert(H),
assert_all(T),!.



