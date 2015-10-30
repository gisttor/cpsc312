/* Team information
Student #1, Name: Kelvin Yip
Student #1, Student #: 18016121
Student #1, ugrad ID: s8u8

Student #2, Name: Gisli Thor Thordarson
Student #2, Student #: 73996150
Student #2, ugrad ID: m8j0b

Student #3, Name: Chun-Wei Yen
Student #3, Student #: 33425125
Student #3, ugrad ID: s2u9a
*/

/**
*   Question 1
*/
% Import the g&s files.
:- ensure_loaded('wn_s').
:- ensure_loaded('wn_g').

% Joins the synset information with the gloss by synset_ID. All the definitons
% of S will be given as outputs. Works the same if you specified what G is and
% want to find out its word.
definition(S, G) :- s(_id,_,S,_,_,_), g(_id, G). 

/*
Output:
?- definition('hello', G).
G = 'an expression of greeting; "every morning they exchanged polite hellos"'.

?- definition('discipula', G).
false.

?- definition('hypernym', G).
G = 'a word that is more generic than a given word'.

?- definition(S, 'a plant or animal that is atypically small').
S = dwarf ;
false.

?- definition('nutrient', G).
G = 'any substance that can be metabolized by an animal to give energy and build tissue' ;
G = 'any substance (such as a chemical element or inorganic compound) that can be taken in by a green plant and used in organic synthesis' ;
G = 'of or providing nourishment; "good nourishing stew"'.
*/

/**
*   Question 2
*/

% Load ProNTo_Morph module
:- ensure_loaded('Schlachter/pronto_morph_engine').

/**
* Read a word and output the complete ProNTo_Morph parsing.
* Precondition: 
* Input is a single word and terminated by a period.
*
*/
word_line_morphs :- 
    read(X),                % read input word from the user
    morph_atoms_bag(X,Y),   % call the PronTO_Morph moduule
    write(Y).               % write out the definition

/* Sample Output
*

?- word_line_morphs.
|: hating.
[[[hating]],[[hate,-ing]],[[hat,-ing]]]
true.

?- word_line_morphs.
|: says.
[[[says]],[[say,-s]]]
true.

?- word_line_morphs.
|: triples.
[[[triples]],[[tripl,-pl]],[[triplis,-pl]],[[triple,-s]]]
true.

*
*/


/**
*   Question 3
*/

% The code for question 3 is in '312-pess.pl' and '312-pess-grammar.pl' marked as the following:

%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%% Part 3 %%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%
%somecode
%%%%%%%%%%%%%%%%%%%%%%%%
%%%%% Part 3 - end %%%%%
%%%%%%%%%%%%%%%%%%%%%%%%

%Addition to '312-pess.pl' (lines 356 - 359):
process(['words:'|L]) :-    % Found words
    word(W,L,[]),   % Parse words
    bug(W),       % Print for debugging
    assert_rules(W), !. % Assert words.


%Addition to '312-pess-grammer.pl' (lines 197 - 230):
%%%%%%%%%%%%%%%%%%% grammar for parsing vocabulary %%%%%%%%%%%%%%%%%%%

% List of vocabulary definitions with and between each.
word([First|Rest]) -->
    single_word(First), [and], word(Rest).

% List of vocabulary definitions without and between each.
word([First|Rest]) -->
    single_word(First), word(Rest).

% Single vocabulary definition (base of reqursion above).
word([Words]) -->
    single_word(Words).

% Vocabulary definition with isA between word and type.
single_word(Words) -->
    [X], isA, [Y], 
    {build_vocab(X,Y,Words)}, !.

% Vocabulary definition without isA between word and type.
single_word(Words) -->
    [X], [Y],
    {build_vocab(X,Y,Words)}, !.

build_vocab(X, noun, n(X)).
build_vocab(X, verb, v(X)).
build_vocab(X, adjective, adj(X)).
build_vocab(X, adverb, adv(X)).

% isA: 'is a', 'is an', or just 'is'.
isA -->
    [is], ind.

ind --> [a]; [an]; [].


%The vocabulary is in vocab.kb. To load the vocabulary, run load_rules('vocab.kb').
	
/* Output:
?- n(thing).
true.

?- v(lift).
true.

?- adj(late).
true.

?- n(project).
true.

?- adv(silly).
true.

?- adj(tired).
true.

?- n(tired). %tired is not a noun
false.

?- n(instructor).
true.

?- n(student). %student not in vocab.kb
false.

?- adj(last).
true.

?- n(word).
true.
*/

/**
*   Question 4
*
* Simplifies the rule and attribute structure to be more human-readable. This
* includes simplifying attr/3 to attr/2 by enlisting function between
* attribute. Rules with an empty body are simplified to a fact and list
* encapsulation is removed for lists of length 1.
*/
% attr(X,Y,[]) -> X(Y)
simplify_attr(attr(X,Y,[]), O) :- !, parse_xy(X,Y,O).
% attr(X,Y,Z) -> attr(X(Y),Z)
simplify_attr(attr(X,Y,Z), attr(O,W)) :- Z\=[],!,parse_xy(X,Y,O), parse_z(Z,W).
% attr(rule(H), []) -> fact(H)
simplify_attr(rule(H,[]), fact(A)) :- simplify_attr(H, A), !.
% rule(H,B) :   H :: attr/3
%               B :: list of attr/3
% rule(H,[B]) -> rule(H,B).
simplify_attr(rule(H,Z), rule(A,B)) :- Z\=[],!, simplify_attr(H,A), parse_z(Z,B).

/**
* Helper function to convert arg X,Y into a functor X(Y)
* Third arg is the return value, X(Y)
*/
parse_xy(X,Y,A) :- functor(A,X,1), arg(1,A,Y).

/**
* Recursively parses the additional list of attribute(s)
* Output can be one of:
* - attr(_,_) (or equivalent X(Y) form)     single attr/2 (not a list)
* - [attr(_,_)...]                          list of attr/2
*/
parse_z([attr(X,Y,[])|[]], O) :-       % 4 cases: 2 for inner z empty and non-empty
    !, parse_xy(X,Y,O).             % outer empty (remove bracket) and non-empty
parse_z([attr(X,Y,[])|Xs], [O|Zs]) :-
    !, parse_xy(X,Y,O), parse_z(Xs,Zs).
parse_z([attr(X,Y,Z)|[]], [attr(O,W)|[]]) :-
    Z\=[],!, parse_xy(X,Y,O), parse_z(Z,W).
parse_z([attr(X,Y,Z)|Xs], [attr(O,W)|Zs]) :-
    Z\=[],!, parse_xy(X,Y,O), parse_z(Z,W), parse_z(Xs,Zs).

/*
Output:
?- simplify_attr(rule(attr(does,eats,[attr(is_how,slowly,[]),attr(is_a,insects,[attr(is_like,large,[])])]),[]),A).
A = fact(attr(does(eats), [is_how(slowly), attr(is_a(insects), is_like(large))])).

?- simplify_attr(rule(attr(does,eats,[attr(is_how,slowly,[]),attr(a,b,[]),attr(a,b,[]),attr(is_a,insects,[attr(is_like,large,[])])]),[]),A).
A = fact(attr(does(eats), [is_how(slowly), a(b), a(b), attr(is_a(insects), is_like(large))])).
*/



