/**
*   Question 1
*/
% Import the g&s files.
:- ensure_loaded('wn_s').
:- ensure_loaded('wn_g').

% The id in s/6, where S is, and the id in g/2 match, all the definitons of S will be given as outputs.
% Works the same if you specified what G is and want to find out its word. 

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
    read(X),
    morph_atoms_bag(X,Y), 
    write(Y).
	
/**
*   Question 3
*/

%Please look into '312-pess.l' and '312-pess-grammar.pl' and mark the parts labelled Part 3
	
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
*/

% Convert attr/3 to attr/2 and parse the Z list
simplify_attr(attr(X,Y,[]), O) :- !, parse_xy(X,Y,O).
simplify_attr(attr(X,Y,Z), attr(O,W)) :- Z\=[],!,parse_xy(X,Y,O), parse_z(Z,W).
simplify_attr(rule(H,[]), fact(A)) :- simplify_attr(H, A), !.
% rule(H,B) :   H :: attr/3
%               B :: list of attr/3
simplify_attr(rule(H,Z), rule(A,B)) :- Z\=[],!, simplify_attr(H,A), parse_z(Z,B).

/**
* Helper function to convert arg X,Y into a functor X(Y)
* Third arg is the return value, X(Y)
*/
parse_xy(X,Y,A) :- functor(A,X,1), arg(1,A,Y).

/**
* Recursively parses the third attribute.
* Output can be one of:
* - attr(_,_)       single attr/2 (not a list)
* - [attr(_,_)...]  list of attr/2
*/
parse_z([attr(X,Y,[])], O) :-       % 4 cases: 2 for inner z empty and non-empty
    !, parse_xy(X,Y,O).             % outer empty (remove bracket) and non-empty
parse_z([attr(X,Y,[])|Xs], [O|Zs]) :-
    !, parse_xy(X,Y,O), parse_z(Xs,Zs).
parse_z([attr(X,Y,Z)], attr(O,W)) :-
    Z\=[],!, parse_xy(X,Y,O), parse_z(Z,W).
parse_z([attr(X,Y,Z)|Xs], [attr(O,W)|Zs]) :-
    Z\=[],!, parse_xy(X,Y,O), parse_z(Z,W), parse_z(Xs,Zs).

/*
* Sample output:
*
* ?- simplify_attr(rule(attr(does,eats,[attr(is_how,slowly,[]),attr(is_a,insects,[attr(is_like,large,[])])]),[]),A).
* A = fact(attr(does(eats), [is_how(slowly)|attr(is_a(insects), is_like(large))])).
*
*/
