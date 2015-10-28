% Import the g&s files.
:- consult('wn_s.pl').
:- consult('wn_g.pl').


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