% Import the g&s files.
:- consult('wn_s.pl').
:- consult('wn_g.pl').


definition(S, G) :- s(_ids,_,S,_,_,_), g(_idg, G), _ids =:= _idg,!.


/*
Output:
?- definition('hello', G).
G = 'an expression of greeting; "every morning they exchanged polite hellos"'.

?- definition('discipula',G).
false.

?- definition('hypernym',G).
G = 'a word that is more generic than a given word'.
*/