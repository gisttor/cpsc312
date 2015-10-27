% Load ProNTo_Morph module
:- ensure_loaded('Schlachter/pronto_morph_engine').

/**
*   Question  2
*
* Read a word and output the complete ProNTo_Morph parsing.
* Precondition: 
* Input is a single word and terminated by a period.
*
*/
word_line_morphs :- 
    read(X),
    morph_atoms_bag(X,Y), 
    write(Y).
