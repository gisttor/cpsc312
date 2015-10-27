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
