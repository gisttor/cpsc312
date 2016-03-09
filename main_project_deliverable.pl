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
*   Question 2
*/

/*
see 312-pess.pl part labelled as follwoing:
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%      Main- Q2: main (based on Amzi's Clam shell)    %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

Output:
1 ?- main.
This is the CPSC312 Prolog Expert System Shell.
Based on Amzi's "native Prolog shell".
Type help. load. solve. or quit.
at the prompt. Notice the period after each command!
> load.

Enter file name in single quotes, followed by a period
(e.g 'bird.kb'.):
|: 'bird.kb'.

Understood: if it has external tubular nostrils and it lives at sea and it has hooked bill then it has order that is tubenose 
Understood: if it has webbed feet and it has flat bill then it has order that is waterfowl 
Understood: if it eats meat and it has feet that is curved talons and it has sharp hooked bill then it has order that is falconiforms 
[...]
Understood: if it has family that is swallow and it has forked tail then it is barn swallow 
Understood: if it has family that is swallow and it is dark then it is purple martin 
rules loaded

> |: solve.
Would you say that it has external tubular nostrils ?> |: no.
Would you say that it has webbed feet ?> |: no.
Would you say that it eats meat ?> |: yes.
Would you say that it has feet that is curved talons ?> |: yes.
Would you say that it has sharp hooked bill ?> |: yes.
Would you say that it scavenges ?> |: yes.
Would you say that it has broad wings ?> |: yes.
Would you say that it has flight that has v-shaped profile ?> |: yes.
The answer is turkey vulture

> |: help.
Type help. load. solve. or quit.
at the prompt. Notice the period after each command!

> |: quit.

true .
*/


/**
*   Question 6
*/

/*
see 312-pess-grammar.pl part labelled as follwoing:
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%  Main- Q6: Added expanding vocab funtionality  %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

Output:
?- read_sentence(S).
it is a beautiful slug.

S = [it, is, a, beautiful, slug].

?- n(slug).
true.

?- adj(beautiful).
true ;
true.

?- n(hyena).
false.

?- adj(amazing).
false.

?- v(amaze).
false.

?- read_sentence(S).
it is an amazing hyena.

S = [it, is, an, amazing, hyena].

?- n(hyena).
true.

?- adj(amazing).
true .

?- v(amaze).
true .
*/