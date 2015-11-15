%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%        Q2: main (based on Amzi's Clam shell)        %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

main :-
greeting,
repeat, 
write('> '), 
read(X), 
do(X), 
X == quit.

greeting :-
write('This is the CPSC312 Prolog Expert System Shell.'), nl,
write('Based on Amzi''s "native Prolog shell".'), nl,
write('Type help. load. solve. or quit.'), nl,
write('at the prompt. Notice the period after each command!'), nl.

do(load):-
write('Enter file name in single quotes, followed by a period'), nl,
write('(e.g ''bird.kb''.):'), nl,
read(F),
load_rules(F),!.

do(solve):- solve,!.

do(help):-
write('Type help. load. solve. or quit.'), nl,
write('at the prompt. Notice the period after each command!'),nl,!.

do(quit).

do(X):-
write(X),
write(' is not a legal command.'), nl,
fail.

%%%%%%%%%%%%%%%%%%%
%%  Question 2:  %%  
%%%%%%%%%%%%%%%%%%%
/*
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
Understood: if it has one long backward toe then it has order that is passerformes 
Understood: if it has order that is tubenose and it is large and it has long narrow wings then it has family that is albatross 
Understood: if it has order that is waterfowl and it has long neck and it is white and it ponderously flies then it has family that is swan 
Understood: if it has order that is waterfowl and it is plump and it powerfully flies then it has family that is goose 
Understood: if it has order that is waterfowl and it feeds on the water's surface and it agilely flies then it has family that is duck 
Understood: if it has order that is falconiforms and it scavenges and it has broad wings then it has family that is vulture 
Understood: if it has order that is falconiforms and it has long pointed wings and it has large head and it has tail that is narrow at the tip then it has family that is falcon 
Understood: if it has order that is passerformes and it has flat bill and it eats flying insects then it has family that is flycatcher 
Understood: if it has order that is passerformes and it has long pointed wings and it has forked tail and it has short bill then it has family that is swallow 
Understood: if it has family that is albatross and it is white then it is laysan albatross 
Understood: if it has family that is albatross and it is dark then it is black footed albatross 
Understood: if it has order that is tubenose and it has medium size and it has flight that is flap/glide then it is fulmar 
Understood: if it has family that is swan and it has voice that is muffled musical whistle then it is whistling swan 
Understood: if it has family that is swan and it has voice that is loud trumpeting then it is trumpeter swan 
Understood: if it has family that is goose and it winters in the united states and it has black head and it has white cheeks then it is canada goose 
Understood: if it has family that is goose and it summers in canada and it has black head and it has white cheeks then it is canada goose 
Understood: if it has family that is goose and it is white then it is snow goose 
Understood: if it has family that is duck and it quacks and it has green head then it is mallard 
Understood: if it has family that is duck and it quacks and it is mottled brown then it is mallard 
Understood: if it has family that is duck and it has voice that is short whistle then it is pintail 
Understood: if it has family that is vulture and it has flight that has v-shaped profile then it is turkey vulture 
Understood: if it has family that is vulture and it has flight that has flat profile then it is california condor 
Understood: if it has family that is falcon and it eats insects then it is sparrow hawk 
Understood: if it has family that is falcon and it eats birds then it is peregrine falcon 
Understood: if it has family that is flycatcher and it has long rusty tail then it is great crested flycatcher 
Understood: if it has family that is flycatcher and it has white throat then it is ash throated flycatcher 
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