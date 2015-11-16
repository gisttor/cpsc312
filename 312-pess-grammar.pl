%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Author: Steve Wolfman
%% Date: 2005/02/02
%% Collaborators: based partially on work by David Lowe
%% Sources: based partially on code from Amzi!, Inc. 
%%
%% Description:
%% This file contains the natural language machinery for the 312
%% Prolog Expert System Shell.  To run this file, you should instead
%% load that file (which expects and tries to load this file).
%% However, this file CAN be run independently for debugging.
%% You'll probably want to use "try_parse/0" as the entry point.
%%
%% Approximately speaking, rules are introduced by "rule:" and end
%% with a period.  A rule can be a sentence or a condition like 
%% if <sentence> then <result>.  Sentences have a noun phrase for a
%% subject and a verb phrase.  The noun phrase can be "it" or a noun
%% with optional descriptive adjectives.  The verb phrase can be "is"
%% or "are", in which case it is followed by a noun phrase or an
%% adjective; "has", "have", "contain", or "contains", in which case
%% it is followed by any series of noun phrases joined together by 
%% "and"; or any other verb, in which case, it is also followed by
%% a conjunction of noun phrases.  Words themselves are defined 
%% (with their part of speech) below.  Words can also be "defined"
%% in place by surrounding them with " characters and tagging them
%% with a type.  For example: it is a "adj:rowdy" "n:game".  This
%% sentence would be recognized properly even though "rowdy" and
%% "game" are not defined below.  The word defined may be any text
%% (even with spaces, although not with a "), but the type must
%% be one of "n", "adj", "adv", and "v" for noun, adjective, adverb,
%% and verb, respectively.
%%
%% This file contains a small amount of code based on Amzi's 
%% various parsers.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Introduction                                                 %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% This file is broken into sections using banners like the 
% "Introduction" banner that begins this section. The key sections are:
% - Reading input into term lists: read_sentence/1 reads 
%   a sentence (up to a period) from standard input into 
%   a list of words represented as atoms.  
% - Grammar: rule/3 (gen'd from the DCG rule/1) parses natural 
%   language into a PESS rule structure; try_parse/0 prompts you 
%   for a sentence and then tries to parse and gloss it.  
% - Glossing: plain_gloss/2 translates a PESS rule structure back 
%   into natural language ("glosses" the rule) 
% - Vocabulary: defines gobs of bird-related vocabulary

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Reading input into term lists                                %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% These are just some helper rules to turn text files/input into
% lists of Prolog atoms.  (For those who know the term, this is a
% cheap lexical analyzer.)

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%  Main- Q6: Added expanding vocab funtionality  %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Load ProNTo_Morph module
:- ensure_loaded('Schlachter/pronto_morph_engine').

% Import the s files.
:- ensure_loaded('wn_s').

% (q6 main function- expand_vocab) if you input a sentence into the program, 
(1) it will first produce a list of all the stems for each word in the sentence. 
% The second step is that it will go through the list of stems and produce a list of 
% stems with all their parts of speech. In the last step, the function asserts 
% everything in the list, and return the list as well.
expand_vocab([]).
expand_vocab([H|T]):-
addword(H),
expand_vocab(T).


% (helper) add each word to the dictionary
%,_listofstemswithpartofspeech
addword(_word):-
allstems(_word,_stemlist),
allstemswithpartofspeech(_stemlist,_listofstemswithpartofspeech),
assert_all(_listofstemswithpartofspeech).


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

% (helper) convert word and its part of speech into the form partofspeech(X).
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
assert_all([]).
assert_all([H|T]):-
assert(H),
assert_all(T).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%               End of Main- Q6                  %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% added expansion functionality to read_sentence

% Read a sentence (rule).
read_sentence(_) :- peek_char(Ch), Ch = 'end_of_file', !, fail.
read_sentence(S) :- read_sent_helper(S), expand_vocab(S).

% Read a sentence as individual words.
read_sent_helper([]) :- peek_char(Ch),       % Stop at end of file.
        Ch = 'end_of_file', !.
read_sent_helper([]) :- peek_char(Ch),       % Stop at a period.
        Ch = '.', !, get_char(Ch).
read_sent_helper(Words) :- peek_char(Ch),    % Eat whitespace
        char_type(Ch, space), !, get_char(Ch), 
        read_sent_helper(Words).
read_sent_helper(Words) :- peek_char(Ch),    % Eat comments
        Ch = '%', !, get_char(Ch),           % starting from %
        discard_to_eol(),                    % to the end of line
        read_sent_helper(Words).
read_sent_helper([Word|Words]) :-            % Read quoted words.
        peek_char(Ch), Ch = '"', !,
        read_word_to(ChWord), 
        atom_chars(Word, ChWord), 
        read_sent_helper(Words).
read_sent_helper([Word|Words]) :-            % Read unquoted words.
        read_word(ChWord), 
        atom_chars(Word, ChWord), 
        read_sent_helper(Words).

% Read and discard everything until end of line
discard_to_eol() :- read_word_to('\n', _).

% Read a word taking the next character read as a delimiter.
% For example, if the input is "|hello world! 'what's new?| with you?"
% this would read "|hello world! 'what's new?|".
read_word_to([C|Cs]) :- get_char(C), read_word_to(C,Cs).

% Helper for read_word_to/1.
read_word_to(Stop, [Stop]) :- peek_char(Stop), !, get_char(Stop).
read_word_to(Stop, [C|Cs]) :- get_char(C), read_word_to(Stop, Cs).

% Read a word delimited by whitespace or a period (which ends the 
% sentence).
read_word([]) :- peek_char(Ch), char_type(Ch, space), !.
read_word([]) :- peek_char(Ch), Ch = '.', !.
read_word([]) :- peek_char(Ch), char_type(Ch, end_of_file), !.
% convert to lower case when reading char
read_word([Lc|Chs]) :- get_char(Ch),downcase_atom(Ch,Lc), read_word(Chs).





%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Grammar for 312 PESS rules + debugging machinery             %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% This section includes the grammar for parsing PESS rules from English
% sentences along with some debugging machinery to for understanding
% the parsing process.

% The next section contains a related set of predicates for glossing
% (i.e., translating into natural language) PESS rules, the reverse
% process to the one performed in this section.

% There are some notes on rules and goals in 312pess.pl's section on
% asking, solving, and proving. However, it is worth repeating some of
% that information here and expanding on how goals (in particular)
% relate to English sentences.
%
% A rule is of the form rule(Head, Body), where Head is a single goal
% (instance of attr/3) while Body is a (possibly empty) list of
% goals. A rule "rule(Head, Body)" means something like "if Body then
% Head". Goals, AKA attributes, have the form attr(Type, Value,
% SubAttributes). Type is one of: 
% - is_a (indicating a noun/is-a relationship) 
% - has_a (indicating a noun/containment/possession relationship) 
% - is_like (indicating an adjective/descriptive relationship) 
% - is_how (indicating an adverb/descriptive relationship) 
%
% Value may be anything. SubAttributes is a (possibly empty) list of
% "attached" attributes -- that is, further attributes describing this
% one.
%
% For example: its very sharp claws slowly tear the paper
% would be described as:
% attr(has_a, claws,                % its claws
%  [attr(is_like, sharp,            % claws like what? sharp
%     [attr(is_how, very, [])]),    % how sharp? very
%   attr(does, tear,                % do what? tear
%     [attr(is_how, slowly, []),    % tear how? slowly
%      attr(is_a, paper, [])])])    % tear what? paper
%      
% Note that glossing this (see the plain_gloss predicate) will
% result in an equivalent "canonical" form:
% it has very sharp claws that slowly tear paper

% The actual words (nouns, adjectives, adverbs, and most verbs) that the
% parser is capable of understanding are enumerated in a separate, long,
% but simple section below.

%%%%%%%%%%%%%%%% debugging code %%%%%%%%%%%%%%%%%%%%%%%%%%

% Debugging predicate for quick test parsing of a rule.
try_parse :- try_parse(P),
        write('Parsed structure is: '), write(P), nl, nl,
        plain_gloss(P, Text),
        write('Understood: '), write_sentence(Text), nl.

% Debugging predicate for quick test parsing of a rule.
try_parse(P) :- read_sentence(Sent), rule(P, Sent, []).

% Debugging predicate for quick test parsing of a goal.
try_goal(P) :- read_sentence(Sent), goal(P, Sent, []).

%% A sample parsed sentence for testing.  Should mean:
%% It very slowly and carefully eats languidly flying very very 
%% small insects and brown worms.
big_test_term(X) :- X =
[attr(does, eat, 
      [attr(is_how, slowly, [attr(is_how, very, [])]), 
       attr(is_how, carefully, []), 
       attr(is_a, insect, [attr(is_like, flying, 
                                [attr(is_how, languidly, [])]), 
                           attr(is_like, small, 
                                [attr(is_how, very, 
                                      [attr(is_how, very, [])])])]), 
       attr(is_a, worm, [attr(is_like, brown, [])])])].



%%%%%%%%%%%%%%%%%%% grammar for parsing rules %%%%%%%%%%%%%%%%%%%

% Rules can be..
rule(Rules) -->                              % if S+ then S+
        [if], sentence_conj_plus(Body),      % conjunctive bodies OK
        [then], sentence_conj_plus(Head),    % conjunctive heads..
        { build_rules(Body, Head, Rules) }.  % broken into separate rules
rule(Rules) -->                              % S if S+
        sentence(Head), [if],                
        sentence_conj_plus(Body),
        { build_rules(Body, Head, Rules) }.
rule(Rules) -->
        sentence(Head),                      % S (only)
        { build_rules([], Head, Rules) }.    % That's a fact! No body.


%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%% Part 3 %%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%
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

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%
%%%%% Part 3 - end %%%%%
%%%%%%%%%%%%%%%%%%%%%%%%


%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%% Main q3 %%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%

% Load ProNTo_Morph module
:- ensure_loaded('Schlachter/pronto_morph_engine').

% Converts the base form verb (Base) to the doing form, as it's given in v(X),
% using the ProNTo Morph engine.
% Intended to be used with Base ground and Doing unground
% Search all the verbs and match if they have a [Base,-s] morpheme
baseToDoingVerb(Base,Doing) :- v(Doing), morph_atoms(Doing, [[Base, -s]]).

% Optional usage for "the hell" or "the heck"
opt_the_hell --> [].
opt_the_hell --> [the,hell];[the,heck].

% Goals can be
goal(Goal) --> % what is it
        % Optional "the hell/the heck" usage for bonus
        % "that" is added to np([]) vocab below.
        % read_sentence translates input to all lower case above
        % in read_word/1
        [what], opt_the_hell, vis, np([]),
        { build_goal(X, [attr(is_a, X, [])], Goal) }, !.
    % answer to goal is the "it"
    % ?- process(['goal:',what,is,it]).
    % Understood: [rule(top_goal(_G2216),[attr(is_a,_G2216,[])])]
    %
    % ?- try_goal(X).
    % what the heck is THAT.
    %
    % X = [rule(top_goal(_G393), [attr(is_a, _G393, [])])]

goal(Goal) -->
        % what does it have
        [what, does], np([]), vhas,
        { build_goal(X, [attr(has_a, X, [])], Goal) }, !.
    % ?- process(['goal:',what,does,it,have]).
    % Understood: [rule(top_goal(_G2264),[attr(has_a,_G2264,[])])]

goal(Goal) -->
        % is it <np/adjp>
        vis, np([]), isitqp(Body),
        { build_goal(yes, Body, Goal) }, !.
    % answer/goal to "is it.." is always yes or no
    % ?- process(['goal:',is,it,a,brown,swan]).
    % Understood: [rule(top_goal(yes),[attr(is_a,swan,[attr(is_like,brown,[])])])]

goal(Goal) -->
        % does it <vp>
        [does], np([]), doesqp(Body),
        { build_goal(yes, Body, Goal) }, !.
    % answer/goal to "does it ..." is always yes or no
    % ?- process(['goal:',does,it,eat,nostrils]).
    % Understood: [rule(top_goal(yes),[attr(does,eats,[attr(is_a,nostrils,[])])])]

goal(Goal) -->
        % what does it <v (base form)>
        [what, does], np([]), vdo([attr(does,X,[])]),
        { build_goal(Y, [attr(does,X, [attr(is_a,Y,[])])], Goal) }, !.
    % answer/goal to "what does it ..." is the subject of a "does" attribute
    % ?- process(['goal:',what,does,it,scavenge]).
    % Understood: [rule(top_goal(_G2391),[attr(does,scavenges,[attr(is_a,_G2391,[])])])]

goal(Goal) -->
        % how does it <v (base form)>
        [how, does], np([]), vdo([attr(does,X,[])]),
        { build_goal(Y, [attr(does,X, [attr(is_how,Y,[])])], Goal) }, !.
    % answer to "how does it .." is the attribute belonging to is_how
    % ?- process(['goal:',how,does,it,fly]).
    % Understood: [rule(top_goal(_G2313),[attr(does,flies,[attr(is_how,_G2313,[])])])]

goal(Goal) -->
        % what is the <np> like
        %
        % If the np includes an is_like relation, like flying toe, we take it to
        % describe the toe, other than that it's flying. i.e., flying would not be
        % a solution as below (unless if there's somehow two is_like attributes)
        [what], vis, np(NP), [like],
        { build_prepend_attrs(NP,[attr(is_like,Y,[])], LikeTerm), build_goal(Y, LikeTerm, Goal) }, !.
    % ?- process(['goal:',what,is,the,throat,like]).
    % Understood: rule(top_goal(_G869),[attr(is_a,throat,[attr(is_like,_G869,[])])])
    %
    % ?- process(['goal:',what,is,the,flying,toe,like]).
    % Understood: rule(top_goal(_G916),[attr(is_a,toe,[attr(is_like,flying,[]),attr(is_like,_G916,[])])])

% Helper function that builds the goal using rule syntax.
% top_goal(X) is the head we want to prove and
% Y is the body (the attribute, taken as in
build_goal(X, Y, R) :- build_rules(Y, [top_goal(X)], R).

% Base for of "Doing" verbs provided below or literals
% E.g. vdo(eat) uses v(eats) and vdo(fly) uses v(flies)
% No translation for literal values. Assume it makes sense to use in the form
% provided
vdo([attr(does,Doing,[])]) --> [X], { baseToDoingVerb(X, Doing) }.
vdo([attr(does,Name,[])]) --> lit(v, Name).

%%%%%%%%%%%%%%%%%%% DOES %%%%%%%%%%%%%%%%%%

doesqp(VPTerms) -->           % Does it has or it contains
        vhas,
        np_conj(NPTerms),       % The noun should be has_a, not is_a
        { convert_to_has_a(NPTerms, VPTerms) }.
    % Example: Does it have a toe

doesqp(VPTerms) -->    % Does it advs verb nouns
    % These all get translated to attr(does,verb,[_]) (vdoes)
    adv_conj(AVTerms),          % E.g., It slowly eats worms.
    vdo(VTerms),                % All the attached attributes just
    np_conj(NPTerms),           % get thrown together on the verb.
    { append(AVTerms, NPTerms, ModTerms),
      build_prepend_attrs(VTerms, ModTerms, VPTerms) }.
  % Example: does it very slowly eats

doesqp(VPTerms) -->
    vdo(VTerms),                % It verb advs.
    adv_conj_plus(AVTerms),     % E.g., it eats slowly.
    { build_prepend_attrs(VTerms, AVTerms, VPTerms) }.

%%%%%%%%%%%%%%%%%%%% IS IT %%%%%%%%%%%%%%%%%

isitqp(VPTerms) -->
        % Is it w/adjectives.
        adj_conj_plus(VPTerms).
    % Example: is it external

isitqp(VPTerms) -->
        % Is it w/nouns (which can also have adjs).
        np_conj_plus(VPTerms).
    % Example: is it a flying toe

%%%%%%%%%%%%%%%%%%%%%%%%
%%%%% Main q3 - End %%%%
%%%%%%%%%%%%%%%%%%%%%%%%

% 1 or more sentences joined by ands.
sentence_conj_plus(Attrs) -->
        sentence(First), [and],
        sentence_conj_plus(Rest),
        { append(First, Rest, Attrs) }.  % Put the attributes together.
                                         % Would diff lists be better here?
sentence_conj_plus(Attrs) -->
        sentence(Attrs).

% Sentences that start with 'it' or other vacuous subjects.
sentence(Attrs) -->
        np([]), vp(Attrs).

% Sentences that start with meaningful subjects are
% noun phrase then verb phrase.
% Sentences like: "its talons are sharp" are converted to
% a canonical form with "it" as the subject:
% "it has talons that are sharp". 
sentence(Attrs) -->
        np([NPT|NPTs]), vp(VPTerms),
        { convert_to_has_a([NPT|NPTs], 
                           NPTermsHas),   % Convert to canonical form.
          build_prepend_attrs(NPTermsHas, 
                              VPTerms, 
                              Attrs) }.

% Verb phrases..
vp(VPTerms) -->                 % It has or it contains
        vhas,                   
        np_conj(NPTerms),       % The noun should be has_a, not is_a
        { convert_to_has_a(NPTerms, VPTerms) }.

vp(VPTerms) -->
        vis,                    % It is w/adjectives.
        adj_conj_plus(VPTerms).

vp(VPTerms) -->
        vis,                    % It is w/nouns (which can also have adjs).
        np_conj_plus(VPTerms).

vp(VPTerms) -->                 % It advs verb nouns
    adv_conj(AVTerms),          % E.g., It slowly eats worms. 
    vdoes(VTerms),              % All the attached attributes just
    np_conj(NPTerms),           % get thrown together on the verb.
    { append(AVTerms, NPTerms, ModTerms),
      build_prepend_attrs(VTerms, ModTerms, VPTerms) }.

vp(VPTerms) -->
    vdoes(VTerms),              % It verb advs.
    adv_conj_plus(AVTerms),     % E.g., it eats slowly.
    { build_prepend_attrs(VTerms, AVTerms, VPTerms) }.

% One or more noun phrases connected by and.
np_conj_plus(NPCTerms) -->
    np(NPTerms), [and],
    np_conj_plus(RestNPTerms),
    { append(NPTerms, RestNPTerms, NPCTerms) }.
np_conj_plus(NPCTerms) -->
    np(NPCTerms).

% Zero or more noun phrases connected by and.
np_conj(NPCTerms) --> np_conj_plus(NPCTerms).
np_conj([]) --> [].

% One or more adjectives (plus advs) connected by and.
adj_conj_plus(ADJCTerms) -->
    adjp(ADJTerms), [and],
    adj_conj_plus(RestADJTerms),
    { append(ADJTerms, RestADJTerms, ADJCTerms) }.
adj_conj_plus(ADJCTerms) -->
    adjp(ADJCTerms).

% Zero or more adjectives (plus advs) connected by and.
adj_conj(ADJCTerms) --> adj_conj_plus(ADJCTerms).
adj_conj([]) --> [].

% One or more adverbs (w/modifying advs) connected by and.
adv_conj_plus(AVCTerms) -->
    adv_plus(AVTerms), [and],
    adv_conj_plus(RestAVTerms),
    { append(AVTerms, RestAVTerms, AVCTerms) }.
adv_conj_plus(AVCTerms) -->
    adv_plus(AVCTerms).

% Zero or more adverbs connected by and.
adv_conj(AVCTerms) --> adv_conj_plus(AVCTerms).
adv_conj([]) --> [].

% One or more adverbs strung together.
adv_plus(AVTerms) -->
    int_adv_plus(AVList),                % List of raw attributes, 
                                         % in forward order.
    { build_up_advs(AVList, AVTerms) }.  % Build them up in reverse order.
                                         % This nests them w/last adverb
                                         % in the text outermost.

% Zero or more adverbs strung together.
adv_star(AVTerms) --> adv_plus(AVTerms).
adv_star([]) --> [].

% Collect one or more adverbs into a list (as though they didn't
% modify each other) to be converted into nested (attached) attributes
% at a later pass.
% It's tempting to just have adv_plus and try something like:
% adv_plus(Terms), adv(LastTerms), 
% { build_prepend_attrs(Terms, LastTerm, ResultTerms) }
% However, this is left recursive and will run forever.
int_adv_plus(AVPTerms) -->
    adv(AVTerms),
    int_adv_plus(RestAVTerms),
    { append(AVTerms, RestAVTerms, AVPTerms) }.
int_adv_plus(AVPTerms) -->
    adv(AVPTerms).


% Noun phrase is determiner (or "its") + adjectives + noun.
% Produces an is_a with attached attributes.
np(NPTerms) --> 
        det_opt, 
        adjp_star(APTerms), 
        n(NTerms),
        { build_prepend_attrs(NTerms, APTerms, NPTerms) }.

% Zero or more adjectives (chained together without and before a noun).
% Adjective phrases in a chain become a list of adjective phrases.
adjp_star(APTerms) -->
        adjp(FstAPTerms),
        adjp_star(RestAPTerms),
        { append(FstAPTerms, RestAPTerms, APTerms) }.
adjp_star([]) --> []. 

% An "adjective phrase", which may include adverbs.
adjp(APTerms) -->
        adv_star(AVTerms),
        adj(AdjTerms),
        { build_prepend_attrs(AdjTerms, AVTerms, APTerms) }.


% Determiners have no effect and are ignored.
det_opt --> [].
det_opt --> [its].
det_opt --> [the].
det_opt --> [a].
det_opt --> [an].

% Nouns become is_a attributes.
n([]) --> [it]; [that].                   % "it", "that" are ignored
n([attr(is_a,X,[])]) --> [X], { n(X) }.   % Anything listed below.
n([attr(is_a,Name,[])]) --> lit(n, Name). % Any literal tagged as 'n'


% Adverbs are either those provided below or literals.
adv([attr(is_how,X,[])]) --> [X], { adv(X) }.
adv([attr(is_how,Name,[])]) --> lit(adv, Name).

% Adjectives are either those provided below or literals.
adj([attr(is_like,X,[])]) --> [X], { adj(X) }.
adj([attr(is_like,Name,[])]) --> lit(adj, Name).


% "Doing" verbs (as opposed to "has" and "is".
% Either provided below or literals.
vdoes([attr(does,X,[])]) --> [X], { v(X) }.
vdoes([attr(does,Name,[])]) --> lit(v, Name).

% "Having" verbs are "has" or "have" and "contain" or "contains".
% The semi-colon is disjunction (just syntactic sugar
% since four separate rules would have the same effect
% as these two disjunctive rules).
vhas --> [has]; [have].
vhas --> [contain]; [contains].

% "Being" verbs are "is" or "are".
vis --> [is]; [are].

% The user can also specify literal terms to include.
% In that case, we just accept them.  Usually, the user
% will use the short form: surround the term by quotes
% (allowing spaces and such) and tag it with a part of
% speech: n (noun), adj (adjective), adv (adverb), or
% v (doing verb).
lit(Type, Name) --> [lit(Type, Name)].
lit(Type, Name) --> [X], 
      { atom_chars(X, ['"'|Cs]),      % starts with "
        append(Word, ['"'], Cs),      % ends with "
        append(TypeCs,                % is of the form
               [':'|NameCs],          % type:name
               Word),
        length(TypeCs, N1), N1 > 0,
        length(NameCs, N2), N2 > 0,
        atom_chars(Type, TypeCs),     % then collect the type
        atom_chars(Name, NameCs) }.   % and name.


%%%%%%%%%%% Helper predicates for parsing rules %%%%%%%%%%%%

% build_rules just breaks the conjunctive head supplied as its second
% argument across multiple rules.  So, p and q :- r becomes p :- r and
% q :- r.
build_rules(_, [], []).
build_rules(Body, [Head|Heads], 
              [rule(Head, Body)|Rules]) :-
        build_rules(Body, Heads, Rules).


% build_up_advs(AdvList, NestedAdv) is true if NestedAdv is
% the (inside out) nested version of the separately listed
% attributes in AdvList.  So, roughly, [very, slowly] becomes
% attr(is_how(slowly), is_how(very)).  Actually, the syntax
% of both arguments is quite different (see the start of this
% grammar for more details), but the basic idea is that the
% last adverb must go on the outside.
build_up_advs(Advs, Soln) :- build_up_advs(Advs, [], Soln).

% Accumulator-based version of build_up_advs/2.
build_up_advs([], Soln, Soln).
build_up_advs([attr(Attr,Val,Subs)|Advs], SoFar, Soln) :-
        append(Subs,SoFar,NewSubs),
        build_up_advs(Advs, [attr(Attr,Val,NewSubs)], Soln).


% build_prepend_attrs(Bases, Attrs, Results) is true if Results
% is a list of attributes, each of which corresponds to the type
% and value of an attribute in the list Bases but with its 
% attached attributes replaced by Attrs.
% Note that this effectively means Bases is assumed to have no
% attached attributes (else, these are thrown away).
build_prepend_attrs([], _, []).
build_prepend_attrs([attr(Base,Val,Subs)|Bases], Attrs, 
                    [attr(Base,Val,NewSubs)|Results]) :-
        append(Subs,Attrs,NewSubs),
        build_prepend_attrs(Bases, Attrs, Results).


% convert_to_has_a/2 is true if its second argument is its first except
% with is_a attributes transformed to has_a attributes.
convert_to_has_a([], []).
convert_to_has_a([attr(is_a, Val, Subs)|Attrs],
                 [attr(has_a, Val, Subs)|ConvAttrs]) :-
        convert_to_has_a(Attrs, ConvAttrs).




%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Glossing rules back into natural language           %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Just as the grammar section transforms natural language into rules,
% this section transforms rules back into natural language. Please see
% the grammar section and the "solving, asking, and proving" sections
% for more details on the structure of rules and goals in PESS.

% "Gloss" (put in natural language) the fact list X into
% the list of words Y. Note that X is a list, not a single fact!
plain_gloss(X, Y) :- top_gloss_all_and(X, Y, []), !.
plain_gloss(X, X).  % Gloss anything unglossable as itself.

% Gloss the list but put ands in between.  (In between every pair b/c
% no punctuation!)
top_gloss_all_and([]) --> [].                   % Empty list gets no "and"
top_gloss_all_and([Attr]) --> top_gloss(Attr).  % Single elt gets no "and"
top_gloss_all_and([Attr1,Attr2|Attrs]) -->      % Put ands btw each pair
        top_gloss(Attr1), [and],                % in longer lists.
        top_gloss_all_and([Attr2|Attrs]).

% "Top" glosses are glosses of the outermost attributes:
% those that control the verb form of the sentence. 
top_gloss(rule(Head, [])) -->            % A rule with no body is a fact.
        top_gloss(Head).                 % Leave off the if/then.
top_gloss(rule(Head, [Body|Rest])) -->   
        [if],                            % If
        top_gloss_all_and([Body|Rest]),  % body
        [then],                          % then
        top_gloss(Head).                 % head
top_gloss(attr(does, Verb, Attrs)) -->
        [it], 
        { split_attrs(Attrs, is_how, Advs, Others) },
        gloss_all_and(Advs),             % It adverbs 
        gloss_poss_pl(Verb),             % verb
        gloss_all_and(Others).           % nouns 
                                         % (e.g., it slowly eats insects)
top_gloss(attr(has_a, What, Attrs)) -->
        [it], [has], 
        { split_attrs(Attrs, is_like, Adjs, Others1) },
        { split_attrs(Others1, is_a, Nouns, Others) },
        gloss_all(Adjs),                 % It has adjs 
        gloss_poss_pl(What),             % noun 
        gloss_that_are_and(Nouns),       % that are noun 
        gloss_all_and(Others).           % E.g., it has two feet that
                                         % are sharp claws.
top_gloss(attr(is_a, What, Attrs)) -->
        [it], [is],
        { split_attrs(Attrs, is_like, Adjs, Others) },
        gloss_all(Adjs),                 % It is adjs
        gloss_poss_pl(What),             % noun
        gloss_all_and(Others).           % E.g., it is a small beetle.
top_gloss(attr(is_like, What, Attrs)) -->
        [it], [is],
        { split_attrs(Attrs, is_how, Advs, Others) },
        gloss_all_and(Advs),             % It is advs
        [What],                          % adj.
        gloss_all_and(Others).           % E.g., it is slightly yellow.

% Non-top glosses are a bit simpler.
% Just ensure that attributes attached to the current one
% are printed in the correct order and with "and" when appropriate.
% For example, any number of small simple infuriating adjectives
% can precede a noun without and while simply and clearly putting
% adverbs before a verb requires ands.
% Of course, number (e.g., singular vs. plural) is not accounted for!
gloss(attr(is_a, What, Attrs)) -->   % Nouns.
        gloss_all(Attrs),            
        gloss_poss_pl(What).
gloss(attr(does, Verb, Attrs)) -->    % Verbs (not is/has).
        [that], { split_attrs(Attrs, is_how, Advs, Others) },
        gloss_all_and(Advs),          % Since this isn't top-level, it's
        gloss_poss_pl(Verb),          % a sub-clause; so, it starts w/that
        gloss_all_and(Others).        % E.g., "it is a bird that slowly
                                      % eats insects and other vermin"
gloss(attr(has_a, What, Attrs)) -->   % Having verbs (has/contains)
        [that], [has],                % As above, this is a subordinate
        gloss_all_and(Attrs),         % clause: "that has many toes"
        gloss_poss_pl(What).
gloss(attr(is_like, What, Attrs)) --> % Adjectives.
        gloss_all(Attrs),
        [What].
gloss(attr(is_how, How, Attrs)) -->   % Adverbs; note that attached
        gloss_all(Attrs),             % attributes (which should also
        [How].                        % be adverbs) go FIRST, ensuring
                                      % the "most important" adverb is 
                                      % last.

% Gloss as possibly a plural.
% A cop-out for not handling number.  Currently unused.
gloss_poss_pl(Atom) --> 
%       { atom_concat(Atom, '(s)', AtomPl) },  % skipping plurals for now.
        { AtomPl = Atom },
        [AtomPl].

% Gloss a list of attributes without conjunctions.
gloss_all([]) --> [].
gloss_all([Attr|Attrs]) --> gloss(Attr), gloss_all(Attrs).

% Gloss a list of attributes, putting "ands" between as necessary.
% (Ands are used between every pair b/c there's no punctuation!)
gloss_all_and([]) --> [].
gloss_all_and([Attr]) --> gloss(Attr).
gloss_all_and([Attr1,Attr2|Attrs]) --> 
        gloss(Attr1), [and], 
        gloss_all_and([Attr2|Attrs]).

% Gloss the list with ands, starting with "that is" if non-empty.
% (For subordinate is clauses.)
gloss_that_are_and([]) --> [].
gloss_that_are_and([A|As]) --> [that], [is], gloss_all_and([A|As]).

% split_attrs(Attrs, Target, Hits, Misses) is true if Hits are all
% the attributes from Attrs with types (first args) matching Target
% and Misses are the rest.  (Order is preserved.)
split_attrs([], _, [], []).
split_attrs([attr(Target, Val, Subs)|Rest], 
            Target,
            [attr(Target, Val, Subs)|Targets], 
            Others) :- 
        split_attrs(Rest, Target, Targets, Others).
split_attrs([attr(NonTarget, Val, Subs)|Rest], 
            Target, Targets,
            [attr(NonTarget, Val, Subs)|Others]) :-
        Target \= NonTarget,
        split_attrs(Rest, Target, Targets, Others).


% Write out a sentence of words.  Uses tabs in hopes of wrapping lines
% more effectively.
write_sentence([]).
write_sentence([Word|Words]) :- write(Word), tab(1), write_sentence(Words).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Vocabulary for the PESS parser                               %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% These are a bunch of bird-related Here's a bunch of bird-related
% words. Note, however, that they really do not belong in this file! 
% Ideally, they would either: 
% - be unnecessary because an external dictionary would provide words.
% - be embedded in the knowledge base file itself as another type of
%   parsable statement.
%
% Note also that some plurals and some singulars of nouns and verbs are
% included. Ideally, again, stemming would be automatic, even for
% user-supplied words.

% Nouns.
:- dynamic(n/1).    % Ensure that the predicate can be modified dynamically

n(order).
n(nostrils).
n(bill).
n(waterfowl).
n(falconiforms).
n(meat).
n(talons).
n(feet).
n(passerformes).
n(flycatcher).
n(voice).
n(toe).
n(size).
n(wings).
n(family).
n(albatross).
n(neck).
n(colour).
n(flight).
n(profile).
n(birds).
n(swan).
n(goose).
n(duck).
n(vulture).
n(head).
n(tail).
n(falcon).
n(insect).
n(flycatcher).
n(swallow).
n(fulmar).
n(whistle).
n(trumpeting).
n(season).
n(country).
n(cheeks).
n(summer).
n(winter).
n(canada).
n(quack).
n(mallard).
n(pintail).
n(bird).
n(throat).
n(insects).

% Adverbs.
:- dynamic(adv/1).  % Ensure that the predicate can be modified dynamically

adv(very).
adv(slowly).
adv(carefully).
adv(languidly).
adv(ponderously).
adv(powerfully).
adv(agilely).
adv(mottled).
% Adjectives.
:- dynamic(adj/1).  % Ensure that the predicate can be modified dynamically

adj(external).
adj(tubular).
adj(hooked).
adj(webbed).
adj(flat).
adj(curved).
adj(sharp).
adj(hooked).
adj(one).
adj(long).
adj(pointed).
adj(backward).
adj(large).
adj(narrow).
adj(white).
adj(dark).
adj(black).
adj(ponderous).
adj(plump).
adj(powerful).
adj(broad).
adj(flying).
adj(forked).
adj(short).
adj(medium).
adj(muffled).
adj(musical).
adj(loud).
adj(green).
adj(brown).
adj('v-shaped').
adj(rusty).
adj(square).

% Doing verbs (i.e., not is/are or has/have/contains/contain).
:- dynamic(v/1).  % Ensure that the predicate can be modified dynamically

v(eats).
v(flies).
v(lives).
v(feeds).
v(scavenges).
v(quacks).
v(summers).
v(winters).
