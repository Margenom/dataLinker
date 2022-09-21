#!/usr/bin/env swipl
%:- initialization(main, main).

kbs_main :- format('Knowlege Base System~n').

main(Argv) :-
        concat_atom(Argv, ' ', SingleArg),
        term_to_atom(Term, SingleArg),
        Val is Term,
        format('~w~n', [Val]).
