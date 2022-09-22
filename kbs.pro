#!/usr/bin/env swipl
% for work with unicode files
:- encoding(utf8).

myconfig(res_path, "/tmp/").

%parse params
params_test(Term) :- atom_to_chars(Term,[M|_]), M == 45.

args_sep([], [], []).
args_sep([A|Args], [A|Pams], Data) :- params_test(A),args_sep(Args,Pams, Data).
args_sep([A|Args], Pams, [A|Data]) :- args_sep(Args,Pams, Data).

params_parse(Arg, Pam) :- split_string(Arg, "=", "-", Pam).
cli_parse(Args, Ppams, Data) :- args_sep(Args, Pams, Data), maplist(params_parse, Pams, Ppams).

% commands of program
cmds(repl, _, _) :- prolog.
cmds(safe, [File|_], _) :- get_time(Time),round(Time,Utime),
	myconfig(res_path,R), string_concat(R, Utime, Sid),
	copy_file(File, Sid),delete_file(File),link_file(Sid, File, symbolic).
cmds(clone, D, P) :- maplist(writeln, P),writeln(D).
cmds(del, D, P) :- maplist(writeln, P),writeln(D).
cmds(relink, D, P) :- maplist(writeln, P),writeln(D).
cmds(rel, D, P) :- maplist(writeln, P),writeln(D).
cmds(about, D, P) :- maplist(writeln, P),writeln(D).
cmds(tree, D, P) :- maplist(writeln, P),writeln(D).

% Main
main([]) :- writeln('print help').
main(Argv) :- cli_parse(Argv, Pams, [Cmd|Data]),cmds(Cmd, Data, Pams);mymain([]).
% for run as application
%:- initialization(main, main).
