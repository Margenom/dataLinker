#!/usr/bin/env swipl
% for work with unicode files
:- encoding(utf8).

myconfig(res_path, M) :- getenv("HOME",H), string_concat(H, "/me/.res/",M).
myconfig(def_path, M) :- getenv("HOME",H), string_concat(H, "/me/.def/",M).
myconfig(rul_path, M) :- getenv("HOME",H), string_concat(H, "/me/.rul/",M).

%parse params
params_test(Term) :- atom_to_chars(Term,[M|_]), M == 45.

args_sep([], [], []).
args_sep([A|Args], [A|Pams], Data) :- params_test(A),args_sep(Args,Pams, Data).
args_sep([A|Args], Pams, [A|Data]) :- args_sep(Args,Pams, Data).

params_parse(Arg, Pam) :- split_string(Arg, "=", "-", Pam).
cli_parse(Args, Ppams, Data) :- args_sep(Args, Pams, Data), maplist(params_parse, Pams, Ppams).

%subs for commands
pwd(P) :- getenv("PWD", CD), string_concat(CD, "/", P).
app_root(Root, Path, Apath) :- string_concat(Root, Path, Apath).
res_root(Path,Apath) :- myconfig(res_path,R), app_root(R, Path, Apath).
getsid(Link, Sid) :- read_link(Link, Path, _), file_base_name(Path, Sid).

print_dir(Pdir) :- atom_to_chars(Pdir, [H|_]), H == 46 %ignore hidden files
	;(exists_directory(Pdir),format("~s/\n", Pdir),
	directory_files(Pdir, Fls), cd(Pdir), maplist(print_dir, Fls),cd("..")
	; read_link(Pdir, File, _), format("~s -> ~s\n", [Pdir, File])
	; writeln(Pdir)).

% commands of program
cmds(repl, _, _) :- prolog.
cmds(safe, [File|_], _) :- get_time(Time),round(Time,Utime), res_root(Utime, Sid),
	exists_file(File),copy_file(File, Sid),delete_file(File),link_file(Sid, File, symbolic).
cmds(clone, [Link|[Clone|_]], _) :- read_link(Link, File, _), copy_file(File, Clone).
cmds(clone, [Link|[]], _) :- file_base_name(Link, Name), cmds(clone, [Link, Name]).
cmds(del, [Link|_], _) :- delete_file(Link).
cmds(relink, [Link|_], _) :- getsid(Link, Sid), res_root(Sid, Sidloc),
	delete_file(Link),link_file(Sidloc, Link, symbolic).
cmds(about, D, P) :- maplist(writeln, P),writeln(D).
cmds(tree, D, P) :- maplist(writeln, P),writeln(D).

% Main
main([]) :- writeln('print help').
main(Argv) :- cli_parse(Argv, Pams, [Cmd|Data]),cmds(Cmd, Data, Pams);main([]).
% for run as application
:- initialization(main, main).
