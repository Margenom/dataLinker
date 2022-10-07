#!/usr/bin/env swipl
% for work with unicode files
:- encoding(utf8).
use_module(library(yall)).
%use_module(library(apply)).
%use_module(library(apply_macros)).

myconfig(res_path, M) :- getenv("HOME",H), string_concat(H, "/me/.res/",M).
myconfig(rel_path, M) :- getenv("HOME",H), string_concat(H, "/me/.rel/",M).
myconfig(hide_path, M) :- getenv("HOME",H),string_concat(H, "/.cache/kbs/",M).
myconfig(struct_root_path, M) :- getenv("HOME",H),string_concat(H, "/me/",M).

%list
assoc_list(Name, [H|Tail], O) :- [Pname|_] = H, Name == Pname, O = H
	; assoc_list(Name, Tail, O). 

length_list([], 0).
length_list([_|Tail], T) :- length_list(Tail,L), T is L + 1.

%string
string_append([X], X).
string_append([P|Ost], Out) :- string_append(Ost, Ret), string_concat(P, Ret, Out).

string_join([X], _, X).
string_join([X|Lst], Del, O) :- string_join(Lst, Del, R), string_append([X, Del, R], O).

%parse params
params_test(Term) :- atom_to_chars(Term,[M|_]), M == 45.

args_sep([], [], []).
args_sep([A|Args], [A|Pams], Data) :- params_test(A),args_sep(Args,Pams, Data).
args_sep([A|Args], Pams, [A|Data]) :- args_sep(Args,Pams, Data).

params_parse(Arg, [A|T]) :- split_string(Arg, "=", "-", [H|T]), string_to_atom(H, A).
cli_parse(Args, Ppams, Data) :- args_sep(Args, Pams, Data), maplist(params_parse, Pams, Ppams).

pamVal(Pams, Pname, Def, Pval) :- assoc_list(Pname, Pams, In),[_|Pval] = In ; Pval = Def.

%subs for commands
pwd(P) :- working_directory(P, P).
app_root(Root, Path, Apath) :- string_concat(Root, Path, Apath).
res_root(Path,Apath) :- myconfig(res_path,R), app_root(R, Path, Apath).
def_root(Path,Apath) :- myconfig(res_path,R), app_root(R, Path, Apath).
hide_(Path,Apath) :- myconfig(res_path,R), app_root(R, Path, Apath).
getsid(Link, Sid) :- read_link(Link, Path, _), file_base_name(Path, Sid).

print_dir(Pdir) :- atom_to_chars(Pdir, [H|_]), H == 46 %ignore hidden files
	;(exists_directory(Pdir),format("~s/\n", Pdir),
	directory_files(Pdir, Fls), cd(Pdir), maplist(print_dir, Fls),cd("..")
	; read_link(Pdir, File, _), format("~s -> ~s\n", [Pdir, File])
	; writeln(Pdir)).

res_type(Rname, Rtype, Sid) :- 
	exists_file(Rname),(
		read_link(Rname, Path, _),myconfig(res_path, Rp),relative_file_name(Path, Rp, Sid),(
			atom_to_chars(Sid, [H|_]), H == 46, Rtype = res %resurces
			; Rtype = ext) %external link
		; Rtype = file, Sid = Rname) %file of structure
	; exists_directory(Rname),(
		read_link(Rname, Path, _), Rtype = dirlink, Sid = Path %user link
		; Sid = Rname, Rtype = dir) %part of struct
	; Rtype = val, Sid = Rname. %rel

% create rels
struct_ref(Path, Struct) :- myconfig(struct_root_path, Root), relative_file_name(Path, Root, Struct).
rel_templ(Rel, Atoms, Out) :- string_join(Atoms, ", ", As), string_append([Rel, "(", As, ").\n"], Out).
rel_pam_conv([], []).
rel_pam_conv([[P|T]|Pams], Rels) :-  rel_pam_conv(Pams, Out),(T == [], Rels = [P|Out] ; Rels = Out). 

info(Pred, Fact) :- current_predicate(FactName/1), Fact =.. [FactName,Pred], call(Fact).
info_all(Pred, Facts) :- findall(F, info(Pred, F), Facts).

% safe data
res_safe(Spec, File) :- get_time(Time),round(Time,Utime), res_root(Sid, Path),
	exists_file(File),copy_file(File, Sid),delete_file(File),link_file(Path, File, symbolic).

% commands of program
cmds(repl, _, _) :-
	myconfig(rel_path, Rel),string_append([Rel, "res.pl"], Rf),[Rf],
	myconfig(rel_path, Rel),string_append([Rel, "struct.pl"], Sf),[Sf],
	myconfig(rel_path, Rel),string_append([Rel, "user.pl"], Uf),[Uf],
	prolog.
cmds(res, Files, _).
%cmds(spec, 
cmds(clone, [Link|[Clone|_]], _) :- read_link(Link, File, _), copy_file(File, Clone).
cmds(clone, [Link|[]], _) :- file_base_name(Link, Name), cmds(clone, [Link, Name]).
cmds(del, [Link|_], _) :- delete_file(Link).
cmds(relink, [Link|_], _) :- getsid(Link, Sid), res_root(Sid, Sidloc),
	delete_file(Link),link_file(Sidloc, Link, symbolic).
cmds(rel, [], Pams) :- myconfig(rel_path, Rel),string_append([Rel, "res.pl"], Rf),
	rel_pam_conv(Pams, Terms), [Rf], maplist(listing, Terms).
cmds(rel, Links, Pams) :- rel_pam_conv(Pams, Terms),maplist(getsid, Links, Atoms),
	maplist([X,Y]>>rel_templ(X, Atoms, Y), Terms, Out), 
	myconfig(rel_path, Rel),string_append([Rel, "res.pl"], Rf),
	open(Rf, append, Fp),maplist([X]>>writeln(Fp,X), Out),close(Fp).
cmds(srel, [], Pams) :- myconfig(rel_path, Rel),string_append([Rel, "struct.pl"], Rf),
	rel_pam_conv(Pams, Terms), [Rf], maplist(listing, Terms).
cmds(srel, Links, Pams) :- rel_pam_conv(Pams, Terms),maplist(struct_ref, Links, Atoms),
	maplist([X,Y]>>rel_templ(X, Atoms, Y), Terms, Out),
	myconfig(rel_path, Rel),string_append([Rel, "struct.pl"], Rf),
	open(Rf, append, Fp),maplist([X]>>writeln(Fp,X), Out),close(Fp).
cmds(about, D, P) :- maplist(writeln, P),writeln(D).
cmds(tree, D, P) :- maplist(writeln, P),writeln(D).

% Main
main([]) :- writeln('print help'), !.
main(Argv) :- cli_parse(Argv, Pams, [Cmd|Data]),cmds(Cmd, Data, Pams);main([]).
% for run as application
:- initialization(main, main).
