#!/usr/bin/chezscheme --script 
; KnowlegeBaseSystem v0.1: System for organase some information as electronic summary
; Copyright (C) 2022 Daniil Shvachkin margenom@ya.ru
; Released under the terms of the GNU General Public License version 2.0

(import (rnrs) (sqlite3) (srfi s13 strings))
(load-shared-object "libc.so.6") ;Работает только в linux (возможно всё до GSF)
(define (readlink link-path) 
	(define _readlink (foreign-procedure "readlink" (string uptr unsigned) long))
	(define-ftype out-string (array 4096 char))
	(define PATH_MAX 4096) 
	(define _out (foreign-alloc (+ 1 PATH_MAX)))
	(define link-length (_readlink link-path _out PATH_MAX))
	(define out #f)
	(if (> link-length 0) (set! out (substring (ftype-pointer->sexpr 
			(make-ftype-pointer out-string _out)) 0 link-length)))
	(foreign-free _out) out)

(define (symlink link-target link-path)
	(define _symlink (foreign-procedure "symlink" (string string) int))
	(< 0 (_symlink link-target link-path)))

(define (realpath path)
	(define _realpath (foreign-procedure "realpath" (string uptr) uptr))
	(define _out (_realpath path 0)) ; when 0 out will be allocated
	(define-ftype out-string (array 4096 char))
	(define out (if (= 0 _out) #f (ftype-pointer->sexpr (make-ftype-pointer out-string _out))))
	(unless  out (foreign-free _out)) out)

(define (file-copy path-from path-to) 
	(system (string-append "cp -r " path-from " " path-to)))

;General system functions
(define-values (CLI_ARGS CLI_PAMS)
	(let rec ((ost (command-line-arguments)) (args '()) (pams '()))
		(if (null? ost) (apply values (map reverse (list args pams))) (let* (
			(head (car ost))
			(type (char=? (string-ref head 0) #\-)))
		(rec (cdr ost) (if type args (cons head args)) 
			(if type (cons (let* (
				(delim (string-index head #\=))
				(name (string->symbol (substring head 1 (or delim (string-length head)))))
				(val (if delim (substring head (+ delim 1) (string-length head)) #t)))
			(cons name val)) pams) pams))))))

(define (pam pamname) (define pam-pair (assoc pamname CLI_PAMS)) (and pam-pair (cdr pam-pair)))
(define (clock-seconds) (time-second (current-time)))
(define (print . X) (let rec((x X)) (or (null? x) (begin (display (car x)) (rec (cdr x))))) (newline))

(define (path-skip path skipto) 
	(if (or (string=? path "") (string=? skipto (path-first path))) path
		(path-skip (path-rest path) skipto)))
; система конфигурации как в more
(define CONFIG '())
(define (config-append name def-val description)
	(unless (pam name) (set! CLI_PAMS (cons (cons name def-val) CLI_PAMS)))
	(set! CONFIG (cons (list name def-val description) CONFIG)))

;Структура католога
(config-append 'structdir (string-append (getenv "PWD") "/test")  "path for your knowlege base system structure")
;/		- системный корень (этой системы)
(config-append 'resdir (string-append (or (pam 'structdir) "") "/.res") "path for resurse dir")
;/.res/		- ресурсы загруженные пользователем (можно все файлы сделать как замеки)
;	- содержит подкаталоги для особых ресурсов (текстовые со сложным форматированием odt+ или бинарные doc+)
;	- в корне же обыные или общие ресурсы (текстовые файлы возможно с форматированием но несущественным)
;Моно все файлы хранить в одном месте и при добавлении просто выполнять следующее
;	- фаил с именем <name.ext> перемещять в каталог .res
;	- переименовать его в <uname>
;	- cоздать ссылку на <uname> с именем <name.ext> в том месте где был фаил или в корне
(config-append 'reldir (string-append (or (pam 'structdir) "") "/.rel") "path with reletionship in your file structure")
;/.rel/ 	- отношения и категории созданные автоматически
;> res.dsv 	- отношения ресурсов общих и специальных неважно 
;Зачем это всё - для возможности сохронить отношения в системе при изменении структуры
;Например я прочел статью и вычлянил из неё несколько заметок и связал их что они из одной статьи,
;потом я разделил их по разным категориям и нишел какоето применение одной из них 
;и захочу детальней понять о чем собственно она или по подробнее узнать я таким образом могу найти 
;и связанные с ней заметки и саму статью, но вдобавок к этому по мере того как заметка гуляет по структуре
;она обрастает новыми связями что очень полезно
;> struct.dsv 	- (ломаеться при изменении структуры) отношения обьектов структуры отношения обьектов структуры с ресурсами,
;есть отношения с сылкой на ресурс как к файлу
;dsv тк это просто пары ключ и значения (от 1 до сколько угодно значений (не более 60))
;	- всё остальное - пользовательские правила (используют пролог или чтото ещё когда придумаю)
;		- контролируються пользователем = вносят хаус в структуру
;		- служат для описания пользовательских обстракций над структурой
;		- могут связывать обьекты структуры и ресурсы с обстракциями
;		- могут содержать специальные правила для обработки чего либо как надо пользователю
(config-append 'cachedir (string-append (getenv "HOME") "/.cache/kbs") "path for cache dir")
;~/.cache	- кеши будут в каталоге системного кеша  
;	- сгинерированные данные на основе грубого перебора всех файлов системы в том числе 
;	- индекс необозначенных ссылок - поиск всех названий каждого файла и катогола и ссыкли 
;	- таблици терминов - список терминов каждого ресурса, (sqlite3 база)
;	- ведения изменений файлов - малоли git меня не устроит
;далее каталога с ресурсами, каталога с каталогами и так до корня
;		- можно с помощью терминов опредилять близость локаций и отдельно взятых файлов
;	- индексы упоминания - анализ близости каталогов ресурсов на основе таблици терминов
;		- для поиска и карты терминов
;в каждом файле системы (только обычные ресурсы, а не специальные)
;* (прочеее)	- структура состоит из
;	- ссылок на res - ресурсы что обозначены в системе
;	- каталогов - определяющих место чего либо в структуре
;	- ссылок на каталоги - для быстрого доступа пользователя (система их игнорирует)
;	- простые файлы (не ссылки и не катологи) - какието пользовательские файлы (система их игнорирует)
;		- могут быть например дополнительные файлы для markdown: картинки код примеры результаты
;	- ссылок кудато (вне папок .res)- пользовательские ссылки (игнорируються)
;(config-append '

(define COMMANDS '())
(define (commands-append name args-min args-max comma doc) 
	(set! COMMANDS (cons (list (symbol->string name) doc args-min args-max comma) COMMANDS)))
(define (commands-get name argc) (define cmd (assoc name COMMANDS))
	(print name "|" argc "/" (>= (list-ref cmd 2) argc (list-ref cmd 3))"|" cmd)
	(and cmd (>= (list-ref cmd 2) argc (list-ref cmd 3)) (list-ref cmd 4)))
;Как работать с системой 
;% ресурсы системы
;	- создает запись в хранилище и ссылку с именем name (лучше не плодить русурсы а повышать их уонкретность и связанность)
;kbs push [-e=<editor, def EDITOR>] [-r=<relroot>] <namep0>[ .. <namepN>] - закуск эдитора для создания файла с именем (то что введено но пробелы меняются на _)
(define (!safe file-path file-type)
	(define res-path (string-append (pam 'resdir) "/" (or file-type "") (if file-type "/" "") (path-last file-path)))
	(if (file-exists? file-path) (begin
	(file-copy file-path res-path)
	(symlink res-path file-path))))
;kbs safe <file> - обьявлякт фаил ресурсом системы и заменяет ссылкой на него
;kbs safe [-t=<type|def blob>] <file> [.. <fileN>] - будет сохранен в <res root>/<type|blob>/<utime>
(commands-append 'safe 1 -1 (lambda A
	(define type (pam 't))
	(map (lambda(file-path) (!safe file-path (and type (if (boolean? type) "blob" type)))) A))
"safe [-t[=<type>, def blob]] <file> [.. <fileN>] - safe file as resurce")

(define (!clone file-link file-name)
	(define res-path (read-link file-link))
	(file-copy res-path file-name))
;kbs clone <link> <file> - востановить копию файла из базы
(commands-append 'clone 2 2 (lambda A (!clone (car A) (cadr A)))
"clone <link> <file> - clone file from resurce")

(define (!delete file-link)
	(define res-path (read-link file-link))
	(delete-file res-path)
	(if (pam 'clean-link) (print "clean rel")))
;kbs delele [-clean-link] <link> - удаляеть из системы (лучше не удолять а изменять)
(commands-append 'delete 1 1 (lambda A (!delete (car A)))
"delete [-clean-rel] <link> - delete file from resurce")
;% структура
;cp, mv	- копировать и перенести запись по структуре
;tree - посмотреть структурную схему (как tree)
;% отношения ресурсов
(define (!relink file-link)
	; resurse id
	(define Rid (path-rest (path-skip (readlink file-link) (path-last (pam 'resdir)))))
	(symlink (string-append (pam 'resdir) "/" Rid) file-link))
;kbs relink <link name> - востановить ссылку тк ссылка имеет абсолютный путь на фаил при переносе базы она ломаеться
(commands-append 'relink 1 -1 (lambda A (map !relink A))
"relink [-R] <link>|<dir> [ .. <linkN>] - recover link to resurce")

(define (!rel rels . files) 
	(define reses (string-append ":" (string-join (filter values (map (lambda(f) (readlink f)) files)) ":")))
	(with-output-to-file (string-append (pam 'reldir) "/res.dsv") (lambda()
		(map (lambda(r) (display r) (display reses) (newline)) rels)) 'append)) 
;kbs rel -<name0> [.. -<nameN>] a [b ..] - связать ресурсы системы (res only)
(commands-append 'rel 1 -1 
(lambda A (define rels (map car (filter (lambda(r) (boolean? (cdr r))) CLI_PAMS)))
	(apply !rel rels A))
"rel -<relname> [.. -<relnameN>] <link> [ .. <linkN>] - make rel with resurses")

(define (!srel . files) 
	; include files, links, resurses, directories, any in directory structdir
	(define structures (string-append ":" (string-join (map (lambda(Uid) 
		; struct id is relative path in struct dir
		(define Sid (path-skip Uid (path-last (pam 'structdir))))
		; universal id is just system file (or maybe url, no work correct only any with no ":")
		(if (string=? Sid "") Uid Sid)) files)) ":"))
	(with-output-to-file (string-append (pam 'reldir) "/struct.dsv") (lambda()
		(map (lambda(r) (display r) (display reses) (newline)) rels)) 'append))
;kbs srel -<name0> [.. -<nameN>] a [b ..] - создать отношение файлов (a,b: link or file or dir)
(commands-append 'srel 1 -1 
(lambda A (define rels (map car (filter (lambda(r) (boolean? (cdr r))) CLI_PAMS)))
	(apply !srel rels A))
"srel -<relname> [.. -<relnameN>] <link> [ .. <linkN>] - make rel with struct objects")
;kbs about [-<name0> [.. -<nameN>]] <link> - посмотреть отношения этого обьекта
;(define (!show <link>) 4)
;kbs trace <term|link|rel> - просмотр ввиде dot граффа
;% статистика
;kbs stat term <link|file> - посмотреть близость терминологии
;kbs gen term - cгерерировать базу термов
;kbs stat link <link> - посмотреть необозначенные ссылки
;kbs gen link - cгерерировать базу необозначенных ссылок
;kbs stat rel <link>
;kbs stat between <link0> <link1> [.. <linkN>]
;% пользовательские ...
;	- запустить repl prolog
;kbs repl

(define (help-gen) 
	(print "Configns: ") 
	(map (lambda(k) (print "\t-" (list-ref k 0) "=<" (list-ref k 2) ", def " (list-ref k 1) ">")) CONFIG) 
	(print "System commands:") 
	(map (lambda(k) (print "\t" (list-ref k 1))) COMMANDS) 0)

;Require config check
;(print (command-line) "|" CLI_ARGS "|" CLI_PAMS)
(unless (and (pam 'structdir)) (begin (help-gen) (exit))) 

;Create require directories
(define (mkdir-if-not-exists dir-path) (and dir-path (unless (file-directory? dir-path) (mkdir dir-path))))
(mkdir-if-not-exists (pam 'resdir))
(mkdir-if-not-exists (pam 'cachedir))
(mkdir-if-not-exists (pam 'reldir))

; main
;(define (alert trunk) (unless trunk (begin (help-gen) (exit)))) 
;(define cmd (commands-get (car CLI_ARGS) (length (cdr CLI_ARGS)))) 
;(define DataBase (open-database (pam 'database))) 
;(apply cmd DataBase (cdr CLI_ARGS))
