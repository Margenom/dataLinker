#!/usr/bin/chezscheme --script
(import (sqlite3) (srfi s13 strings))
;Структура католога
;/		- системный корень
;/.res/		- ресурсы загруженные пользователем (можно все файлы сделать как замеки)
;	- содержит подкаталоги для особых ресурсов (текстовые со сложным форматированием odt+ или бинарные doc+)
;	- в корне же обыные или общие ресурсы (текстовые файлы возможно с форматированием но несущественным)
;Моно все файлы хранить в одном месте и при добавлении просто выполнять следующее
;	- фаил с именем <name.ext> перемещять в каталог .res
;	- переименовать его в <uname>
;	- cоздать ссылку на <uname> с именем <name.ext> в том месте где был фаил или в корне
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

(define-values (CLI_ARGS CLI_PAMS)
	(let rec ((ost (command-line-arguments)) (args '()) (pams '()))
		(if (null? ost) (apply values (map reverse (list args pams))) (let* (
			(head (car ost))
			(type (char=? (string-ref head 0) #\-)))
		(rec (cdr ost) (if type args (cons head args)) 
			(if type (cons (let* (
				(delim (string-index head #\=))
				(name (string->symbol (substring head 1 (or delim (string-length head)))))
				(val (and delim (substring head (+ delim 1) (string-length head)))))
			(cons name val)) pams) pams))))))

(define (print . X) (let rec((x X)) (or (null? x) (begin (display (car x)) (rec (cdr x))))) (newline))
(define (pam pamname) (define pam-pair (assoc pamname CLI_PAMS)) (and pam-pair (cdr pam-pair)))

(print CLI_ARGS "|" CLI_PAMS)

(define (clock-seconds) (time-second (current-time)))

; система конфигурации как в more 
(define CONFIG '())
(define (config-append name def-val description)
	(set! CONFIG (cons (list name def-val description) CONFIG)))

(config-append 'database #f "path for your time switch database")
;(config-append '

(define COMMANDS '())
(define (commands-append name args-min args-max doc comma) 
	(set! COMMANDS (cons (list (symbol->string name) doc args-min args-max comma) COMMANDS)))
(define (commands-get name argc) (define cmd (assoc name COMMANDS))
	(print name "|" argc "/" (>= (list-ref cmd 2) argc (list-ref cmd 3))"|" cmd)
	(and cmd (>= (list-ref cmd 2) argc (list-ref cmd 3)) (list-ref cmd 4)))
;Как работать с системой 
;% ресурсы системы
;	- создает запись в хранилище и ссылку с именем name (лучше не плодить русурсы а повышать их уонкретность и связанность)
;kbs push [-e=<editor, def EDITOR>] [-r=<relroot>] <namep0>[ .. <namepN>] - закуск эдитора для создания файла с именем (то что введено но пробелы меняются на _)
;kbs safe <file> - обьявлякт фаил ресурсом системы и заменяет ссылкой на него
;kbs safe [-t=<type|def blob>] <file> - будет сохранен в <res root>/<type|blob>/<utime>
(define (!safe file-name file-type) 4)
;	- востановить копию фаил из базы
;kbs clone <link>
(define (!clone file-link) 4)
;	- удаляеть из системы (лучше не удолять а изменять)
;kbs delele <link> 
(define (!delete file-link clean-hist) 4)
;% структура
;	- копировать и перенести запись по структуре
;cp
;mv
;	- посмотреть структурную схему (как tree)
;kbs tree [<root>] 
;% отношения ресурсов
;	- востановить ссылку тк ссылка имеет абсолютный путь на фаил при переносе базы она ломаеться
;kbs relink <link name>
(define (!relink file-link) 4)
;	- связать ресурсы системы (res only)
;kbs rel -<name0> [.. -<nameN>] a [b]
(define (!rel . files) 4) 
;	- создать отношение файлов (a,b: link or file or dir)
;kbs srel -<name0> [.. -<nameN>] a [b]
(define (!srel . files) 4) 
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

; main
;(define (alert trunk) (unless trunk (begin (help-gen) (exit)))) 
;(define cmd (commands-get (car CLI_ARGS) (length (cdr CLI_ARGS)))) 
;(define DataBase (open-database (pam 'database))) 
;(apply cmd DataBase (cdr CLI_ARGS))
