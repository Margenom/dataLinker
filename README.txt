Knowlege Base System
Примерный цикл работы с системой
	- создаем фаил ознакомления с ресурсом - не являеться частью системы
	- разбиваем его на заметки в разные файлы (именуем их)
	- связываем первичными отношениями эти заметки, помещая их в систему
	- размещаем ссылки в структуре
	- накидываем дополнительные отношения если надо
	- пользуемся или радуемся

Недостатки же 
	- скрипты и массовое добавление занисей онасно для записей
	- много возни, но при имспользовании бумажных карточек возни больше

Так и чего мне надо для этой системы
	- главный атом это заметка
		- одна заметка = одна идея
		- имя
	- связи с остальными заметками
	- система должна иметь структуру

Как это делать
- хранить в файловой древовидной структуре
#	- заметки храняться в папках по датам
#		- для того чтобы иметь привязку к дате
#		- длительность настраиваеться (<last name> - now > <per>) -> make new dir
	- заметки
		- название utime 
		- просто текстовые файлы 
	- структура определяеться с помощью ссылок 
		- раздел - папка
		- значение - ссылка
			- название файла - utime
			- название ссылки - описание 
		- может и должно содержать обычные файлы
			- 
	- отношения - через фаил на prolog 
		- задаються через уневерсальный указатель ресурса в системе 
			- заметка её дата и часть пути от корня
			- фаил имя и аналогисно
		- типы связей задаёт пользователь
	- категории - отношения одноранговые 

Как работать с системой 
% ресурсы системы
	- создает запись в хранилище и ссылку с именем name (лучше не плодить русурсы а повышать их уонкретность и связанность)
kbs push <namep0>[ .. <namepN>] - закуск эдитора для создания файла с именем (то что введено но пробелы меняются на _)
1kbs res <file> - обьявлякт фаил ресурсом системы и заменяет ссылкой на него
kbs spec [-t=<type|def blob>] <file> - будет сохранен в <res root>/<type|blob>/<utime>
	- востановить копию фаил из базы
2kbs clone <link>
	- удаляеть из системы (лучше не удолять а изменять)
3kbs delele <link> 
% структура
	- копировать и перенести запись по структуре
cp
mv
	- посмотреть структурную схему (как tree)
7kbs tree [<root>] 
% отношения ресурсов
	- востановить ссылку тк ссылка имеет абсолютный путь на фаил при переносе базы она ломаеться
4kbs relink <link name>
	- связать ресурсы системы (res only)
5kbs rel -<name0> [.. -<nameN>] a [b]
	- создать отношение файлов (a,b: link or file or dir)
5kbs srel -<name0> [.. -<nameN>] a [b]
6kbs about [-<name0> [.. -<nameN>]] <link> - посмотреть отношения этого обьекта
kbs trace <term|link|rel> - просмотр ввиде dot граффа
% статистика
kbs stat term <link|file> - посмотреть близость терминологии
kbs gen term - cгерерировать базу термов
kbs stat link <link> - посмотреть необозначенные ссылки
kbs gen link - cгерерировать базу необозначенных ссылок
kbs stat rel <link>
kbs stat between <link0> <link1> [.. <linkN>]
% пользовательские ...
	- запустить repl prolog
0kbs repl
