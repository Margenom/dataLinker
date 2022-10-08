#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <string.h>

/* global Cli variables */
struct params_t {
	char* pname;
	char* pval;
	struct params_t *next;
} *CLI_PARAMS = NULL;
char ** CLI_ARGS  = NULL;
int cli_init (int argc, char* argv[]) {
	int argi = 0;
	for (int i = 0; i < argc; i++) {
		if (argv[i][0] == '-') {
			struct params_t *next = malloc(sizeof (struct params_t));
			char str = argv[i],
				*sep = strchr(str, '=');
			next->next = CLI_PARAMS;
			next->pname = strndup(str, sep? sep-str: strlen(str));
			next->pval = sep? strdup(sep): NULL;
		} else {
			CLI_ARGS = realloc(CLI_ARGS, ++argi * sizeof(char*));
			CLI_ARGS[argi-1] = strdub(argv[i]);
		}
	}
	CLI_ARGS = realloc(CLI_ARGS, ++argi * sizeof(char*));
	CLI_ARGS[argi-1] = NULL;
	return argi;
}


int main(int argc, char*argv[]) {
	cli_init(argc, argv);
}
