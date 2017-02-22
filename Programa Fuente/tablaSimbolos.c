#include "tablaSimbolos.h"
#include <stdlib.h>
#include <string.h>

void init_table() {
	table.t_size = 0;
	table.t_head = (entry_t *)0;
}

entry_t *create_entry(int type, const char *lexeme, float value) {
	entry_t *new_entry = (entry_t *)malloc(sizeof(entry_t));

	new_entry->type = type;
	new_entry->lexeme = strdup(lexeme);
	new_entry->value = value;
	new_entry->next = NULL;

	return new_entry;
}

void put_entry(entry_t *new_entry) {
	new_entry->next = table.t_head;
	table.t_head = new_entry;

	table.t_size++;
}

entry_t *get_entry(const char *lexeme) {
	/* return:
	   NULL - not found
	   pointer to the entry found */

	entry_t *cur;
	for( cur = table.t_head;
		 cur != (entry_t *)0;
		 cur=cur->next) {

		if(strcmp(cur->lexeme, lexeme) == 0) {
			return cur;
		}
	}
	return 0;
}

float get_value(const char *lexeme) {
	entry_t *cur = get_entry(lexeme);

	return get_entry(lexeme) ? (*cur).value : 0;
}

void set_value(const char *lexeme, float value) {
	entry_t *cur = get_entry(lexeme);

	(*cur).value = value;
}

int get_type(const char *lexeme) {
	entry_t *cur = get_entry(lexeme);

	return get_entry(lexeme) ? (*cur).type : 0;
}
