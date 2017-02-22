#ifndef SYMBOLTABLE_H
#define SYMBOLTABLE_H

struct entry_s {
	int type;		
        char *lexeme;		
	float value;		
        struct entry_s *next;	
};
typedef struct entry_s entry_t;

struct table_s {
        int t_size;             
        entry_t *t_head;        
};
typedef struct table_s table_t;

table_t table;

void init_table();

entry_t *create_entry(int type, const char *lexeme, float value);

void put_entry(entry_t *new_entry);

entry_t *get_entry(const char *lexeme);

float get_value(const char *lexeme);

void set_value(const char *lexeme, float value);

int get_type(const char *lexeme);

#endif
