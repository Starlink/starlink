/******************************************************************************
 *
 *		S Y M T A B . H
 *
 * Interface to ICL symtab.c module
 *
 *		Created : 		S.K. Robinson 8/1/92
 *		Tidied:			B.K. McIlwrath 12/11/93
 *
 *
 ******************************************************************************
 */
struct _symtab;

extern value value_symbols(void);
extern value savesymboltablecontext(node *procnode);
extern value restoresymboltablecontext (void);
extern value value_emptysymtab(void);
extern value lookfor_variable_value(struct _symtab *sym, char *name);
extern value lookup_variable_value (char *name);
extern value lookup_procvariable_value (node *procnode, char *name);
extern node *lookup_symbol (char *name, int type);
extern node *lookup_proc (char *name);
extern value put_symbol(value symb, char *name, int type, node *n);
extern value store_symbol (char *name, int type, node *n);
extern value install_abbrevs (char *name,  node *n);
extern value assign_helper (node *var, value val);
extern value assign_helper1 (node *var, value val);
extern int   delete_symbol (char *name, int type);
extern void  delete_sym_parameters (value symbval);
extern value listaproc(char *name);
extern value listallprocs  (void);
extern value listprocsvars (char *name);
extern value fileaproc(char *procname, char *filename, char *command);
