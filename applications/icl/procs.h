extern int precision;
extern int messages;
extern int autoload;
extern char *editor;
extern char *helpfile;
extern int save, checkpars, trace, os_warning;
/*
 * Function prototypes
 */
extern value sys_exception (char * mess);
extern value sys_exception1(char *format, char *arg1);
extern value sys_exception2(char *format, char *arg1, char *arg2);
extern value proc_clear(node *n);
extern value proc_locate(node *n);
extern value proc_set (node *n);
extern value proc_list(node *n);
extern value proc_vars(node *n);
extern value proc_procs(node *n);
extern value proc_load(node *n);
extern value proc_save(node *n);
extern value proc_exit(node *n);
extern value proc_signal(node *n);
extern value proc_delete(node *n);
extern value proc_defstring(node *n);
extern value proc_defproc(node *n);
extern value init_procs (void);
