/*****************************************************************************
 *
 *
 *		I N P U T . H 		Interface to input.c
 *
 *		Created 		S.K.Robinson  14/5/92
 *
 *
 *****************************************************************************
 */

extern char *prompt;
/*
 * input.c function prototypes
 */
extern int stack_string_input(char *s, int length);
extern void  unstack_input(void);
extern int   stack_prompt(char *newprompt);
extern int   unstack_prompt(void);
extern void  clear_promptstack(void);
extern value init_input(void);
extern int   interactive (void);
extern int   terminal_output (void);
extern int   sense_vms (FILE *fp);
extern int   icl_input(void);
extern void  icl_unput(int i);
extern value icl_gets  (int interruptable, char *whofor, char *buffer,
			int buffersize, char *prompt, char *dflt);
extern value clearupafterinput(char *whofor);
extern value icl_fgets (char *filename, char *buffer, int buffersize,
			value vfp);
extern value clearupafterread(value file, char *whofor, char *filename);
extern value iclopenfile(char *whofor, char *filename, char *openmode);
extern value do_load (char *whofor, char *filenametobeloaded);
extern node *compose_syntaxerr (char *message);
