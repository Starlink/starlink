typedef void *PORTPTR;   /* should be void  */
#define ICL_BUFSIZE 1024
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include <errno.h>
#include "utils.h"	/* ICL utility functions */
#include "value.h"	/* ICL all prevasive 'value' concept! */
/*
 * Commonly used  functions from output.c
 */
extern void systemfail(char *mess);				 /* output.c */
extern void flshbufe(void);					 /* output.c */
extern void bufnewlinee(void);					 /* output.c */
extern void bufstringe(char *mess);				 /* output.c */
extern void outstringe(char *mess);				 /* output.c */

#ifndef TRUE
#define TRUE 1
#endif
#ifndef FALSE
#define FALSE 0
#endif

typedef struct _node
 {
 	value (*interpret)();
 	int n_nodes;
	struct _node **sub;
	value val;
 } node;
/*
 * Node Structure nils.  NODENIL is defined in node.c
 */
#define NODENULL ((node *)0)
extern node *NODENIL;

/*
 * Classes of symbol table entry. These are folded into one symbol table so 
 * that different symbol scopes can be referred to by one symbol table pointer
 *
 */
#define SYM_NOTHING	0
#define SYM_VARIABLE	1
#define SYM_FUNCTION	2
#define SYM_PROC	3
#define SYM_HIDDEN_PROC	4
#define SYM_PARAMETER	5
#define SYM_FILE	6
#define SYM_BUILTIN	7
#define SYM_DEFINED	8
#define SYM_HELP        9

/* The name that the ICL system is known by in the messys/msp layers */

#define ICLSYSTEMNAME ("ICL")
#define ICLDEFAULTPROMPT ("ICL> ")

/* The integer msp queue id for the io subsystem */

extern int iocommand_q;

#ifdef vaxc
#include <perror.h>
#elif !defined(linux)
extern int sys_nerr;
extern char *sys_errlist[];
#endif

#ifndef M_PI
/* amazingly, this is not mentioned in the math.h in ANSI C. (K&R 2) */
#define M_PI	3.14159265358979323846
#endif

#if defined (mips) || defined (vaxc)
#define isexc(val) (isexception (val))
/* the Ultrix compiler will not cope with below */
#else
#define isexc(val) ((val).type == TYPE_EXCEPTION)
#define cntlcexp(val) ((val).type == TYPE_EXCEPTION && \
		     !strncmp((val).u.string, "CNTLC", 5) )
#endif

#ifdef mips
long finite ();
#endif

/* operating system specific - true if floating point overflow generates a signal */

extern int float_can_signal;

/* the top of the prompt stack. This gets stacked when the user is entering
 * code as part of a procedure or control statement.
 */
extern char *prompt;

/* this flag is set by a signal handler when the user causes a CTRLC interrupt
 */
extern int interrupted;

