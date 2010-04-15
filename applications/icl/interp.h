/******************************************************************************
 *
 *		I N T E R P . H 	Interface to interp.c
 *
 *		Created :		S.K. Robinson 8/1/92
 *		Tidied :		B.K. McIlwrath 15/11/93
 *
 ******************************************************************************
 */

/*
 * Possible actions for *_interpret functions
 */
#define OP_INTERPRET	1
#define OP_PRINT	2
#define OP_DESTROY	3
#define OP_FORMAT	4

/******************************************************************************
 *
 * The *_interpret routines keep track of the procedure and function names
 * which have been called. If an exception occurs, this stack is not popped
 * until the exception is handled. This allows a stack trace to be printed if
 * the exception is not handled.
 * Some things use the call_stack when formatting exceptions in order to insert
 * a name.
 *
 ******************************************************************************
 */

/* extern char *call_stack[];  now static to main.c */
/* extern int call_stack_level; */

/******************************************************************************
 *
 * The arguments to a PROC or function are passed in a global array arglist[].
 * arglist[] contains the arguments and nargs how many there are.
 * all_args is a pointer to the parse tree which contained them, as this is
 * sometimes useful for passing them on elsewhere.
 *
 ******************************************************************************
 */
extern node *arglist[], *all_args;
extern int nargs;
/*
 * 'node todo' is used by the parser and is non-NULL if there is something
 * to do.
 */
extern node *todo;

/*
 * Function prototypes
 */
extern void  get_args(node *n);
extern void  setup_task_hds(node *n, int *hdsvars);
extern value nargs_should_be(int n);
extern value nargs_in_range(int low, int high);
extern value concat_args ( int from );
extern value interpret(node * n);
extern void  print(node *n);
extern void  destroy(node *n);
extern value interpret_to_real(node *n);
extern value interpret_to_numeric(node *n);
extern value interpret_to_integer(node *n);
extern value interpret_to_string(node *n);
extern value explist_interpret(node *n, int op);
extern value abbut_interpret(node *n, int op);
extern value abbut_qstring_interpret(node *n, int op);
extern value parameter_interpret(node *n, int op);
extern value exception_interpret(node *n, int op);
extern value exception_list_interpret(node *n, int op);
extern value procedure_interpret(node *n, int op);
extern value builtin_interpret(node *n, int op);
extern value defstring_interpret(node *n, int op);
extern value defproc_interpret(node *n, int op);
extern value help_interpret(node *n, int op);
