/******************************************************************************
 *
 *		U N I X . C
 *
 * ICL interface to the UNIX system
 *
 *	History
 *	Created :	S.K.Robinson	21/11/91
 *	Edited  :	S.K.Robinson, I. R. Jenkins  5/5/92
 *			To remove editor temporary file
 *			in proc_edit() using remove().
 *	Tidied and reformatted:
 *			B.K. McIlwrath	23/7/93 + 16/11/93
 *      Remove HELP tp procs.c:
 *                      A.J. Chipperfield 23/12/93
 *      Correct comments and flush on DEFAULT
 *      Also use expand_name on directory
 *                      A.J.Chipperfield  8/2/94
 *
 ******************************************************************************
 */
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <fcntl.h>
#include <time.h>
#include <sys/time.h>
#include <sys/wait.h>
#include <math.h>
#include <termios.h>
#include <signal.h>
#include "icl.h"
#include "interp.h"
#include "icl_io.h"
#include "output.h"

/* node.h included by init_os() only */

extern node *lookup_symbol(char *name, int type);	/* symtab.c */
extern value sys_exception1(char *format, char *arg1);  /* procs.c  */
extern char *editor;					/* procs.c  */
extern value do_load
	(char *whofor, char *filenametobeloaded);	/* input.c  */

#define FUNC_DATE	1
#define FUNC_TIME	2

/******************************************************************************
 * The version of ICL currently running
 ******************************************************************************
 */
extern char *version;					/* main.c */

/******************************************************************************
 * The Operating system of the host
 ******************************************************************************
 */
char *os = "UNIX";

/******************************************************************************
 * Operating system specific - true if floating point overflow
 * generates a signal. Ironically only integer divide by zero does
 * on UNIX. Math is usually IEEE...
 ******************************************************************************
 */
int float_can_signal = 0;

/******************************************************************************
 * Whenever the system() function is used to pass a command to Unix the
 * os_status variable is set to the returned value. This is tested in the
 * ICL function func_status() to impletement the command STATUS
 ******************************************************************************
 */
int os_status=0;

/******************************************************************************
 * pid of the iosubsystem process
 ******************************************************************************
 */
extern int iopid;						/* main.c */

/*****************************************************************************
 *
 *	S E T E N V (char *var, char *valu)
 *
 * Set a UNIX environment variable - while the Unix routine putenv() WILL
 * do this it is not guananteed to be available on all systems.
 * If the value is NULL, then delete the environment variable.
 *
 * environ (char **) is an array of strings made available by the environment
 * each string is of the form name=value.  Certain names have builtin
 * meanings such as PATH, HOME, TERM, SHELL, etc.
 *
 * We search each of the environment strings for a match on "var" and, if
 * found and "value" is null, the current environ is shuffled up to overwrite
 * the entry to be deleted.
 * If a match is found and we are changing it, then we, using malloc(),
 * get space for a new string of the form var=value, copy var and value into
 * it and reset the relevant environ element.
 * If the environment does not contain the var, we recreate a complete new
 * environ array using malloc(), copy the old values into it and add a new
 * var=value entry at the end ensuring the last entry is a null string pointer.
 *
 * Returns a zero if successful
 *
 *****************************************************************************
 */
static value
Setenv(char *var, char *valu)
{
#if HAVE_SETENV && HAVE_UNSETENV
  int retval;
  if (valu != CHARNIL) {
    retval = setenv(var, valu, 1);
    if (retval == 0) {
      return trueval;
    } else {
      return exception("Setenv: ICL has exhausted its memory");
    }
  } else {
    unsetenv(var);
  }
  return trueval;
#else
 /* Should consider optionally using putenv here. I worry
    about memory leaks though */
    extern char **environ;			/* run time environment */
    char **environ_new;
    int ind = 0, i;

    while (environ[ind] != NULL) {
	if (strncmp(environ[ind], var, (int) strlen(var)) == 0) {
	    if (valu == CHARNIL)
		for (; environ[ind] != NULL; ind++)
		    environ[ind] = environ[ind + 1];
	    else {		/* free (environ[ind]); */
		environ[ind] = (char *) malloc((unsigned)
			((int) strlen(var) + (int) strlen(valu) + 2));
		if (environ[ind] == CHARNIL)
		    return exception("Setenv: ICL has exhausted its memory");
		strcpy(environ[ind], var);
		strcat(environ[ind], "=");
		if (valu != CHARNIL)
		    strcat(environ[ind], valu);
	    }
	    return trueval;
	}
	ind++;
    }
    if (valu == CHARNIL)
	return falseval;
    environ_new = (char **) malloc((unsigned) (sizeof(char *) * (ind + 2)));
    if (environ_new == ((char **) 0))
	return exception("Setenv: ICL has exhausted its memory");
    for (i = 0; i < ind; i++)
	environ_new[i] = environ[i];
    environ = environ_new;
    environ[ind] = (char *) malloc((unsigned) (strlen(var) + 2
		+ (valu == CHARNIL? 0 : strlen(valu))));
    if (environ[ind] == CHARNIL)
	return exception("Setenv: ICL has exhausted its memory");
    strcpy(environ[ind], var);
    strcat(environ[ind], "=");
    strcat(environ[ind], valu);
    environ[++ind] = CHARNIL;
    return trueval;
#endif
}

/******************************************************************************
 *
 * 	P R O C _ S E T E N V ()
 *
 * Implements the ICL command SETENV
 *
 * By the time this routine is called get_args() will have been called from
 * builtin_interpret() and this will have set up the arguments to the SETENV
 * command in arglist[].
 * 'nargs' will have been set to the total number of arguments.
 *
 ******************************************************************************
 */
static value
proc_setenv(node *n)
{
    char *variable, *s;
    value val;

    if (nargs < 1)
	return exception("TOOFEWARGS  Not enough arguments for SETENV");
    if (isexc(val = interpret_to_string(arglist[0])))
	return val;
    variable = string_part(val);
    if (nargs > 1) {
	if (isexc(val = interpret_to_string(n->sub[1])))
	    return val;
	s = string_part(val);
    } else
	s = "";
    return (Setenv(variable, s));
}

/******************************************************************************
 *
 * 	P R O C _ U N S E T E N V ()
 *
 * Implements the ICL command UNSETENV
 *
 * The inverse of SETENV - see above
 *
 ******************************************************************************
 */
static value
proc_unsetenv(node *n)
{
    char *variable;
    value val;

    if (nargs < 1)
	return exception("TOOFEWARGS  Not enough arguments for UNSETENV");
    else if (nargs > 1)
	return exception("TOOMANYARGS  too many arguments for UNSETENV");
    if (isexc(val = interpret_to_string(arglist[0])))
	return val;
    variable = string_part(val);
    return (Setenv(variable, CHARNIL));
}

/******************************************************************************
 *
 * 	T I M E _ D A T E (int func)
 *
 * Helper function for the ICL TIME() and DATE() functions.
 *
 * Called with an argument of FUNC_DATE this function uses  time(time_t x) to
 * get the current time (seconds since 00:00:00 GMT 1/1/1970), and then
 * localtime(x) to adjust for time zones and breaks the time up into days
 * months etc. It then forms a string representing the day-month-year.
 *
 * FUNC_TIME uses gettimeofday() to get the time of day as a string HH:MM:SS
 *
 * May return CHARNIL if memory exhausts in strcopy().
 *
 ******************************************************************************
 */
static char *
time_date(int func)
{
    time_t tnow;
    struct tm *now;
    char buf[20];
    static char *month[] = {"JAN", "FEB", "MAR", "APR", "MAY", "JUN",
			    "JUL", "AUG", "SEP", "OCT", "NOV", "DEC"};
    struct timeval tod;

    time(&tnow);
    now = localtime(&tnow);
    if (func == FUNC_DATE)
	sprintf(buf, "%2d-%3s-%4d", now->tm_mday, month[now->tm_mon],
		1900 + now->tm_year);
    else {
	(void) gettimeofday(&tod, (struct timezone *) 0);
	sprintf(buf, "%2d:%02d:%05.2f", now->tm_hour, now->tm_min,
		now->tm_sec + tod.tv_usec / 1e6);
    }
    return strcopy(buf);
}

/******************************************************************************
 *
 *	F U N C _ D A T E - returns the current date in the form  DD-MM-YY
 *
 ******************************************************************************
 */
static value
func_date(void)
{
    char *s;

    if ((s = time_date(FUNC_DATE)) == CHARNIL)
	return exception("SYSERR memory exhausted in DATE()");
    else
	return value_string(s);
}

/******************************************************************************
 *
 *	F U N C _ T I M E - returns the current time in the form  HH:MM:SS
 *
 ******************************************************************************
 */
static value
func_time(void)
{
    char *s;

    if ((s = time_date(FUNC_TIME)) == CHARNIL)
	return exception("SYSERR memory exhausted in TIME()");
    else
	return value_string(s);
}

/******************************************************************************
 *
 *	F U N C _ S T A T U S (void)
 *
 * This function returns the ICL value true of false depending on whether
 * a system command executed OK.
 *
 * Releated to the use of os_command() (which calls system()) or direct use
 * of system().  Use of system() sets the variable os_status to zero if call
 * executed OK, non zero otherwise
 *
 ******************************************************************************
 */
static value
func_status(void)
{
    return value_logical(os_status == 0);
}

/******************************************************************************
 *
 *	F U N C _ O K (void)
 *
 * The ICL function OK is not yet implemented on Unix.
 *
 ******************************************************************************
 */
static value
func_ok(void)
{
    return exception(
		"OSERR  Function OK() not available on UNIX; use STATUS()");
}

/******************************************************************************
 *
 * 	F U N C _ G E T E N V (char *s)
 *
 * getenv(char *s) returns the string associated with the environment
 * variable s  (refer Set_env() for a description of environment
 * variables.
 *
 ******************************************************************************
 */
static value
func_getenv(char *s)
{
    char *t, *p;

    t = getenv(s);
    if (t == CHARNIL)
	p = "";
    else if ((p = strcopy(t)) == CHARNIL)
	    return exception("SYSERR memory exhausted in func_getenv()");
    return value_string(p);
}

/******************************************************************************
 *
 *	F U N C _ G E T _ S Y M B O L (void)
 *
 * The ICL function GET_SYMBOL() is not yet implemented on Unix.
 *
 ******************************************************************************
 */
static value
func_get_symbol(void)
{
    return exception(
	"OSERR  get_symbol() is not available on UNIX; use getenv()");
}

/******************************************************************************
 *
 *	F U N C _ U N I M P L (void)
 *
 * func_unimpl() is used within the ICL symbol table to produce a suitable
 * message when a ICL function is not yet implemented in the Unix version.
 *
 ******************************************************************************
 */
static value
func_unimpl(void)
{
    return exception("IMPLERR  Unimplemented function");
}

/******************************************************************************
 *
 *	P R O C _ O S E R R (void)
 *
 * Function which is called when an ICL command which is inappropriate to the
 * Unix version is used.
 *
 ******************************************************************************
 */
static value
proc_oserr(node *n)
{
    return exception("OSERR  not available on UNIX");
}

/******************************************************************************
 *
 * 	F U N C _ F I L E _ E X I S T S (char *s)
 *
 * The function FILE_EXISTS(char *S) returns true if the file, with name s,
 * exists, false otherwise
 *
 * Uses the Unix supplied function access().
 *
 * Returns 0 on success, -1 on failure.
 *
 ******************************************************************************
 */
static value
func_file_exists(char *s)
{
    char *fullname;
    if (!expand_name(&fullname, s))
	return falseval;
    else
	return value_logical(access(fullname, R_OK) == 0);
}

/******************************************************************************
 *
 *	O S _ C O M M A N D (char *command)
 *
 * Issue a command to Unix.
 *
 * Uses the fact that the iosubsystem process resets the terminal
 * characteristics to before stopping itself on receipt of an
 * IO_COMMAND_SUSPEND from icl. The iosubsystem then stops and awits
 * a SIGCONT signal before proceeding.
 *
 * Returns either the status returned from the shell running the command
 * or -1 if a system service fails.
 *
 ******************************************************************************
 */
int
os_command(char *command)
{
    int pid, status=-1, iostatus=0;
    struct sigaction oact;
    char *shell;
    extern void sendtoiosubsystem(int command, int info, char *message);
								/* output.c */

    if( (shell = getenv("SHELL")) == CHARNIL)
	shell = "csh";
    sigaction(SIGCHLD, NULL, &oact);
    signal(SIGCHLD, SIG_DFL);
/*
 * The Unix command is likely to write directly to the tty. We suspend the
 * io-subsystem process by sending it an IO_COMMAND_SUSPEND command.
 * The io-subsustem resets the terminal then suspends itself waiting for
 * a SIGCONT signal.
 */
    sendtoiosubsystem(IO_COMMAND_SUSPEND,0,"");
    do
	if (waitpid(iopid, &iostatus, WUNTRACED|WNOHANG) < 0 ) {
	    perror("os_command - io - waitpid");
	    break;
	}
    while(!WIFSTOPPED(iostatus));	  /* Wait for io process to stop */
/*
 * In case we accidently run an ADAM task as a Unix command annul the
 * environment variable which tells the task it is being run from ICL
 */
    (void) Setenv("ICL_TASK_NAME", CHARNIL);
    if ( (pid = fork()) < 0 ) {
	perror("os_command() fork error");
	sigaction(SIGCHLD, &oact, NULL);
        return FALSE;
    } else if (pid == 0) {		/* Child */
	if (command[0] == '\0') {
	    if( execlp(shell, shell, (char *) 0) < 0)
		perror("os_command() shell exec failure");
	} else if( execlp(shell, shell, "-c", command, (char *) 0) < 0)
	    perror("os_command() shell exec failure");
	exit(1);
    } else {				/* Parent */
        if (waitpid(pid, &status, 0) < 0 ) /* Wait to get child status */
	    perror("os_command - waitpid");
    }
    kill(iopid, SIGCONT);	/* restart io-subsystem */
    sigaction(SIGCHLD, &oact, NULL);

    return (status <= 255? status: 255-status);
}

/******************************************************************************
 *
 *	P R O C _ S H (node *n)
 *
 * Implements the ICL command "SH [unix_command]"
 *
 * The Unix command may have several parameters.  The whole abbut_interpret()
 * node list formed by the parser for this sequence is pointed to by 'n' and
 * interpreting this causes the command string and all of its parameters
 * to be concatenated together as strings.
 *
 * If no Unix command is given, then the environment variable SHELL is used
 * to run a shell which will accept a series of Unix commands and return to
 * ICL on logout.
 *
 ******************************************************************************
 */
value
proc_sh(node *n)
{
    char *comm;
    value val;

    if (n != NODENIL) {
	if (isexc(val = interpret_to_string(n)))
	    return val;
	comm = string_part(val);
    } else
	comm = "";
    os_command(comm);
    return trueval;
}

/******************************************************************************
 *
 *	P R O C _ D C L (node *n)
 *
 * Implements the ICL command "DCL [command]"
 *
 * We detect direct mode as the system maintains a stack which is pushed by
 * the name of a procedure on each proc call, and popped on each procedure
 * return. The routine currentproc() returns the name of the currently active
 * procedure or CHARNIL if in direct mode.
 *
 * proc_sh(n) issues the string derived from interpreting the abstract syntax
 * nodes pointed to by n, seen as a string, to Unix.
 *
 ******************************************************************************
 */
static value
proc_dcl(node *n)
{
    extern char *currentproc(void);			/* main.c */
    char *name;

    if ( (name = currentproc()) != CHARNIL)
	return exception(
   "OSERR VMS commands ($, DCL and SPAWN) not accepted in Unix procedures");
    else {
	bufstring(
	"[OSWARN -  Replace VMS only command by Unix command (SH or !)]\n");
	flshbuf();
	return proc_sh(n);
    }
}

/******************************************************************************
 *
 *	P R O C _ E D I T (node *n)
 *
 * Implements the ICL command "EDIT [proc]"
 *
 * Edit procedure 'proc' using the preferred editor (default or SET in
 * proc_set() by user issuing a SET EDITOR command). If the name of the proc
 * is unchanged during the editing session the new version replaces the old.
 * If, however, the name is changed a new procedure results, with the old
 * remaining.
 *
 * The default is set in the environment variable EDITOR or, failing that, vi.
 *
 * We first interpet() the argument in n which will point to the abstract
 * syntax of the proc arguement to the EDIT command. This, seen as a string,
 * forms "name". We lookup the procedure to see if it exists using
 * lookup_symbol(name, SYMPROC).
 * If it does we open a temporary file (name iclout.icldddddd
 * where ddddd is the current process's pid and save the procedure into it
 * using fileaproc().
 * We then call the editor passing it the file iclout.icldddddd
 * (note even if the procedure doe snot exist this still ocurs to allow a new
 * procedure  to be input).
 * On return from the editor with a success status , we use do_load() to
 * parse the file's contents.
 *
 ******************************************************************************
 */
static value
proc_edit(node *n)
{
    extern value fileaproc(char *procname, char *filename,
			   char *command);			/* symtab.c */
    char *name;
    char filename[ICL_BUFSIZE], command[ICL_BUFSIZE];
    node *proc;
    value val;

    if (isexc(val = interpret_to_string(n)))
	return val;
    name = string_part(val);
    sprintf(filename, "iclout%d.icl", getpid());
    if (proc = lookup_symbol(name, SYM_PROC))
	fileaproc(name, filename, "EDIT");
    sprintf(command, "%s %s", editor ? editor : (getenv("EDITOR") ?
				getenv("EDITOR") : "vi"), filename);
    if (os_command(command) != 0)
	val = exception("EDIT - system editor command failed");
    else
	val = do_load("EDIT", filename);
    (void) remove(filename);
    return val;
}

/******************************************************************************
 *
 *
 *	P R O C _ D E F A U L T (node *n)
 *
 * Implements the ICL command "DEFAULT directory".
 *
 * Set the current working directory for the process running ICL.
 * The directory may be specified in any of the forms accepted by the Unix
 * chdir command.
 *
 * DEFAULT with no parameter displays the current default directory.
 *
 * On entry, n will point to the parsed abstract syntax node form of
 * "directory".
 * This is interpreted and, seen as a string, passed to chdir().
 *
 * When 'n' is null or after changing directory, getcwd() is used to get the
 * current working directory.
 *
 ******************************************************************************
 */
static value
proc_default(node *n)
{
    char *dir, *fulldir, *taskname;
    value val;
    extern value adam_control(char *taskname, char *action, char *message);
								/* adam.c */

    if (nargs > 2)
	return exception("TOOMANYARGS  Too many arguments for DEFAULT");
    if (nargs != 0) {
	if (isexc(val = interpret_to_string(arglist[0])))
	    return val;
	dir = string_part(val);
	if (!expand_name(&fulldir, dir))
	    return exception2(
		"DEFAULTERR  failed to expand name \"%s\" - %s", dir, fulldir);
    }
    if (nargs == 0)
	expand_name(&fulldir, "$cwd");
    else if (nargs == 1) {
	if (chdir(fulldir))
	    return sys_exception1(
		"DEFAULTERR  chdir() failed to directory \"%s\"", fulldir);
        if (isexc(val = adam_control("CACHED", "DEFAULT", fulldir)))
	   return val;
    } else {
	if (isexc(val = interpret_to_string(arglist[1])))
	    return val;
	taskname = string_part(val);
        if (isexc(val = adam_control(taskname, "DEFAULT", fulldir)))
	   return val;
    }
    if (nargs != 2) {
	outfpformatstring("ICL default Directory: %s\n", fulldir);
	flshbuf();
    }
    return trueval;
}

/******************************************************************************
 *
 *	P R O C _ C D (node *n)
 *
 * Implements the ICL command "CD [directory]".
 *
 * Change the current directory.
 *
 * On entry n will point to the parsed abstract syntax node form of directory.
 * This is interpreted and, seen as a string, passed to chdir to change
 * directory.  If "directory" is not provided we change directory to that
 * associated with the HOME environment variable.
 *
 ******************************************************************************
 */
static value
proc_cd(node *n)
{
    char *dir = CHARNIL;
    char *fulldir = CHARNIL;
    value val;

    if (n != NODENIL) {
	if (isexc(val = interpret_to_string(n)))
	    return val;
	dir = string_part(val);
	if (!expand_name(&fulldir, dir))
	    return exception2("cd: failed to expand name \"%s\" - %s",
			     dir, fulldir);
    } else
	fulldir = getenv("HOME");
    if (chdir(fulldir))
	return sys_exception1("cd: chdir() failed to directory \"%s\"", dir);
    else
	return trueval;
}

/******************************************************************************
 *
 *	P R O C _ V E R S I O N (node *n)
 *
 * Implements the ICL "VERSION" command.
 *
 ******************************************************************************
 */
value
proc_version(node *n)
{
    extern char *version;

    if (nargs != 0)
	return exception("TOMANYARGS  Too many arguments to VERSION");
    bufstring("ICL (");
    bufstring(os);
    bufstring(") Version ");
    bufstring(version);
    bufstring("\n");
    flshbuf();

    return trueval;
}

/******************************************************************************
 *
 *	I N I T _ O S (void)
 *
 * Called from main() to initialise the symbol_table to contain entries for
 * Unix dependant functions.
 *
 ******************************************************************************
 */
value
init_os(void)
{
#include "node.h"
extern value store_symbol(char *name, int type, node *n);	/* symtab.h */

    value val;

    if (
	(isexc(val = store_symbol("DATE", SYM_FUNCTION,
				node_nonary_func(func_date))))		||
	(isexc(val = store_symbol("TIME", SYM_FUNCTION,
				node_nonary_func(func_time))))		||
	(isexc(val = store_symbol("GETNBS", SYM_FUNCTION,
				node_unary_string_func(func_unimpl))))	||
	(isexc(val = store_symbol("FILE_EXISTS", SYM_FUNCTION,
				node_unary_string_func(func_file_exists)))) ||
	(isexc(val = store_symbol("OK", SYM_FUNCTION,
				node_unary_func(func_ok))))		||
	(isexc(val = store_symbol("GET_SYMBOL", SYM_FUNCTION,
				node_unary_func(func_get_symbol))))	||
	(isexc(val = store_symbol("GETENV", SYM_FUNCTION,
				node_unary_string_func(func_getenv))))	||
	(isexc(val = store_symbol("STATUS", SYM_FUNCTION,
				node_nonary_func(func_status))))	||
	(isexc(val = store_symbol("ALLOC", SYM_BUILTIN,
				node_builtin(proc_oserr))))		||
	(isexc(val = store_symbol("ALL", SYM_BUILTIN,
				node_builtin(proc_oserr))))		||
	(isexc(val = store_symbol("DEALLOC", SYM_BUILTIN,
				node_builtin(proc_oserr))))		||
	(isexc(val = store_symbol("DEALL", SYM_BUILTIN,
				node_builtin(proc_oserr))))		||
	(isexc(val = store_symbol("MOUNT", SYM_BUILTIN,
				node_builtin(proc_oserr))))		||
	(isexc(val = store_symbol("MOU", SYM_BUILTIN,
				node_builtin(proc_oserr))))		||
	(isexc(val = store_symbol("DISMOUNT", SYM_BUILTIN,
				node_builtin(proc_oserr))))		||
	(isexc(val = store_symbol("DISMOU", SYM_BUILTIN,
				node_builtin(proc_oserr))))		||
	(isexc(val = store_symbol("DCL", SYM_BUILTIN,
				node_builtin(proc_dcl))))		||
	(isexc(val = store_symbol("SPAWN", SYM_BUILTIN,
				node_builtin(proc_dcl))))		||
	(isexc(val = store_symbol("CD", SYM_BUILTIN,
				node_builtin(proc_cd))))		||
	(isexc(val = store_symbol("SH", SYM_BUILTIN,
				node_builtin(proc_sh))))		||
	(isexc(val = store_symbol("DEFAULT", SYM_BUILTIN,
				node_builtin(proc_default))))		||
	(isexc(val = store_symbol("DEF", SYM_BUILTIN,
				node_builtin(proc_default))))		||
	(isexc(val = store_symbol("SETENV", SYM_BUILTIN,
				node_builtin(proc_setenv))))		||
	(isexc(val = store_symbol("UNSETENV", SYM_BUILTIN,
				node_builtin(proc_unsetenv))))		||
	(isexc(val = store_symbol("EDIT", SYM_BUILTIN,
					     node_builtin(proc_edit))))	||
	(isexc(val = store_symbol("VERSION", SYM_BUILTIN,
					node_builtin(proc_version))))   )
	return (val);
    else
	return (trueval);
}
