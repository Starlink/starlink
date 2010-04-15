/*****************************************************************************
 *
 * 	PROCS.C	 - Routines associated with ICL built-in commands
 *
 *	History
 *	Created :	S.K.Robinson	15/11/91
 *	Tidied, improved comments and reformatted by 'indent'
 *			B.K.McIlwrath	22/07/93 + 15/11/93
 *      Added HELP :    A.J.Chipperfield  23/12/93
 *      Revise SET so all have NO facility:    A.J.Chipperfield 10/1/94
 *      Added DEFHELP : A.J.Chipperfield 12/1/94
 *      Use starfile routine (utils.c) to find help file
 *                      A.J.Chipperfield 3/1/95
 *      Use ems1_starf_c to find help file
 *                      A.J.Chipperfield 3/8/95
 *      Change ems1_starf_c to ems1Starf
 *                      A.J.Chipperfield 15/6/99
 *
 ******************************************************************************
 */
#include <signal.h>
#include <stdio.h>
#include <string.h>
#include <sys/types.h>
#include <sys/wait.h>
#include "icl.h"
#include "interp.h"
#include "node.h"
#include "procs.h"
#include "symtab.h"
#include "icl_io.h"
#include "output.h"
#include "ems.h"

extern void sendtoiosubsystem(int command, int info, char *message);
								/* output.c */

/******************************************************************************
 *
 *	SET command variables
 *
 * The SET command is used to control various features of the state of ICL.
 *
 * SET ATTRIBUTES attributes
 *    Sets the attributes for text written with the LOCATE command -
 *    'attributes' is a string containing any combination of the letters
 *    D (double size), B (bold), R (reverse), U (underlined) and F (flashing)
 *
 * SET CHECKPARS, SET NOCHECKPARS
 *    Switches on/off the checking of parameters supplied to ICL procedures.
 *    ICLs default is CHECKPARS so an exception occurs if too few parameters
 *    are provided on a procedure call. NOCHECKPARS allows the procedure to
 *    to use the UNDEFINED function to test if the parameter was supplied
 *    and to prompt if not.  Only trailing parameters can be omitted in this
 *    fashion.
 *
 * SET EDITOR name
 *    Sets the editior to be used for editing ICL files.  'name' must be one
 *    of TPU,EDT or LSE.  The default is TPU.
 *
 * SET MESSAGES, SET NOMESSAGES
 *    These commands control whether or not task loading messages are output
 *    when ADAM tasks are loaded,  By default messages are output.
 *
 * SET PRECISION digits
 *    Sets the number of decimal digits precision for unformatted conversions
 *    of real values to strings.  The default is 6 but may be SET to 1 - 16.
 *
 * SET PROMPT string
 *    Specifies a replacement for the default ICL> prompt.
 *
 * SET SAVE, SET NOSAVE
 *    Controls whether the system saves the current procedures on exit.  The
 *    default is save defined ptocedures to the file SAVE.ICL.
 *
 * SET SCREEN n , SET NOSCREEN
 *    Sets the number of lines in the scrolling area of the screen (default 8).
 *    NOSCREEN sets the screen to normal I/O mode.
 *
 * SET TRACE, SET NOTRACE
 *    Turns on/off the tracing of procedures,  Default is off.
 *    When turned on, each line of the procedure is output on the terminal
 *    prior to execution.
 *
 ******************************************************************************
 */

/******************************************************************************
 *
 *	SET data structure
 *
 * This structure holds the current values of the arguments to the SET command.
 * If the string field points at a string, the next SET argument sets that
 * string otherwise, the variable value is set true (false if the SET command
 * begins with NO).
 * If the string field points at 'numeric', the next SET argument is a number
 * defining the value.
 *
 ******************************************************************************
 */

static char *numeric = 0;	/* this is a place-holder - used for its
				 * address */
static char *attributes = "";	/* attributes of screen mode text
				 * highlighting */
int autoload = 1;               /* Automatic loading of tasks */
char *editor = (char *) 0;	/* user's choice of text editor */
char *helpfile = (char *) 0;	/* name of source of help */
int messages = 1;	/* messages about loading tasks */
int precision = 6;		/* default precision of REAL printing */
extern char *prompt;		/* prompt string for user, owned by input.c */
int save = 1;			/* save procedures automatically on EXIT */
int checkpars = 1;		/* check parameters all provided as expected */
int trace = 0;			/* TRACE procedures when executing them */

static struct comms {
    char *name;
    char **string;
    int *value;
}   set_commands[] =

{
    {
	"attributes", (char **) &attributes, (int *) 0
    },
    {
	"autoload", (char **) 0, &autoload
    },
    {
	"editor", &editor
    },
    {
	"helpfile", &helpfile
    },
    {
	"messages", (char **) 0, &messages
    },
    {
	"checkpars", (char **) 0, &checkpars
    },
    {
	"precision", &numeric, &precision
    },
    {
	"prompt", &prompt
    },
    {
	"save", (char **) 0, &save
    },
    {
	"trace", (char **) 0, &trace
    },
    {
	0
    }
};

/******************************************************************************
 *
 *	EXCEPTION routines
 *
 * sys_exception, sys_exception1 & sys_exception2 are a series of routines
 * designed to generate ICL exceptions from system exceptions
 *
 ******************************************************************************
 */

/******************************************************************************
 *
 *	S Y S _ E X C E P T I O N (char *mess)
 *
 ******************************************************************************
 */
value
sys_exception(char *mess)
{
    char *buf;
    const char *qual1;
    value new;

    qual1 = ((errno < 65535 && errno >= 0) ? strerror(errno) :
					     "unknown error");
    if ((buf = (char *) malloc(((unsigned int)
				(strlen(mess) + strlen(qual1) + 3))))
				== CHARNIL)
	return exception("SYSERR memory exhausted in sys_exception()");
    sprintf(buf, "%s: %s", mess, qual1);
    new = exception(buf);
    (void) free(buf);
    return new;
}

/******************************************************************************
 *
 *	S Y S _ E X C E P T I O N 1 (char *format, char *arg1)
 *
 ******************************************************************************
 */
value
sys_exception1(char *format, char *arg1)
{
    char *buf, *buf2;
    const char *qual1;
    value new;

    qual1 = ((errno < 65535 && errno >= 0) ? strerror(errno) :
					     "unknown error");
    if ((buf = (char *) malloc(((unsigned int)
				(strlen(format) + strlen(arg1)+ 1))))
				== CHARNIL)
	return exception("SYSERR memory exhausted in sys_exception1()");
    sprintf(buf, format, arg1);
    if ((buf2 = (char *) malloc(((unsigned int)
				(strlen(buf) + strlen(qual1) + 3))))
				== CHARNIL)
	return exception("SYSERR memory exhausted in sys_exception1()");
    sprintf(buf2, "%s: %s", buf, qual1);
    new = exception(buf2);
    (void) free(buf);
    (void) free(buf2);
    return new;
}

/******************************************************************************
 *
 *	S Y S _ E X C E P T I O N 2 (char *format, char *arg1, char *arg2)
 *
 ******************************************************************************
 */
value
sys_exception2(char *format, char *arg1, char *arg2)
{
    char *buf, *buf2;
    const char *qual1;
    value new;

    qual1 = ((errno < 65535 && errno >= 0) ? strerror(errno) :
					     "unknown error");
    if ((buf = (char *) malloc(((unsigned int)
			(strlen(format) + strlen(arg1) + strlen(arg2) + 1))))
			== CHARNIL)
	return exception("SYSERR memory exhausted in sys_exception2()");
    sprintf(buf, format, arg1, arg2);
    if ((buf2 = (char *) malloc(((unsigned int)
				(strlen(buf) + strlen(qual1) + 3))))
				== CHARNIL)
	return exception("SYSERR memory exhausted in sys_exception2()");
    sprintf(buf2, "%s: %s", buf, qual1);
    new = exception(buf2);
    (void) free(buf);
    (void) free(buf2);
    return new;
}

/******************************************************************************
 *
 *	P R O C _ C L E A R (node *n)
 *
 * Handles the ICL command "CLEAR Row Column"
 *
 * We first evaluate the Row and Column arguments using interpret_to_integer()
 * using the linearised argument list arglist[].
 *
 * These are then sent to the iosubsystem using sendtoiosubsystem()
 *
 ******************************************************************************
 */
value
proc_clear(node *n)
{
    value val;
    int rs, re;

    if (nargs > 0) {
	if (isexc(val = interpret_to_integer(arglist[0])))
	    return exception("INVLDINT CLEAR: First argument not integer row");
	rs = integer_part(val);
	if (rs < 0)
	    return exception("INVLDINT CLEAR: Start Row not positive");
    } else
	rs = 0;
    if (nargs > 1) {
	if (isexc(val = interpret_to_integer(arglist[1])))
	    return exception("INVLDINT CLEAR: Second argument not integer row");
	re = integer_part(val);
	if (re < rs || re > 255)
	    return exception(
		"INVLDINT CLEAR: End column less than start column or >255)");
    } else
	re = 255;
    sendtoiosubsystem(IO_COMMAND_CLEAR, rs * 256 + re, "");
    return trueval;
}

/******************************************************************************
 *
 *	P R O C _ L O C A T E (node *n)
 *
 * Handles the ICL command "LOCATE Row Column Message"
 *
 * We first evaluate the Row and Column arguments using interpret_to_integer()
 * using the linearised argument list arglist[].  We then use concat_args()
 * to loin together the remaining arguments into a single string 'message'
 * which is then sent to the iosubsystem using sendtoiosubsystem().
 *
 ******************************************************************************
 */
value
proc_locate(node *n)
{
    value val;
    int r, c;
    char *mess;

    if (nargs < 2)
	return exception(
	"TOOFEWPARS  LOCATE: (Row,Column) position arguments required");
    if (isexc(val = interpret_to_integer(arglist[0])))
	return exception("INVLDINT LOCATE: First argument not integer row");
    r = integer_part(val);
    if (r < 0)
	return exception("INVLDINT LOCATE: First argument not positive");
    if (isexc(val = interpret_to_integer(arglist[1])))
	return exception("INVLDINT LOCATE: Second argument not integer column");
    c = integer_part(val);
    if (c < 0)
	return exception("INVLDINT LOCATE: Second argument not positive");
    if (c > 255)
	return exception("INVLDINT LOCATE: column not within screen area");
    if (nargs != 2) {
	if (isexc(val = concat_args(2)))
	    return val;
	mess = string_part(val);
    } else
	mess = "";
    sendtoiosubsystem(IO_COMMAND_LOCATE, r * 256 + c, mess);
    return trueval;
}

/******************************************************************************
 *
 *	P R O C _ S E T (node *n)
 *
 * Implements the ICL "SET" command.
 *
 * Using the comms structure it searches for the relevant SET command
 * using arglist[] which by this time has been set to the list of arguments
 * given to the set command (see get_args() in interp.c()).
 *
 * If it finds the command it checks to see if the string member of the comms
 * structure is the address of numeric. If so then a numeric argument is
 * expected and the value member points to the integer to be set to the
 * argument value.
 *
 * If not numeric then, if a string argument is expected the string member is
 * non nil and points to the char * variable to be set.
 * If the string argument is nil then a flag is being set on/off and the value
 * member points to the integer to be set to 1 (on) or 0 (off).
 *
 * If the SET command is not found on a first pass but the argument begins
 * with "NO" then we search again for the string after the "NO".
 * If a match is now found then the relevant flag (pointed to by the value
 * member) is set off.
 *
 ******************************************************************************
 */
value
proc_set(node *n)
{
    int i;
    char *comm;
    value val, val1;

    if (nargs == 0)
	return exception("TOOFEWPARS  SET: argument required");
    if (isexc(val = interpret_to_string(arglist[0])))
	return val;
    comm = string_part(val);

/* Handle SET SCREEN as a special case */
    if (strcasecmp("screen", comm) == 0) {
	if (nargs < 2)
	    sendtoiosubsystem(IO_COMMAND_SETSCREEN, 8, ""); /* in output.c */
	else {
	    if (isexc(val = interpret_to_integer(arglist[1])))
		return val;
	    sendtoiosubsystem(IO_COMMAND_SETSCREEN, integer_part(val), "");
	}
	return trueval;
    }

/* Handle normal cases */
    for (i = 0; set_commands[i].name; i++)
	if (strcasecmp(set_commands[i].name, comm))
	    continue;
	else if (set_commands[i].string == &numeric) {
	    if (nargs < 2)
		return exception1(
			"TOOFEWPARS  SET %s: argument required", comm);
	    if (isexc(val = interpret_to_integer(arglist[1])))
		return val;
	    *set_commands[i].value = integer_part(val);
	    return trueval;
	} else if (set_commands[i].string) {
	    if (nargs < 2)
                if (strcasecmp("screen", comm) == 0) {
                    *set_commands[i].string = CHARNIL;
                    return trueval;
                } else
  		  return exception1("TOOFEWPARS  SET %s: argument required",
				comm);
	    if (isexc(val1 = interpret_to_string(arglist[1])))
		return val1;
	    if ((*set_commands[i].string = strcopy(string_part(val1))) ==
						   CHARNIL)
		return exception("SYSERR  memory exhausted in SET command");
	    else
		return trueval;
	} else {
	    *set_commands[i].value = 1;
	    return trueval;
	}

/* Check for NO cases */
    if (strncasecmp(comm, "no", 2) == 0) {
	comm += 2;
	if (strcasecmp("screen", comm) == 0) {
	    sendtoiosubsystem(IO_COMMAND_SETSCREEN, (-1), ""); /* in output.c */
	    return trueval;
	}
	for (i = 0; set_commands[i].name; i++)
	    if (strcasecmp(set_commands[i].name, comm) == 0 ) {
/* If the value is a string, set it to CHARNIL; otherwise set it to 0 */
	      if ( !set_commands[i].string)
/* Simple flag */
		*set_commands[i].value = 0;
              else if (set_commands[i].string == &numeric)
/* Numeric (currently only precision so set default 6) */
		*set_commands[i].value = 6;
              else
/* String */
	        if (strcasecmp(comm, "prompt") == 0 )
                   *set_commands[i].string = strcopy("ICL> ");
                else
                   *set_commands[i].string = CHARNIL;

	      return trueval;
            }
    }
    return exception1("INVSET  SETX: invalid option \"%s\"\n", comm);
}

/******************************************************************************
 *
 *	P R O C _ L I S T (node *n)
 *
 * Implements the ICL "LIST procedure" command.
 *
 * This routine interprets the argument to the list command as a string and
 * then calls listaproc() (symtab.c) to perform the actual listing.
 *
 ******************************************************************************
 */
value
proc_list(node *n)
{
    value val;

    if (n == NODENIL)
	return exception("PROCERR LIST: procedure name not given");
    if (isexc(val = interpret_to_string(n)))
	return val;
    else {
	val = listaproc(string_part(val));
	flshbuf();
	return val;
    }
}

/******************************************************************************
 *
 *	P R O C _ V A R S (node *n)
 *
 * Implements the ICL "VARS [proc]" command.
 *
 * Lists the variables of procedure 'proc' with their current types and values
 * or with no argument lists the direct mode variables.
 *
 * The node argument 'n' argument points to the expression node of the
 * argument to VARS. If not nil, we call interpret() and then listprocsvars()
 * with the result as a string.
 *
 * If nil, we call listprocsvars(CHARNIL) to list the direct mode variables.
 *
 ******************************************************************************
 */
value
proc_vars(node *n)
{
    value val;

    if (n != NODENIL) {
	value val;

	if (isexc(val = interpret_to_string(n)))
	    return val;
	else {
	    val = listprocsvars(string_part(val));
	    flshbuf();
	    return val;
	}
    } else {
	val = listprocsvars(CHARNIL);
	flshbuf();
	return val;
    }
}

/******************************************************************************
 *
 *	P R O C _ P R O C S ( node *n)
 *
 * Implements the ICL "PROCS" command to list the names of all the current
 * procedures.
 *
 * Uses listallprocs() (symtab.c).
 *
 ******************************************************************************
 */
value
proc_procs(node *n)
{
    value val;

    val = listallprocs();
    flshbuf();
    return val;
}

/******************************************************************************
 *
 *	P R O C _ L O A D (node *n)
 *
 * Implements the ICL command "LOAD file" which processes commands from the
 * file "file.ICL" rather than from the terminal. The SAVE command saves
 * procedures in filenames compatible with this command.
 *
 * This  function interprets the file argument as a string and then, if it does
 * not end with .icl, this string is appended.
 * do_load() (in input.c) is used to actually load and interpret the file.
 *
 ******************************************************************************
 */
value
proc_load(node *n)
{
    extern value do_load(char *whofor, char *filenametobeloaded); /* input.c */

    if (n != NODENIL) {
	value val;
	char *name;

	if (isexc(val = interpret_to_string(n)))
	    return val;
	if (!expand_name(&name, string_part(val)))
		return exception2("LOADERR - cannot expand filename %s - %s",
				   string_part(val), name);
	if (strcmp(&name[strlen(name) - 4], ".icl")) {
	    if ((name = strconcat(name, ".icl")) == CHARNIL)
		return exception("SYSERR  memory exhausted in LOAD");
	}
	return do_load("LOAD", name);
    } else
	return exception("TOOFEWPARS  LOAD: file name required");
}

/******************************************************************************
 *
 *	P R O C _ S A V E (node *n)
 *
 * Implements the ICL "SAVE proc" command.
 *
 * This saves the source of the ICL procedure 'proc' in the file proc.icl.
 *
 * If the parameter is the string ALL then we save all the currently known
 * procedures into a file SAVE.ICL (used automatically on ICL exit unless
 * NOSAVE is set).
 *
 * This routine interprets the single argument as a string and, if it is not
 * the string "all" calls fileaproc(proc,proc.icl) (symtab.c). Otherwise
 * fileaproc( CHARNIL,"save.icl") is called.
 *
 ******************************************************************************
 */
value
proc_save(node *n)
{
    if (n != NODENIL) {
	value val;
	char *origname;

	if (isexc(val = interpret_to_string(n)))
	    return (val);
	origname = lowercase(string_part(val));
	if (strcmp(origname, "all") == 0)
	    return (fileaproc(CHARNIL, "save.icl", "SAVE"));
	else {
	    char *w;

	    if ((w = strconcat(origname, ".icl")) == CHARNIL)
		return exception("SYSERR  memory exhausted in SAVE");
	    else
		return (fileaproc(origname, w, "SAVE"));
	}
    } else
	return exception("TOOFEWPARS  SAVE: file name required");
}

/******************************************************************************
 *
 *	P R O C _ E X I T (node *n)
 *
 * Implements the ICL "EXIT" command to return control to the operating system.
 *
 * On exiting, a copy of all the known procedures are saved in the file
 * SAVE.ICL (unless SET NOSAVE is in effect)
 *
 * 'nodeall' is set up at init time to prevent memory exceptions this late in
 * the day (see init_procs())
 *
 ******************************************************************************
 */
static node *nodeall;			/* initialised in init_procs() */

value
proc_exit(node *n)
{
    extern void killiosubsystem(void);				/* main.c */
    extern int  deinit_hdsfile(void);				/* hds.c  */
    extern void adam_stop(void);				/* adam.c */
    int pid;

/* Cancel our SIGCHLD handler */
    signal(SIGCHLD, SIG_DFL);

    if (save)
	proc_save(nodeall);	/* node_string ("all") */
    adam_stop();
    deinit_hdsfile();
    killiosubsystem();

/* wait for iosubsystem and all child process to die */
    while( (pid = wait((int *) 0)) != -1) /* Null statement */;

    exit(0);
    return trueval; /* for lint */
}

/******************************************************************************
 *
 *	P R O C _ S I G N A L (node *n)
 *
 * Implements the ICL command "SIGNAL name [text]" to signal an ICL exception.
 *
 * 'Name' is the exception name and can be any valid ICL identifier.
 * 'text' is a message associated with the 'name'
 *
 * Following proc_signal() an ICL exception handler will be executed if one
 * exists for the exception, otherwise a message will be output, and a return
 * will be made to direct mode.
 *
 * The 'name' argument is interpreted as a string and exception() is called
 * to generate the exception. We use the string concatenate facilty of
 * interpreting abbut_interpret() nodes of the arguments to join the exception
 * name and its text qualifiers.
 *
 ******************************************************************************
 */
value
proc_signal(node *n)
{
    value val;

    if (n != NODENIL)
	if (isexc(val = interpret(n)))
	    return val;
	else
	    return exception(string_part(val));
    else
	return exception("TOOFEWPARS  SIGNAL: exception name expected");
}

/******************************************************************************
 *
 *	P R O C _ D E L E T E (node *n)
 *
 * Implements the ICL command "DELETE proc".
 *
 * This removes the entry for the procedure "proc" from the symbol table.
 *
 * We interpret the proc parameter as a string and use delete_symbol()
 * (symtab.c) to remove it.
 *
 ******************************************************************************
 */
value
proc_delete(node *n)
{
    value val;

    if (isexc(val = interpret_to_string(n)))
	return val;
    (void) delete_symbol(string_part(val), SYM_PROC);
    return trueval;
}

/******************************************************************************
 *
 *	P R O C _ D E F H E L p (node *n)
 *
 * Implements the ICL command "DEFHELP topic helpfile key1 key2...
 *
 * This associates a particular helpfile and item within it with the
 * specified topic.
 *
 * The topic and its target are interpreted before being stored.
 *
 ******************************************************************************
 */
value
proc_defhelp(node *n)
{
    value val;
    char *library;
    char *topic;
    char *argstr;
    char space[2]=" ";
    node *nw;
    node *nw1, nw2;

    if (nargs < 2)
	return exception("TOOFEWPARS  Not enough parameters for DEFHELP");
    if (isexc(val = interpret(arglist[0])))  return val;
    if (!isstringtype(val))
	return (exception("DEFHELP  First parameter not a valid name"));
    topic = string_part(val);

    if (isexc(val = interpret(arglist[1]))) return val;
    if (!isstringtype(val))
	return (exception("DEFHELP  Second parameter not a valid name"));
    library = string_part(val);

    if ((nw = node1(help_interpret, value_string(library),
                    node_string( nargs==2? topic :
		    ( strcmp(argstr=string_part(concat_args(2)), "0" )? argstr:
                    space ) )))
		== NODENIL)
	return exception("SYSERR memory exhausted in DEFPROC");
    return (store_symbol( topic, SYM_HELP, nw));
}

/******************************************************************************
 *
 *	P R O C _ D E F S T R I N G (node *n)
 *
 * Implements the ICL command "DEFSTRING com(mand) equivalence string"
 *
 * This associates an equivalence string with a new ICL command 'command' -
 * the command to be defined.  The syntax com(mand) can be used and indicates
 * that com, comm, comma, comman and command are all acceptable abbreviations
 * for the command.
 *
 * Issuing the command is equivalent to having typed the equivalence string.
 * Any parameters given to 'command' are appended to the equivalence string.
 *
 * Both the command and the equivalence string are interpreted before storing.
 * The equivalence string will need to have arguments added when it is used,
 * and we will need to re-parse it then.
 *
 ******************************************************************************
 */
value
proc_defstring(node *n)
{
    value val;
    char *name;
    node *nw;

    if (nargs < 2)
	return exception("TOOFEWPARS  Not enough parameters for DEFSTRING");
    if (isexc(val = interpret(n->sub[0])))
	return val;
    if (!isstringtype(val))
	return (exception("DEFSTRING  First parameter not a valid name"));
    name = string_part(val);
    if (isexc(val = interpret(n->sub[1])))
	return val;
    if ((nw = node0(defstring_interpret, val)) == NODENIL)
	return exception("SYSERR  memory exhausted in DEFSTRING");
    else
	return (install_abbrevs(name, nw));
}

/******************************************************************************
 *
 *	P R O C _ D E F P R O C (Node *n)
 *
 * Implements the ICL command 'DEFPROC com(mand) file [procedure]'
 *
 * This defines a command which runs a procedure from a source file.
 *
 * command - the command to be defined. COM(MAND) specifies that COM is the
 *   minimum acceptable abbreviation
 * file - the source file containing the procedure
 * procedure - the procedure to be executed. If ommitted defaults to command.
 *
 * When command is issued, the file file.ICL is opened, loaded (if not already
 * loaded) and the procedure is called.  Any parameters with the issued
 * command are passed to the procedure.
 *
 * A series of entries in the global symbol table is established- one for
 * each possible abbreviation.  The val member contains the name of the file
 * as a string, and the value member points to a defproc_interpret() node
 * whose the sub[0] member points to the the procedure name and whose
 * sub[1] members points to the command name.
 *
 ******************************************************************************
 */
value
proc_defproc(node *n)
{
    value val;
    node *nw, *nw1, *nw2;
    char *name, *proc_name, *file_name;

    if (nargs < 2)
	return exception("TOOFEWPARS  Not enough parameters for DEFPROC");
    else if (nargs > 3)
	return exception("TOOMANYPARS  Too many parameters for DEFPROC");
    if (isexc(val = interpret(arglist[0])))
	return val;
    if (!isstringtype(val))
	return (exception("DEFPROC  First parameter not a valid name"));
    name = string_part(val);
    proc_name = name;
    if (isexc(val = interpret(arglist[1])))
	return val;
    if (!isstringtype(val))
	return (exception("DEFPROC  Second parameter not a file name"));
    file_name = string_part(val);
    if (nargs == 3) {
	if (isexc(val = interpret(arglist[2])))
	    return val;
	if (!isstringtype(val))
	    return (exception(
		"DEFPROC  Third parameter not a valid procedure name"));
	if ((proc_name = strcopy(string_part(val))) == CHARNIL)
	    return exception("SYSERR memory exhausted in DEFPROC");
    } else if (strchr(name, '(') != CHARNIL) {
	if ((proc_name = remove_parens(name)) == CHARNIL)
	    return exception("SYSERR memory exhausted in DEFPROC");
    } else
	proc_name = name;
    if ((nw1 = node_value(value_string(proc_name))) == NODENIL)
	return exception("SYSERR memory exhausted in DEFPROC");
    if ((nw2 = node_value(value_string(name))) == NODENIL)
	return exception("SYSERR memory exhausted in DEFPROC");
    if ((nw = node2(defproc_interpret, value_string(file_name), nw1, nw2))
		== NODENIL)
	return exception("SYSERR memory exhausted in DEFPROC");
    return (install_abbrevs(name, nw));
}

/******************************************************************************
 *
 *	P R O C _ H E L P (node *n)
 *
 * Implements the ICL command "HELP  [topic]".
 *
 * This the topic (if present) as a string to the help system.
 *
 * The help command may have several parameters.  The whole abbut_interpret()
 * node list formed by the parser for this sequence is pointed to by 'n' and
 * interpreting this causes the command string and all of its parameters to be
 * interpreted and the results of this to be seen as strings and concatenated
 * together.
 *
 * If no help topic command is given, then the help system is called with no
 * parameters.
 *
 ******************************************************************************
 */
value
proc_help(node *n)
{
/* Help variables */
    char *hfile;
    int pathlen;
    char *key1;
    value val;
    node *nw;

    if (nargs > 0 ) {
/* Get the first component of the topic
 * There is at least one */
       if (isexc(val = interpret(arglist[0]))) return val;
       key1 = string_part( val );
/* See if key1 has been defined by DEFHELP */
        if ( ( nw = lookup_symbol( key1, SYM_HELP ) ) != NODENIL ) {
/* It has */
           return interpret(nw);

        }
    }

/* The topic hasn't been DEFHELPed - set up a defhelp node */
    if ( ( helpfile == CHARNIL ) || (*helpfile == '\0') ) {
      if ( ems1Starf( "ICL_HELPFILE", "", "r", &hfile, &pathlen ) ) {
         helpfile = strcopy( hfile );
      } else {
         if (ems1Starf
           ( "PATH", "../help/icl/iclhelp.shl", "r", &hfile, &pathlen ) ) {
            helpfile = strcopy( hfile );
         } else {
            return exception("SYSERR help file not found in HELP");
         }
      }
    }
    if ((nw = node1(help_interpret, value_string(helpfile),
                    (nargs == 0) ? NODENIL : arglist[0] ))
		== NODENIL)
	return exception("SYSERR memory exhausted in HELP");
    if ( isexc( val = interpret( nw ) ) ) return val;

/* destroy the temporary node */

    return trueval;

}
/******************************************************************************
 *
 *	I N I T _ P R O C S (void)
 *
 * Install in the global symbol table all the implemented built_in commands.
 *
 ******************************************************************************
 */
value
init_procs(void)
{
    value val;

    if ((nodeall = node_string("all")) == NODENIL)
	return exception(
		"SYSERR  memory exhausted while establishing procedure table");

    if ((isexc(val = store_symbol("DEFPROC", SYM_BUILTIN,
					node_builtin(proc_defproc))))	||
	(isexc(val = store_symbol("DEFHELP", SYM_BUILTIN,
					node_builtin(proc_defhelp))))	||
	(isexc(val = store_symbol("DEFSTRING", SYM_BUILTIN,
					node_builtin(proc_defstring))))	||
	(isexc(val = store_symbol("DELETE", SYM_BUILTIN,
					node_builtin(proc_delete))))	||
	(isexc(val = store_symbol("EXIT", SYM_BUILTIN,
					node_builtin(proc_exit))))	||
	(isexc(val = store_symbol("LIST", SYM_BUILTIN,
					node_builtin(proc_list))))	||
	(isexc(val = store_symbol("LOAD", SYM_BUILTIN,
					node_builtin(proc_load))))	||
	(isexc(val = store_symbol("PROCS", SYM_BUILTIN,
					node_builtin(proc_procs))))	||
	(isexc(val = store_symbol("QUIT", SYM_BUILTIN,
					node_builtin(proc_exit))))	||
	(isexc(val = store_symbol("SAVE", SYM_BUILTIN,
					node_builtin(proc_save))))	||
	(isexc(val = store_symbol("CLEAR", SYM_BUILTIN,
					node_builtin(proc_clear))))	||
	(isexc(val = store_symbol("LOCATE", SYM_BUILTIN,
					node_builtin(proc_locate))))	||
	(isexc(val = store_symbol("SET", SYM_BUILTIN,
					node_builtin(proc_set))))	||
	(isexc(val = store_symbol("SIGNAL", SYM_BUILTIN,
					node_builtin(proc_signal))))	||
	(isexc(val = store_symbol("HELP", SYM_BUILTIN,
					node_builtin(proc_help))))	||
	(isexc(val = store_symbol("VARS", SYM_BUILTIN,
					node_builtin(proc_vars)))))
	return (val);
    else
	return (trueval);
}

