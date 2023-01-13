/******************************************************************************
 *
 *			M A I N . C
 *
 * This file contains the main() routine of the Unix version of ICL and much
 * of the initialisation code.
 *
 *	History
 *	Created :	S.K.Robinson	7/11/91
 *	Edited :	I.R.Jenkins	11/6/92
 *			Added function prototypes and cleaned
 *			up code fragments.
 *	Tidied :	B.K.McIlwrath	16/7/93
 *			Edit code and reformat with indent
 *
 ******************************************************************************
 */

#include <stdio.h>	/* for error output before iosubsystem running */
#include <signal.h>
#include <setjmp.h>
#include <string.h>
#include <sys/wait.h>
#include <sys/types.h>
#include <unistd.h>
#include "icl.h"
#include "sae_par.h"
#include "icl_io.h"
#include "ams.h"
#include "output.h"
#include "messys_len.h"
#include "messys_par.h"
#include "messys_err.h"
#include "dtask_err.h"
#include "f77.h"

int yyparse();

extern int uface_interrupt(void);				/* uface.c  */

/*
 * External variables
 */
extern node *todo;                                              /* interp.c */

/******************************************************************************
 *
 * The version of ICL in use
 *
 ******************************************************************************
 */
#ifdef PACKAGE_VERSION
char *version = "V " PACKAGE_VERSION;
#else
char *version = "Version Unknown";
#endif

/*
 * Need a dummy main on some fortran compilers
 */

#if HAVE_FC_MAIN
void FC_MAIN () {}
#endif


/******************************************************************************
 *
 * The operating system in use (see unix.c and vms.c)
 *
 ******************************************************************************
 */
extern char *os;						/* unix.c */

/******************************************************************************
 *
 * Flag vale set when a terminal SIGINT (CNTL/C) is detected - see
 * cntlc_handler()
 ******************************************************************************
 */
int sigint_flag;
/******************************************************************************
 *
 * Whether ICL is running interactively or not - can ONLY run interactively
 * at present!
 *
 ******************************************************************************
 */
int non_interactive = 0;

/******************************************************************************
 *
 * We can't guarantee to recognise VMS Variable Length Record Files
 * reliably on UNIX so the -v option allows the user to force it.
 *
 ******************************************************************************
 */
int always_vms = 0;

/******************************************************************************
 *
 * Name of the program, for startup error messages - initialised from argv[0]
 *
 ******************************************************************************
 */
char *prog_name;

/******************************************************************************
 *
 * The process id number of the iosubsystem task and the file descriptor
 * of the pipe to send it commands. This is used to send IO requests to the
 * subsystem.
 *
 ******************************************************************************
 */
int iopid, iocommand_fd=0;

/******************************************************************************
 *
 * Flag set when system is set up to handle floating point exceptions by
 * setjmp and longjmp() and its associated variable
 *
 ******************************************************************************
 */
jmp_buf jmp_float;
int jmp_float_setup = 0;

/******************************************************************************
 *
 *	F L O A T _ E X C E P T I O N (int)
 *
 * A function to handle a floating point exception.
 *
 * Called when a floating exception (SIG_FPE) occurs, it resets the signal
 * handler function to itself, and if the system is set up to handle floating
 * exception signals executes a longjmp() to continue execution at the point
 * set up in jmp_float. This should have been set just prior to executing a
 * floating point operation within carith.c.
 *
 * If the system does not expect floating signals then we have a system error
 * so we report failure and abort.
 *
 ******************************************************************************
 */
static void
float_exception(int sig)
{
    if (jmp_float_setup)
	longjmp(jmp_float, 1);
    else {
	systemfail("unexpected floating point exception: aborting...\n");
	/* No return from systemfail! */
	exit(1);	/* for lint */
    }
}

/******************************************************************************
 *
 *	M E M O R Y  _ E X C E P T I O N (int)
 *
 * A function to handle ICL memory access exceptions.
 *
 * Trys to close down the message system and the io subsystem and then aborts
 * with a core dump
 *
 ******************************************************************************
 */
static void
memory_violation(int sig)
{
    void killiosubsystem(void);
    extern void adam_stop(void);

    int pid;

    signal(SIGCHLD, SIG_DFL);
    signal(SIGABRT, SIG_DFL);

    fprintf(stderr, "memory violation in ICL....aborting...core file dumped\n");
    killiosubsystem();
    adam_stop();

/* wait for all child processes to die */
    while( (pid = wait((int *) 0)) != -1) /* Null statement */;

    abort();	/* Terminate with core */
}

/******************************************************************************
 *
 *	C N T L C _ H A N D L E R (int signo)
 *
 * Handler set for keyboard interrupts. Just sets the flag value 'sigint_flag'
 * which is picked up by the function uface_interrupt()
 ******************************************************************************
 */
void
cntlc_handler(int signo)
{
    int status = SAI__OK;
    sigint_flag = 1;
    ams_extint(&status);
    return;
}

/******************************************************************************
 *
 * ICL procedure calls are implemented by using a push down stack of
 * procedure names which allows the system to keep track of the currently
 * active proccedures.
 *
 * This is pushed with the name of the currently active procedure or function
 * on entry by either proc_call_interpret() or function_call_interpret()
 * respectively.
 * It is normally popped on return unless an exception occured in execution.
 * If an exception occured the stack is left until either we detect an
 * exception handler (when we restore the stack to the level where the handler
 * was found)
 * OR
 * when we arrive at the top level when the stack is used to report the
 * exception and the call list.
 *
 * To permit the restore mentioned above we provide a further stack
 * calllevel_stack[ICL_BUFSIZE] with calllevel_stack_level which is pushed in
 * procedure_interpret() with the current value of call_stack_level and popped
 * to restore that level when needed.
 *
 ******************************************************************************
 */
static char *call_stack[ICL_BUFSIZE];
static int call_stack_level = 0;
static int calllevel_stack[ICL_BUFSIZE];
static int calllevel_stack_level = 0;

/******************************************************************************
 *
 *	C H E C K C A L L S T A C K (char *procname)
 *
 * Checks the stack of currently executing procs against 'procname'.
 * Used to prevent recursive calls to ICL procedures.
 *
 ******************************************************************************
 */
value
checkcallstack(char *procname)
{
    int i;

    for (i = 0; i < call_stack_level; ++i)
	if (iclidentityequal(call_stack[i], procname, 15))
	    return exception1("RECCALL   Recursive call of \"%s\" ", procname);
    return (trueval);
}

/******************************************************************************
 *
 *	P U S H C A L L S T A C K (char * procname)
 *
 * Push the procname on the currently active procedures stack IF there is
 * space.
 *
 ******************************************************************************
 */
value
pushcallstack(char *procname)
{
    if (call_stack_level == ICL_BUFSIZE)
	return exception("STKOVFLOW  Call nesting too deep");
    call_stack[call_stack_level++] = procname;
    return (trueval);
}

/******************************************************************************
 *
 *	P U S H C A L L L E V E L (void)
 *
 * Save the current call_stack_level onto calllevel_stack[]
 *
 ******************************************************************************
 */
value
pushcalllevel(void)
{
    if (calllevel_stack_level == ICL_BUFSIZE)
	return exception("STKOVFLOW  Call nesting too deep");
    calllevel_stack[calllevel_stack_level++] = call_stack_level;
    return trueval;
}

/******************************************************************************
 *
 *	P O P C A L L S T A C K (void)
 *
 * Pop the currently active procedure providing there is at least one.
 *
 ******************************************************************************
 */
value
popcallstack(void)
{
    if (call_stack_level <= 0)
	return exception("STKUNFLOW Call stack underflow");
    --call_stack_level;
    return trueval;
}

/******************************************************************************
 *
 *	R E S T O R E C A L L L E V E L (void)
 *
 * Restore a saved call_stack_level from the calllevel_stack[]
 *
 ******************************************************************************
 */
value
restorecalllevel(void)
{
    if (calllevel_stack_level <= 0)
	return exception("STKUNFLOW Calllevel stack underflow");
    call_stack_level = calllevel_stack[calllevel_stack_level - 1];
    return trueval;
}

/******************************************************************************
 *
 *	P O P C A L L L E V E L (void)
 *
 * Clear a saved call_stack_level from the calllevel_stack[]
 *
 ******************************************************************************
 */
value
popcalllevel(void)
{
    if (calllevel_stack_level <= 0)
	return exception("STKUNFLOW Calllevel stack underflow");
    --calllevel_stack_level;
    return trueval;
}

/******************************************************************************
 *
 *	C U R R E N T P R O C (void)
 *
 * Return the top entry in the call stack if there is one .
 *
 ******************************************************************************
 */
char *
currentproc(void)
{
    if (call_stack_level <= 0)
	return CHARNIL;
    return (call_stack[call_stack_level - 1]);
}

/******************************************************************************
 *
 *	E X E C U T E (void)
 *
 * yyparse() parses the current input source line (see input.c) and constructs
 * the abstract node syntax form. If this delivers anything to be interpreted
 * "todo" is set to point to the top of this node structure.
 *
 * This routine, using interpret() (in node.c), executes the abstract syntax.
 * If this returns an exception this is reported and the call_stack is printed
 * and cleared.
 * During normal execution the call stack is popped.
 *
 ******************************************************************************
 */
int
execute(void)
{
    extern value interpret(node *n);			/* interp.c */
    value val;

    sigint_flag = 0;
    if (todo != NODENIL) {
	if (isexc(val = interpret(todo))) {
	    bufstring(string_part(val));
	    bufnewline();
	    if (call_stack_level) {
		bufstring("  In Procedure: ");
		bufstring(call_stack[--call_stack_level]);
		bufnewline();
	    }
	    while (call_stack_level) {
		bufstring("  Called from: ");
		bufstring(call_stack[--call_stack_level]);
		bufnewline();
	    }
	    flshbuf();
	}
    }
    if (uface_interrupt()) {
	char buff[40];

	sprintf(buff, "\nInterrupt\n");
	outstring(buff);
    }
    todo = NODENIL;
    return 0;
}

/******************************************************************************
 *
 *	K I L L I O S U B S Y S T E M (void)
 *
 * Kill off the io subsystem if it is running. This is done by sending the
 * appropriate message using MSP.
 *
 ******************************************************************************
 */
void
killiosubsystem(void)
{
    struct iocommand iomessage;

    iomessage.fcode = IO_COMMAND_KILL;
    iomessage.iarg = 1;
    iomessage.buflen = 2;

    write(iocommand_fd, &iomessage, iocommandlength(iomessage));

    return;
}

/******************************************************************************
 *
 *	I O S U B S Y S T E M _H A N D L E R (void)
 *
 * Signal handler used for SIGCHLD events during ICL iosubsystem loading.
 *
 ******************************************************************************
 */
static void
iosubsystem_handler(int sig)
{
    int pid, status;

    while( (pid = waitpid(-1, &status, WNOHANG)) > 0) {
	if (pid != iopid)
	    fprintf(stderr,
		"Unknown ICL child terminated - pid = %d, status = %d\n",
		pid, status);
	else if (WIFSTOPPED(status))
	    break;
	fprintf(stderr, "iosubsystem process died ");
	if (WIFSIGNALED(status))
	    fprintf(stderr,"- signal #%d\n", WTERMSIG(status));
	else if (WIFEXITED(status))
	    fprintf(stderr,"- exit with status=%d\n", WEXITSTATUS(status));
    }
    return;
}

/******************************************************************************
 *
 *	S T A R T I O S U B S Y S T E M (char *filename, char *icl_argv[],
 *					 int nargs)
 *
 * The IO from ICL is handled by a cooperating iosubsystem process.
 *
 * This subprocess is either loaded from 'filename' or from the default file
 * defined by the macro IOPROCESSNAME if 'filename' is null.
 * 'icl_argv' and 'nargs' are used to pass parameters given on the icl command
 * line to the subsystem using the *argv[] mechanism - see below.
 *
 * We return (-1) if an error is detacted in loading the iosubsystem or it
 * fails to signon correctly.
 * Otherwise the return value is the identifier of the io subsystems
 * MSP command_q.
 *
 ******************************************************************************
 */
static int
startiosubsystem(char *filename, char *icl_argv[], int nargs)
{
    int status, path, messid, message_context, message_status, value_len;
    int i, filedes[2];
    char name[MSG_NAME_LEN], message_name[MSG_NAME_LEN], value[MSG_VAL_LEN];
    struct iocommand message;
    char *image_file;
    char **argv;
/*
 * Set up the argv[] array to pass to the subsystem.
 *	argv[1] is the name the ICL process has registered itself with AMS.
 *      icl_argv[] references argument(s) to icl prfixed by the '-ioa' switch
 *	    These are passed to the subsystem as argv[2]...argv[n]
 */
    sprintf(name, "%s%d", ICLSYSTEMNAME, getpid());
    argv = (char **) malloc (sizeof(char *) * (3+nargs));
    argv[0] = IOPROCESSNAME;
    argv[1] = name;
    if( nargs==0 )
	argv[2] =NULL;
    else
	for (i=0; i<=nargs; i++)
	    argv[2+i] = *icl_argv++;
    image_file = ((filename != NULL)? filename : IOPROCESSNAME);
/*
 * Create a pipe to send commands to the iosubsystem
 */
    pipe(filedes);
    switch( iopid = fork() ) {

      case (-1):	/* fork() error */
	iocommand_fd = -2;
	break;			/* this function will return -2 */

      case 0:           /* Child */
    /*
     * Close any file descriptors inherited from icl referring to the message
     * system and ensure that the io process has the read end of the command
     * pipe on fd=3
     */
	i = dup2(filedes[0], 3);
	for(i=4; i<=filedes[1]; i++)
	    close(i);
    /*
     * Run the iosubsystem image
     */
	if (execvp(image_file, argv )!= 0)
	    fprintf(stderr,"exec() iosubsystem failed - file %s - %s\n",
		image_file, strerror(errno));
    /*
     * if we get here the io subsystem failed to start in the child -
     * notify the parent
     */
	exit(1);
	break;

      default:	/* fork() worked and we are still the parent (ie ICL) */
    /*
     * We wait to receive the io subsystem OBEY message and return the
     * initial acknowledgement
     */
        free(argv);
	i = close(filedes[0]);	/* close read end of pipe */
	iocommand_fd = filedes[1];
    	signal(SIGCHLD, iosubsystem_handler);
        status = SAI__OK;
	ams_receive(MESSYS__INFINITE, MSG_NAME_LEN, MSG_VAL_LEN,
		    &message_status, &message_context, message_name,
		    &value_len, value, &path, &messid, &status);
        ams_reply(path, messid, MESSYS__MESSAGE, DTASK__ACTSTART,
		  message_context, message_name, value_len, value, &status);
	break;
    }
    signal(SIGCHLD, SIG_DFL);

    return iocommand_fd;
}

   /* extern int yydebug; * For debugging the parser */
/******************************************************************************
 *
 *	M A I N - ICL mainline code and entry point
 *
 * ICL is invoked from the shell by the command icl [-vwf] filename
 *
 * Method:
 *
 * Initialise prog_name to the argv[0] (icl)
 *
 * Initialise the output system stream to stderr by calling init_output().
 *
 * The system initialises itself further using init_functions(), init_values()
 * init_procs(), init_fileio(), init_os() and init_adam().
 *
 * A welcome banner is printed on stderr.
 *
 * A handler for SIGFPE (floating point) is set (float_exception()) and
 * SIGTERM exceptions are by default ignored.
 *
 * We then check to see if there is a start_up file (set as the value of the
 * environment variable ICL_LOGIN). If defionied the system loads (using
 * do_load()) the contents of the file otherwise it tries to load the file
 * login.icl from the users HOME directory.
 *
 * After this the parameters of the ICL call are processed.
 * 	-v	sets always_vms to inform the system that all
 * 		input files are vms format
 * 	-f	sets non_interactive to indicate a background run of icl
 * 	-?	causes an error message and a call to exit() to cause normal
 * 		program termination.
 *
 * Allowance is made for the fact that Unix uses '-' and VMS uses '/'.
 * Any parameter that does not begin with - (or /) is treated as a
 * source of input and is loaded using do_load().  Both the argument string
 * and, if that fails,  the string concatenated with .icl, are passed to
 * do_load(). If the do_load() causes an exception then the exception message
 * is reported.
 *
 * If after this the non_interactive flag is set then we are done and  we can
 * finish with exit(1). Otherwise we set up input, using init_input(), and
 * trap interrupts using the function signal().  We then enter a loop of
 * calling yyparse() to parse the interactive input (LINE BY LINE - with the
 * exception of proc definitions) and call execute() to interpret() the parsed
 * input's abstract syntax form (if any) which will have been set into the
 * variable 'todo'.  We make sure we clear the PROMPT stack on each cycle in
 * the case of an incomplete execution.
 *
 * This cycle is broken by yyparse() delivering zero (End-of-file) when we
 * output newline to the terminal and call proc_exit() (in procs.c) to make
 * the decision to whether to save ICL's state or not depending  on the
 * variable save (altered by SET (NO)SAVE). We then return to the shell.
 *
 ******************************************************************************
 */
int
main(int argc, char *argv[])
{
/*
 * Module initialisation routines - only called from main()
 */
    extern void  init_arith(void);			/* arith.c */
    extern value init_adam(void);			/* adam.c */
    extern value init_fileio(void);			/* fileio.c */
    extern value init_functions(void);			/* functions.c */
    extern value init_input(void);			/* input.c */
    extern value init_output(void);			/* output.c */
    extern value init_os(void);				/* unix.c */
    extern value init_procs(void);			/* procs.c */
    extern value init_hds(void);			/* hds.c */
/*
 * Other external function prototypes used only in main()
 */
    extern value proc_exit(node * n);			/* procs.c */
    extern void  clear_promptstack(void);		/* input.c */
    extern value do_load(char *command_name, char *name); /* input.c */
    extern void task_exithandler(int signo);		/* adam.c */
/*
 * Local variables
 */
    int istat, non_interactive = 0, abort = 0;
    value val;
    char *s;
    char *io_imagefile = NULL;
    char **io_args = NULL;
    int io_nargs = 0;
    struct sigaction act;
    /* yydebug = 1; * Debug */
/*
 * Start of main() code
 */

/*
 * Some fortran is involved
 */

    cnfInitRTL( argc, argv );

    prog_name = argv[0];
/*
 * The ICL command line must be of the form:
 *
 *  icl <command line arguments starting with '-'> names of .icl files to load
 *	The options MUST precede the names of files to load
 *
 * notable new arguments (November 1993) include:
 *
 *	-io  <name of iosubsystem file to load is next argument>
 *	-ioa <argument list> - all parameters after -ioa will be passed to
 *	     the iosubsystem.
 */
    for (*argv++; *argv; *argv++) {
	if (**argv != '-')
	    break;

	switch ((*argv)[1]) {
	  case 'v':
	    ++always_vms;
	    break;
	  case 'f':
	    ++non_interactive;
	    break;
	  case 'i':	/* argument can be either -io or -ioa */
	    if ((*argv)[2] == 'o') {
		if((*argv)[3] == '\0')
		    io_imagefile = *++argv;
		else if((*argv)[3] == 'a' && (*argv)[4] == '\0') {
		    io_args = &(*++argv);
		    while( *argv != NULL   && **argv == '-') {
			*argv++;
			io_nargs++;
		    }
		}
		else
		    io_imagefile = &(*argv)[3];
	    break;
	    }
	  default:
	    fprintf(stderr,"%s: unknown option %s\n", prog_name, *argv);
 	    abort = 1;
	    break;
	  } /* switch */
    } /* for */
    if (abort)
	exit(1);
/*
 * Perform internal ICL initialisation
 */
    init_arith();		/* The C arithmetic and value modules must */
    init_values();		/* be initialised first and in this order  */
/*
 * Complete internal ICL initialisation
 */
    if ((isexc(val = init_output()) )	||
        (isexc(val = init_input())  )	||
        (isexc(val = init_functions())) ||
	(isexc(val = init_procs())  )	||
	(isexc(val = init_fileio()) )	||
	(isexc(val = init_os()) )) {
	fprintf(stderr, "SYSTEM FAIL : ICL failed to initialise -\n");
        fprintf(stderr, "%s\n", string_part(val));
	exit(1);
	return (0); /* for lint */
    }
/*
 * Initialise the ADAM task control module - this also registers ICL with
 * messys and starts message system support processes
 */
    if (isexc(val = init_adam())) {
        fprintf(stderr, "%s\n", string_part(val));
	fprintf(stderr, "SYSTEM FAIL :  Failed to initialise ADAM\n");
	exit(1);
	return (0); /* for lint */
    }
/*
 * Start HDS
 */
    if (isexc(val = init_hds())) {
        fprintf(stderr, "%s\n", string_part(val));
	fprintf(stderr, "SYSTEM FAIL :  Failed to initialise HDS\n");
	exit(1);
	return (0); /* for lint */
    }
/*
 * Start the ICL iosubsystem process
 */
    if ((istat = startiosubsystem(io_imagefile, io_args, io_nargs)) < 0) {
	if (istat != -2)
	    wait((int *) 0); /* wait for iosubsystem child to die */
	if (istat == -2)
	    fprintf(stderr,
		"SYSTEM FAIL :  Failed to start io subsystem\n");
	else if (istat == -1)
	    fprintf(stderr,
		"SYSTEM FAIL :  iosubsystem failed to communicate\n");
	else
	    fprintf(stderr,
		"SYSTEM FAIL :  iosubsystem failed to signon\n");
	exit(1);
	return (0); /* for lint */
    }
/*
 * We have a working message system and iosubsystem process - try and ensure
 * the user does not exit ICL except by approved routes
 */
    act.sa_handler = cntlc_handler;
    sigemptyset(&act.sa_mask);
    act.sa_flags = 0;
#ifdef SA_RESTART
    act.sa_flags |= SA_RESTART;
#endif
    sigaction(SIGINT,  &act, NULL);	/* Handler for terminal interrupt */
    sigaction(SIGQUIT, &act, NULL);

#ifdef SA_NOCLDSTOP
    act.sa_flags |= SA_NOCLDSTOP;
#endif
    act.sa_handler = task_exithandler;  /* Child process exit handler */
    sigaction(SIGCHLD, &act, NULL);

    signal(SIGTERM, SIG_IGN);		/* Ignore kill and hangup */
    signal(SIGHUP,  SIG_IGN);
/* We use socket based communications - avoid process aborts on SIGPIPE */
/* (the invalid length read/writes on the socket will be detected)      */
    signal ( SIGPIPE, SIG_IGN);

/*
 *  act.sa_handler = float_exception;
 *  sigaction(SIGFPE, &act, NULL);     * Arithmetic exception handler *
 */
    act.sa_handler = memory_violation;
#ifdef SA_RESETHAND
    act.sa_flags = SA_RESETHAND;
#else
    act.sa_flags = 0;
#endif

    sigaction (SIGSEGV, &act, NULL);	    /* Memory exceptions */
    sigaction (SIGBUS,  &act, NULL);
/*
 * find a startup file if there is one
 */
    if ((s = getenv("ICL_LOGIN_SYS")) != CHARNIL) {
	if (strcmp(&s[strlen(s) - 4], ".icl"))
	    s = strconcat(s, ".icl");
	val = do_load(prog_name, s);
	if (isexc(val)) {
	    bufstring(string_part(val));
	    bufnewline();
	}
    }
    if ((s = getenv("ICL_LOGIN_LOCAL")) != CHARNIL) {
	if (strcmp(&s[strlen(s) - 4], ".icl"))
	    s = strconcat(s, ".icl");
	val = do_load(prog_name, s);
	if (isexc(val)) {
	    bufstring(string_part(val));
	    bufnewline();
	}
    }
    if ((s = getenv("ICL_LOGIN")) != CHARNIL) {
	if (strcmp(&s[strlen(s) - 4], ".icl"))
	    s = strconcat(s, ".icl");
	val = do_load(prog_name, s);
	if (isexc(val)) {
	    bufstring(string_part(val));
	    bufnewline();
	}
    }
    clear_promptstack();

/*
 * We may have unprocessed command line arguments (not starting with '-')
 * which are the names of .icl files to load (.icl suffix optional)
 */
    while (*argv != 0) {
        s = *argv;
	if (strcmp(&s[strlen(s) - 4], ".icl"))
	    s = strconcat(s, ".icl");
	val = do_load(prog_name, s);
	if (isexc(val)) {
	    bufstring(string_part(val));
	    bufnewline();
	}
	*argv++;
    }
    flshbuf();
/*
 * This is the heart of icl. yacc will return a pointer to the parsed syntax
 * in TODO and execute() causes this to be obeyed.
 */
    todo = NODENIL;
    while ((istat = yyparse()) != 0) {
	execute();
	clear_promptstack();
	sigint_flag = 0;
    }
/*
 * ICL should exit by calling proc_exit (in procs.c) from the EXIT command.
 * - we only get here if we drop out of the yyparse() loop but, for neatness,
 * we can still perform the exit processing in proc_exit(node *n) directly as
 * the node argument is not used.
 */
    proc_exit(NODENIL);
    return 0;	/* for lint */
}

/******************************************************************************
 *
 *	Y Y W R A P (void)
 *
 * lex is used to tokenise the input line and allows the redefinition of its
 * library routine yywrap() which is called at an end_of_file.
 *
 * If it returns 1, lex continues with the normal wrap_up of input.
 *
 * Sometimes, however, it is convenient to arrange for more input to
 * arrive from a new source.  In this case the programmer should provide a
 * yywrap() which arranges for new input and returns 0.  This instructs
 * lex to continue processing.  The default yywrap returns 1 as does this
 * version which is present in case we need it later.
 *
 ******************************************************************************
 */
int
yywrap(void)
{
    return 1;
}

/******************************************************************************
 *
 *	Y Y E R R O R (char * s)
 *
 * We use yacc to parse the input line and this requires the definition of a
 * function yyerror(char *s) that can report an error message 's'.
 *
 ******************************************************************************
 */
int
yyerror(char *s)
{
    flshbuf();
    bufstring("Exception during parsing of input : ");
    bufstring(s);
    bufnewline();
    flshbuf();
    return 1;			/* return value not used */
}
