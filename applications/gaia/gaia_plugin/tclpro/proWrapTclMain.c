/* 
 * proWrapTclMain.c --
 *
 *	Main program for Tcl applications wrapped using TclPro Wrapper.
 *
 * Copyright (c) 1998 Scriptics Corporation
 * All rights reserved.
 *
 * RCS: @(#) $Id: proWrapTclMain.c,v 1.1 1999/03/16 21:17:25 abrighto Exp $
 */

#include "tcl.h"
#include "tclInt.h"

#include <proWrap.h>

/*
 * The following code ensures that tclLink.c is linked whenever
 * Tcl is linked.  Without this code there's no reference to the
 * code in that file from anywhere in Tcl, so it may not be
 * linked into the application.
 */

EXTERN int Tcl_LinkVar();
int (*tclDummyLinkVarPtr)() = Tcl_LinkVar;

/*
 * Declarations for various library procedures and variables (don't want
 * to include tclPort.h here, because people might copy this file out of
 * the Tcl source directory to make their own modified versions).
 * Note:  "exit" should really be declared here, but there's no way to
 * declare it without causing conflicts with other definitions elsewher
 * on some systems, so it's better just to leave it out.
 */

extern int		isatty _ANSI_ARGS_((int fd));
extern char *		strcpy _ANSI_ARGS_((char *dst, CONST char *src));

static Tcl_Interp *interp;	/* Interpreter for application. */

#ifdef TCL_MEM_DEBUG
static char dumpFile[100];	/* Records where to dump memory allocation
				 * information. */
static int quitFlag = 0;	/* 1 means "checkmem" command was called,
				 * so the application should quit and dump
				 * memory allocation information. */
#endif

/*
 * Forward references for procedures defined later in this file:
 */

#ifdef TCL_MEM_DEBUG
static int		CheckmemCmd _ANSI_ARGS_((ClientData clientData,
			    Tcl_Interp *interp, int argc, char *argv[]));
#endif

/*
 *----------------------------------------------------------------------
 *
 * Pro_WrapTclMain --
 *
 *	Main program for tclsh and most other Tcl-based applications
 *	that may or may not be wrapped.
 *
 * Results:
 *	None. This procedure never returns (it exits the process when
 *	it's done.
 *
 * Side effects:
 *	This procedure initializes the Tk world and then starts
 *	interpreting commands;  almost anything could happen, depending
 *	on the script being interpreted.
 *
 *----------------------------------------------------------------------
 */

void
Pro_WrapTclMain(argc, argv, appInitProc)
    int argc;				/* Number of arguments. */
    char **argv;			/* Array of argument strings. */
    Tcl_AppInitProc *appInitProc;	/* Application-specific initialization
					 * procedure to call after most
				 	 * initialization but before starting
					 * to execute commands. */
{
    Tcl_Obj *prompt1NamePtr = NULL;
    Tcl_Obj *prompt2NamePtr = NULL;
    Tcl_Obj *resultPtr;
    Tcl_Obj *commandPtr = NULL;
    char buffer[1000], *args, *fileName, *bytes;
    char **saveArgv = NULL, **newArgv = NULL;
    int saveArgc = 0;
    int code, gotPartial, tty, length;
    int exitCode = 0;
    char *argv0 = NULL;			 /* A pointer to the string that will
					  * become the value of the Tcl "argv0"
					  * variable */
    Tcl_Channel inChannel, outChannel, errChannel;
    char *wrappedStartupFileName = NULL; /* Name of the startup script file
					  * that exists in the package. */
    char *wrappedArgs = NULL;		 /* The sequence of arguments that were
					  * specified when the application was
					  * wrapped. */

    Tcl_FindExecutable(argv[0]);
    interp = Tcl_CreateInterp();
#ifdef TCL_MEM_DEBUG
    Tcl_InitMemory(interp);
    Tcl_CreateCommand(interp, "checkmem", CheckmemCmd, (ClientData) 0,
	    (Tcl_CmdDeleteProc *) NULL);
#endif

    argv0 = argv[0];
    fileName = NULL;

    /*
     * Determine if the currently running application is wrapped or not.
     *
     * If the application is wrapped, the wrapped startup script file
     * (if any) is sourced after all other initialization; if additional
     * arguments were supplied during the wrapping process they are
     * inserted between argv[0] and argv[1].
     *
     * A wrapped application that does not specify a startup script file
     * will create an interactive shell.  However, if the first argument
     * (e.g. argv[1]) is a file name (e.g. a string without a leading
     * dash), that file is used as the startup script file for the
     * interpreter.
     *
     * If the application is not wrapped, the code below behaves exactly
     * like Tcl_Main and Tk_Main.
     */

    if (Pro_WrapIsWrapped(Tcl_GetNameOfExecutable(),
    	    &wrappedStartupFileName,
	    &wrappedArgs)) {
	if (wrappedStartupFileName != NULL) {
	    fileName = wrappedStartupFileName;
	}
        if (wrappedArgs != NULL) {
	    saveArgc = argc;
	    saveArgv = argv;

	    Pro_WrapPrependArgs(wrappedArgs, saveArgc, saveArgv, &argc, &argv);

	    /*
	     * argc and argv now represent the modified argument list.
	     * Save away argv in case it is modified immediately below
	     * and needs to be dellocated prior to exiting.
	     */

	    newArgv = argv;
	}
    }
    if (wrappedStartupFileName == NULL) {
	/*
	 * Make command-line arguments available in the Tcl variables "argc"
	 * and "argv".  If the first argument doesn't start with a "-" then
	 * strip it off and use it as the name of a script file to process.
	 */

	if ((argc > 1) && (argv[1][0] != '-')) {
	    fileName = argv[1];
	    argv0 = fileName;
	    argc--;
	    argv++;
	}
    }
    args = Tcl_Merge(argc-1, argv+1);
    Tcl_SetVar(interp, "argv", args, TCL_GLOBAL_ONLY);
    ckfree(args);
    TclFormatInt(buffer, argc-1);
    Tcl_SetVar(interp, "argc", buffer, TCL_GLOBAL_ONLY);
    Tcl_SetVar(interp, "argv0", argv0, TCL_GLOBAL_ONLY);

    /*
     * Set the "tcl_interactive" variable.
     */

    tty = isatty(0);
    Tcl_SetVar(interp, "tcl_interactive",
	    ((fileName == NULL) && tty) ? "1" : "0", TCL_GLOBAL_ONLY);

    /*
     * Initialize the wrapper for the specific interpreter.
     */

    Pro_WrapInit(interp);
    
    /*
     * Invoke application-specific initialization.  Note that we are
     * quitting immediately upon failure.  This is different from the
     * standard tclsh behavior on the assumption that if initialization
     * fails in a wrapped application, all bets are off.
     */

    if ((*appInitProc)(interp) != TCL_OK) {
	errChannel = Tcl_GetStdChannel(TCL_STDERR);
	if (errChannel) {
	    Tcl_Write(errChannel,
		    "application-specific initialization failed: ", -1);
	    Tcl_Write(errChannel, interp->result, -1);
	    Tcl_Write(errChannel, "\n", 1);
	}
	Tcl_DeleteInterp(interp);
	Tcl_Exit(1);
    }

    /*
     * If a script file was specified then just source that file
     * and quit.
     */

    if (fileName != NULL) {
	code = Tcl_EvalFile(interp, fileName);
	if (code != TCL_OK) {
	    errChannel = Tcl_GetStdChannel(TCL_STDERR);
	    if (errChannel) {
		/*
		 * The following statement guarantees that the errorInfo
		 * variable is set properly.
		 */

		Tcl_AddErrorInfo(interp, "");
		Tcl_Write(errChannel,
			Tcl_GetVar(interp, "errorInfo", TCL_GLOBAL_ONLY), -1);
		Tcl_Write(errChannel, "\n", 1);
	    }
	    exitCode = 1;
	}
	goto done;
    }

    /*
     * We're running interactively.  Source a user-specific startup
     * file if the application specified one and if the file exists.
     */

    Tcl_SourceRCFile(interp);

    /*
     * Process commands from stdin until there's an end-of-file.  Note
     * that we need to fetch the standard channels again after every
     * eval, since they may have been changed.
     */

    commandPtr = Tcl_NewObj();
    Tcl_IncrRefCount(commandPtr);
    prompt1NamePtr = Tcl_NewStringObj("tcl_prompt1", -1);
    Tcl_IncrRefCount(prompt1NamePtr);
    prompt2NamePtr = Tcl_NewStringObj("tcl_prompt2", -1);
    Tcl_IncrRefCount(prompt2NamePtr);
    
    inChannel = Tcl_GetStdChannel(TCL_STDIN);
    outChannel = Tcl_GetStdChannel(TCL_STDOUT);
    gotPartial = 0;
    while (1) {
	if (tty) {
	    Tcl_Obj *promptCmdPtr;

	    promptCmdPtr = Tcl_ObjGetVar2(interp,
		    (gotPartial? prompt2NamePtr : prompt1NamePtr),
		    (Tcl_Obj *) NULL, TCL_GLOBAL_ONLY);
	    if (promptCmdPtr == NULL) {
                defaultPrompt:
		if (!gotPartial && outChannel) {
		    Tcl_Write(outChannel, "% ", 2);
		}
	    } else {
		code = Tcl_EvalObj(interp, promptCmdPtr);
		inChannel = Tcl_GetStdChannel(TCL_STDIN);
		outChannel = Tcl_GetStdChannel(TCL_STDOUT);
		errChannel = Tcl_GetStdChannel(TCL_STDERR);
		if (code != TCL_OK) {
		    if (errChannel) {
			resultPtr = Tcl_GetObjResult(interp);
			bytes = Tcl_GetStringFromObj(resultPtr, &length);
			Tcl_Write(errChannel, bytes, length);
			Tcl_Write(errChannel, "\n", 1);
		    }
		    Tcl_AddErrorInfo(interp,
			    "\n    (script that generates prompt)");
		    goto defaultPrompt;
		}
	    }
	    if (outChannel) {
		Tcl_Flush(outChannel);
	    }
	}
	if (!inChannel) {
	    goto done;
	}
        length = Tcl_GetsObj(inChannel, commandPtr);
	if (length < 0) {
	    goto done;
	}
	if ((length == 0) && Tcl_Eof(inChannel) && (!gotPartial)) {
	    goto done;
	}

        /*
         * Add the newline removed by Tcl_GetsObj back to the string.
         */

	Tcl_AppendToObj(commandPtr, "\n", 1);
	if (!TclObjCommandComplete(commandPtr)) {
	    gotPartial = 1;
	    continue;
	}

	gotPartial = 0;
	code = Tcl_RecordAndEvalObj(interp, commandPtr, 0);
	inChannel = Tcl_GetStdChannel(TCL_STDIN);
	outChannel = Tcl_GetStdChannel(TCL_STDOUT);
	errChannel = Tcl_GetStdChannel(TCL_STDERR);
	Tcl_SetObjLength(commandPtr, 0);
	if (code != TCL_OK) {
	    if (errChannel) {
		resultPtr = Tcl_GetObjResult(interp);
		bytes = Tcl_GetStringFromObj(resultPtr, &length);
		Tcl_Write(errChannel, bytes, length);
		Tcl_Write(errChannel, "\n", 1);
	    }
	} else if (tty) {
	    resultPtr = Tcl_GetObjResult(interp);
	    bytes = Tcl_GetStringFromObj(resultPtr, &length);
	    if ((length > 0) && outChannel) {
		Tcl_Write(outChannel, bytes, length);
		Tcl_Write(outChannel, "\n", 1);
	    }
	}
#ifdef TCL_MEM_DEBUG
	if (quitFlag) {
	    Tcl_DecrRefCount(commandPtr);
	    Tcl_DecrRefCount(prompt1NamePtr);
	    Tcl_DecrRefCount(prompt2NamePtr);
	    Tcl_DeleteInterp(interp);
	    Tcl_Exit(0);
	}
#endif
    }

    /*
     * Rather than calling exit, invoke the "exit" command so that
     * users can replace "exit" with some other command to do additional
     * cleanup on exit.  The Tcl_Eval call should never return.
     */

    done:
    /*
     * Deallocate memory associated with prepending of wrapped arguments.
     * Restore the original values of argv/argc.
     */

    if (newArgv != NULL) {
	ckfree((char *) newArgv);
	argc = saveArgc;
	argv = saveArgv;
    }
    if (commandPtr != NULL) {
	Tcl_DecrRefCount(commandPtr);
    }
    if (prompt1NamePtr != NULL) {
	Tcl_DecrRefCount(prompt1NamePtr);
    }
    if (prompt2NamePtr != NULL) {
	Tcl_DecrRefCount(prompt2NamePtr);
    }
    sprintf(buffer, "exit %d", exitCode);
    Tcl_Eval(interp, buffer);
}

/*
 *----------------------------------------------------------------------
 *
 * CheckmemCmd --
 *
 *	This is the command procedure for the "checkmem" command, which
 *	causes the application to exit after printing information about
 *	memory usage to the file passed to this command as its first
 *	argument.
 *
 * Results:
 *	Returns a standard Tcl completion code.
 *
 * Side effects:
 *	None.
 *
 *----------------------------------------------------------------------
 */
#ifdef TCL_MEM_DEBUG

	/* ARGSUSED */
static int
CheckmemCmd(clientData, interp, argc, argv)
    ClientData clientData;		/* Not used. */
    Tcl_Interp *interp;			/* Interpreter for evaluation. */
    int argc;				/* Number of arguments. */
    char *argv[];			/* String values of arguments. */
{
    extern char *tclMemDumpFileName;
    if (argc != 2) {
	Tcl_AppendResult(interp, "wrong # args: should be \"", argv[0],
		" fileName\"", (char *) NULL);
	return TCL_ERROR;
    }
    strcpy(dumpFile, argv[1]);
    tclMemDumpFileName = dumpFile;
    quitFlag = 1;
    return TCL_OK;
}
#endif
