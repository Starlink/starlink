/*
 * tkXshell.c
 *
 * Version of Tk main modified for TclX to support SIGINT and use some of
 * the TclX utility procedures.
 *-----------------------------------------------------------------------------
 * Copyright 1991-1997 Karl Lehenbauer and Mark Diekhans.
 *
 * Permission to use, copy, modify, and distribute this software and its
 * documentation for any purpose and without fee is hereby granted, provided
 * that the above copyright notice appear in all copies.  Karl Lehenbauer and
 * Mark Diekhans make no representations about the suitability of this
 * software for any purpose.  It is provided "as is" without express or
 * implied warranty.
 *-----------------------------------------------------------------------------
 * $Id: tkXshell.c,v 8.6 1997/08/23 18:56:11 markd Exp $
 *-----------------------------------------------------------------------------
 */
/* 
 * tkMain.c --
 *
 *	This file contains a generic main program for Tk-based applications.
 *	It can be used as-is for many applications, just by supplying a
 *	different appInitProc procedure for each specific application.
 *	Or, it can be used as a template for creating new main programs
 *	for Tk applications.
 *
 * Copyright (c) 1990-1994 The Regents of the University of California.
 * Copyright (c) 1994-1995 Sun Microsystems, Inc.
 *
 * See the file "license.terms" for information on usage and redistribution
 * of this file, and for a DISCLAIMER OF ALL WARRANTIES.
 *
 * SCCS: @(#) tkMain.c 1.145 96/03/08 17:13:44
 */

#include <ctype.h>
#include <stdio.h>
#include <tclExtdInt.h>
#include <tk.h>

/*
 * Declarations for various library procedures and variables (don't want
 * to include tkInt.h or tkPort.h here, because people might copy this
 * file out of the Tk source directory to make their own modified versions).
 * Note: don't declare "exit" here even though a declaration is really
 * needed, because it will conflict with a declaration elsewhere on
 * some systems.
 */

#ifndef __WIN32__
extern char * strrchr _ANSI_ARGS_((CONST char *string, int c));
#endif


/*
 *----------------------------------------------------------------------
 *
 * TkX_Main --
 *
 *	Main program for Wish and most other Tk-based applications.
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
TkX_Main(argc, argv, appInitProc)
    int argc;				/* Number of arguments. */
    char **argv;			/* Array of argument strings. */
    Tcl_AppInitProc *appInitProc;	/* Application-specific initialization
					 * procedure to call after most
					 * initialization but before starting
					 * to execute commands. */
{
    char *args, *msg, *fileName;
    char buf[20];
    int code;
    size_t length;
    Tcl_Channel inChannel, outChannel, errChannel;
    Tcl_Interp *interp;
    int tty;

    TclX_SetAppInfo (TRUE,
                     "wishx",
                     "Extended Wish",
                     TKX_FULL_VERSION,
                     TCLX_PATCHLEVEL);

    Tcl_FindExecutable(argv[0]);
    interp = Tcl_CreateInterp();
#ifdef TCL_MEM_DEBUG
    Tcl_InitMemory(interp);
#endif

    /*
     * Parse command-line arguments.  A leading "-file" argument is
     * ignored (a historical relic from the distant past).  If the
     * next argument doesn't start with a "-" then strip it off and
     * use it as the name of a script file to process.
     */

    fileName = NULL;
    if (argc > 1) {
	length = strlen(argv[1]);
	if ((length >= 2) && (strncmp(argv[1], "-file", length) == 0)) {
	    argc--;
	    argv++;
	}
    }
    if ((argc > 1) && (argv[1][0] != '-')) {
	fileName = argv[1];
	argc--;
	argv++;
    }

    /*
     * Make command-line arguments available in the Tcl variables "argc"
     * and "argv".
     */

    args = Tcl_Merge(argc-1, argv+1);
    Tcl_SetVar(interp, "argv", args, TCL_GLOBAL_ONLY);
    ckfree(args);
    sprintf(buf, "%d", argc-1);
    Tcl_SetVar(interp, "argc", buf, TCL_GLOBAL_ONLY);
    Tcl_SetVar(interp, "argv0", (fileName != NULL) ? fileName : argv[0],
	    TCL_GLOBAL_ONLY);

    /*
     * Set the "tcl_interactive" variable.
     */

    /*
     * For now, under Windows, we assume we are not running as a console mode
     * app, so we need to use the GUI console.  In order to enable this, we
     * always claim to be running on a tty.  This probably isn't the right
     * way to do it.
     */

#ifdef __WIN32__
    tty = 1;
#else
    tty = isatty(0);
#endif
    Tcl_SetVar(interp, "tcl_interactive",
	    ((fileName == NULL) && tty) ? "1" : "0", TCL_GLOBAL_ONLY);

    if ((fileName == NULL) && tty)
        TclX_SetupSigInt ();

    /*
     * Invoke application-specific initialization.
     */

    if ((*appInitProc)(interp) != TCL_OK) {
        TclX_ErrorExit (interp, 255,
                        "\n    while\ninitializing application (Tcl_AppInit?)");
    }

    /*
     * Invoke the script specified on the command line, if any.
     */

    if (fileName != NULL) {
	code = TclX_Eval (interp,
                          TCLX_EVAL_GLOBAL | TCLX_EVAL_FILE |
                          TCLX_EVAL_ERR_HANDLER,
                          fileName);
	if (code != TCL_OK) {
	    goto error;
	}
	tty = 0;
    } else {

	/*
	 * Commands will come from standard input, so set up an event
	 * handler for standard input.  Evaluate the .rc file, if one
	 * has been specified, set up an event handler for standard
	 * input, and print a prompt if the input device is a terminal.
	 */

        TclX_EvalRCFile (interp);

	/*
	 * Establish a channel handler for stdin.
	 */

	inChannel = Tcl_GetStdChannel(TCL_STDIN);
	if (inChannel) {
            if (TclX_AsyncCommandLoop (interp,
                                       tty ? (TCLX_CMDL_INTERACTIVE |
                                              TCLX_CMDL_EXIT_ON_EOF) : 0,
                                       NULL, NULL, NULL) == TCL_ERROR)
                goto error;
        }
    }

    outChannel = Tcl_GetStdChannel(TCL_STDOUT);
    if (outChannel) {
	Tcl_Flush(outChannel);
    }
    Tcl_ResetResult(interp);

    /*
     * Loop infinitely, waiting for commands to execute.  When there
     * are no windows left, Tk_MainLoop returns and we exit.
     */

    Tk_MainLoop();
    TclX_ShellExit (interp, 0);

error:
    msg = Tcl_GetVar(interp, "errorInfo", TCL_GLOBAL_ONLY);
    if (msg == NULL) {
	msg = interp->result;
    }
    errChannel = Tcl_GetStdChannel(TCL_STDERR);
    if (errChannel) {
        Tcl_Write(errChannel, msg, -1);
        Tcl_Write(errChannel, "\n", 1);
    }

    TclX_ShellExit (interp, 1);
}


