/*
 * tkXshell.c
 *
 * Version of tkMain.c modified for TclX to support SIGINT and use some of
 * the TclX utility procedures.
 *-----------------------------------------------------------------------------
 * Copyright 1991-1999 Karl Lehenbauer and Mark Diekhans.
 *
 * Permission to use, copy, modify, and distribute this software and its
 * documentation for any purpose and without fee is hereby granted, provided
 * that the above copyright notice appear in all copies.  Karl Lehenbauer and
 * Mark Diekhans make no representations about the suitability of this
 * software for any purpose.  It is provided "as is" without express or
 * implied warranty.
 *-----------------------------------------------------------------------------
 * $Id: tkXshell.c,v 8.17 2001/06/29 20:55:39 andreas_kupries Exp $
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
 * Copyright (c) 1994-1997 Sun Microsystems, Inc.
 *
 * See the file "license.terms" for information on usage and redistribution
 * of this file, and for a DISCLAIMER OF ALL WARRANTIES.
 *
 * RCS: @(#) $Id: tkXshell.c,v 8.17 2001/06/29 20:55:39 andreas_kupries Exp $
 */

/*
 * This define is used to mark the differences in the TclX version of
 * this program.   Its never intended to be turned off.
 */
#define TKX_SHELL

#include <ctype.h>
#include <stdio.h>
#include <string.h>
#ifdef TKX_SHELL
#include <tclExtdInt.h>
#endif
#include <tcl.h>
#include <tk.h>
#include "tkInt.h"
#ifdef NO_STDLIB_H
#   include "../compat/stdlib.h"
#else
#   include <stdlib.h>
#endif
#ifdef __WIN32__
#include "tkWinInt.h"
#endif


typedef struct ThreadSpecificData {
    Tcl_Interp *interp;         /* Interpreter for this thread. */
    Tcl_DString command;        /* Used to assemble lines of terminal input
				 * into Tcl commands. */
    Tcl_DString line;           /* Used to read the next line from the
				 * terminal input. */
    int tty;                    /* Non-zero means standard input is a 
				 * terminal-like device.  Zero means it's
				 * a file. */
} ThreadSpecificData;
Tcl_ThreadDataKey dataKey;

/*
 * Declarations for various library procedures and variables (don't want
 * to include tkInt.h or tkPort.h here, because people might copy this
 * file out of the Tk source directory to make their own modified versions).
 * Note: don't declare "exit" here even though a declaration is really
 * needed, because it will conflict with a declaration elsewhere on
 * some systems.
 */

#if !defined(__WIN32__) && !defined(_WIN32)
extern int		isatty _ANSI_ARGS_((int fd));
extern char *		strrchr _ANSI_ARGS_((CONST char *string, int c));
#endif

#ifndef TKX_SHELL
/*
 * Forward declarations for procedures defined later in this file.
 */

static void		Prompt _ANSI_ARGS_((Tcl_Interp *interp, int partial));
static void		StdinProc _ANSI_ARGS_((ClientData clientData,
			    int mask));
#endif

/*
 *----------------------------------------------------------------------
 *
 * TkMainEx --
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
#ifdef TKX_SHELL
void
TkX_MainEx(argc, argv, appInitProc, interp)
#else
void
Tk_MainEx(argc, argv, appInitProc, interp)
#endif
    int argc;				/* Number of arguments. */
    char **argv;			/* Array of argument strings. */
    Tcl_AppInitProc *appInitProc;	/* Application-specific initialization
					 * procedure to call after most
					 * initialization but before starting
					 * to execute commands. */
    Tcl_Interp *interp;
{
    char *args, *fileName;
    char buf[TCL_INTEGER_SPACE];
    int code;
#ifndef TKX_SHELL
    size_t length;
#endif
    Tcl_Channel inChannel, outChannel;
    Tcl_DString argString;
    ThreadSpecificData *tsdPtr;
#ifdef __WIN32__
    HANDLE handle;
#endif
#ifdef TKX_SHELL
    char *msg;
    Tcl_Channel errChannel;
    int argi;
#endif
#ifdef USE_TCL_STUBS
    /*
     * Ensure that we are getting the matching version of Tcl.  This is
     * really only an issue when Tk is loaded dynamically.
     */

    if (Tcl_InitStubs(interp, TCL_VERSION, 1) == NULL) {
	abort();
    }
#endif
#ifdef TKX_SHELL
    if (TclX_InitTclStubs(interp, TCLX_VERSION, 0) == NULL) {
	abort();
    }
    
    TclX_SetAppInfo(TRUE,
                    "wishx",
                    "Extended Wish",
                    TKX_FULL_VERSION,
                    TCLX_PATCHLEVEL);
#endif

    tsdPtr = (ThreadSpecificData *) 
	Tcl_GetThreadData(&dataKey, sizeof(ThreadSpecificData));
    
    Tcl_FindExecutable(argv[0]);
    tsdPtr->interp = interp;

#if (defined(__WIN32__) || defined(MAC_TCL))
    Tk_InitConsoleChannels(interp);
#endif
    
#ifdef TCL_MEM_DEBUG
    Tcl_InitMemory(interp);
#endif

#ifdef TKX_SHELL
    /*
     * TclX command line parsing.  Need to find the file
     * name. Options to Tk assumed to take one arguments, with
     * the exception of -sync.
     */
    fileName = NULL;
    argi = 1;
    while ((argi < argc) && (argv[argi][0] == '-')) {
        if (STREQU(argv[argi], "--")) {
	    /*
	     * Fix for #230194.
	     */
	    argi ++;
            break; /* end of options */
        } else if (!STREQU(argv[argi], "-sync")) {
            argi++; /* Option is not "-sync", takes an argument */
        }
	argi ++;
    }
    /* Parse out file name, if supplied. */
    if (argi < argc) {
        int i;
        fileName = argv[argi];
        if (STREQU(argv[argi-1], "--")) {
            i = argi-1;
        } else {
            i = argi;
        }
        argi++;
        while (argi < argc) {
            argv[i++] = argv[argi++];
        }
	/*
	 * Fix for #230194.
	 */
        argc = i;
    }
#else
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
#endif

    /*
     * Make command-line arguments available in the Tcl variables "argc"
     * and "argv".
     */

    args = Tcl_Merge(argc-1, argv+1);
    Tcl_ExternalToUtfDString(NULL, args, -1, &argString);
    Tcl_SetVar(interp, "argv", Tcl_DStringValue(&argString), TCL_GLOBAL_ONLY);
    Tcl_DStringFree(&argString);
    ckfree(args);
    sprintf(buf, "%d", argc-1);

    if (fileName == NULL) {
	Tcl_ExternalToUtfDString(NULL, argv[0], -1, &argString);
    } else {
	fileName = Tcl_ExternalToUtfDString(NULL, fileName, -1, &argString);
    }
    Tcl_SetVar(interp, "argc", buf, TCL_GLOBAL_ONLY);
    Tcl_SetVar(interp, "argv0", Tcl_DStringValue(&argString), TCL_GLOBAL_ONLY);

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
    handle = GetStdHandle(STD_INPUT_HANDLE);

    if ((handle == INVALID_HANDLE_VALUE) || (handle == 0) 
	     || (GetFileType(handle) == FILE_TYPE_UNKNOWN)) {
	/*
	 * If it's a bad or closed handle, then it's been connected
	 * to a wish console window.
	 */

	tsdPtr->tty = 1;
    } else if (GetFileType(handle) == FILE_TYPE_CHAR) {
	/*
	 * A character file handle is a tty by definition.
	 */

	tsdPtr->tty = 1;
    } else {
	tsdPtr->tty = 0;
    }

#else
    tsdPtr->tty = isatty(0);
#endif
    Tcl_SetVar(interp, "tcl_interactive",
	    ((fileName == NULL) && tsdPtr->tty) ? "1" : "0", TCL_GLOBAL_ONLY);

    /*
     * Invoke application-specific initialization.
     */

    if ((*appInitProc)(interp) != TCL_OK) {
#ifdef TKX_SHELL
        TclX_ErrorExit(interp, 255,
                       "\n    while\ninitializing application (Tcl_AppInit?)");
#else
	TkpDisplayWarning(Tcl_GetStringResult(interp),
		"Application initialization failed");
#endif
    }
#ifdef TKX_SHELL
    if (Tk_InitStubs(interp, TK_VERSION, 0) == NULL) {
	abort();
    }
#endif

    /*
     * Invoke the script specified on the command line, if any.
     */

    if (fileName != NULL) {
	Tcl_ResetResult(interp);
#ifdef TKX_SHELL
	code = TclX_Eval(interp,
                         TCLX_EVAL_GLOBAL | TCLX_EVAL_FILE |
                         TCLX_EVAL_ERR_HANDLER,
                         fileName);
	if (code != TCL_OK) {
	    goto error;
	}
#else
	code = Tcl_EvalFile(interp, fileName);
	if (code != TCL_OK) {
	    /*
	     * The following statement guarantees that the errorInfo
	     * variable is set properly.
	     */

	    Tcl_AddErrorInfo(interp, "");
	    TkpDisplayWarning(Tcl_GetVar(interp, "errorInfo",
		    TCL_GLOBAL_ONLY), "Error in startup script");
	    Tcl_DeleteInterp(interp);
	    Tcl_Exit(1);
	}
#endif
	tsdPtr->tty = 0;
    } else {
#ifdef TKX_SHELL
        /*
         * Commands will come from standard input, so set up an event
         * handler for standard input.  Evaluate the .rc file, if one
         * has been specified, set up an event handler for standard
         * input, and print a prompt if the input device is a terminal.
         *
         * NOTE: At one time this was only if tcl_interactive was true.
         * This was removed due to Tk tests starting a process as a 
         * child and sending it commands on stdin.
         */
        TclX_EvalRCFile(interp);

        /*
         * Establish a channel handler for stdin.
         */
        inChannel = Tcl_GetStdChannel(TCL_STDIN);
        if (inChannel) {
            if (TclX_AsyncCommandLoop(interp,
                                      tsdPtr->tty ? (TCLX_CMDL_INTERACTIVE |
                                                     TCLX_CMDL_EXIT_ON_EOF) : 0,
                                      NULL, NULL, NULL) == TCL_ERROR)
                goto error;
        }
#else
	/*
	 * Evaluate the .rc file, if one has been specified.
	 */

	Tcl_SourceRCFile(interp);

	/*
	 * Establish a channel handler for stdin.
	 */

	inChannel = Tcl_GetStdChannel(TCL_STDIN);
	if (inChannel) {
	    Tcl_CreateChannelHandler(inChannel, TCL_READABLE, StdinProc,
		    (ClientData) inChannel);
	}
	if (tsdPtr->tty) {
	    Prompt(interp, 0);
	}
#endif
    }
    Tcl_DStringFree(&argString);

    outChannel = Tcl_GetStdChannel(TCL_STDOUT);
    if (outChannel) {
	Tcl_Flush(outChannel);
    }
    Tcl_DStringInit(&tsdPtr->command);
    Tcl_DStringInit(&tsdPtr->line);
    Tcl_ResetResult(interp);

    /*
     * Loop infinitely, waiting for commands to execute.  When there
     * are no windows left, Tk_MainLoop returns and we exit.
     */

    Tk_MainLoop();
#ifdef TKX_SHELL
    TclX_ShellExit(interp, 0);
#else
    Tcl_DeleteInterp(interp);
    Tcl_Exit(0);
#endif

#ifdef TKX_SHELL
error:
    msg = Tcl_GetVar(interp, "errorInfo", TCL_GLOBAL_ONLY);
    if ((msg == NULL) || (msg[0] == '\0')) {
	msg = interp->result;
    }
    errChannel = Tcl_GetStdChannel(TCL_STDERR);
    if (errChannel) {
        Tcl_Write(errChannel, msg, -1);
        Tcl_Write(errChannel, "\n", 1);
    }

    TclX_ShellExit (interp, 1);
#endif
}
#ifdef TKX_SHELL
/*
 * StdinProc and Prompt functions deleted.
 */
#endif
