
/*
 * bltDebug.c --
 *
 * Copyright 1993-1998 Lucent Technologies, Inc.
 *
 * Permission to use, copy, modify, and distribute this software and
 * its documentation for any purpose and without fee is hereby
 * granted, provided that the above copyright notice appear in all
 * copies and that both that the copyright notice and warranty
 * disclaimer appear in supporting documentation, and that the names
 * of Lucent Technologies any of their entities not be used in
 * advertising or publicity pertaining to distribution of the software
 * without specific, written prior permission.
 *
 * Lucent Technologies disclaims all warranties with regard to this
 * software, including all implied warranties of merchantability and
 * fitness.  In no event shall Lucent Technologies be liable for any
 * special, indirect or consequential damages or any damages
 * whatsoever resulting from loss of use, data or profits, whether in
 * an action of contract, negligence or other tortuous action, arising
 * out of or in connection with the use or performance of this
 * software.
 */

#include "bltInt.h"

#ifndef NO_BLTDEBUG

static Blt_List watchList;

#ifdef __STDC__
static Tcl_CmdTraceProc DebugProc;
static Tcl_CmdProc DebugCmd;
#endif

/*ARGSUSED*/
static void
DebugProc(clientData, interp, level, command, proc, cmdClientData,
    argc, argv)
    ClientData clientData;	/* not used */
    Tcl_Interp *interp;		/* not used */
    int level;			/* Current level */
    char *command;		/* Command before substitution */
    Tcl_CmdProc *proc;		/* not used */
    ClientData cmdClientData;	/* not used */
    int argc;
    char **argv;		/* Command after parsing, but before
				 * evaluation */
{
    static unsigned char traceStack[200];
    register int i;
    char *string;
    Tcl_Channel errChannel;
    Tcl_DString dString;

    /* This is pretty crappy, but there's no way to trigger stack pops */
    for (i = level + 1; i < 200; i++) {
	traceStack[i] = 0;
    }
    if (Blt_ListGetLength(&watchList) > 0) {
	register Blt_ListItem item;

	for (item = Blt_ListFirstItem(&watchList); item != NULL;
	    item = Blt_ListNextItem(item)) {
	    if (Tcl_StringMatch(argv[0], Blt_ListGetKey(item))) {
		break;
	    }
	}
	if ((item != NULL) && (level < 200)) {
	    traceStack[level] = 1;
	    traceStack[level + 1] = 1;
	}
	if ((level >= 200) || (!traceStack[level])) {
	    return;
	}
    }
    /*
     * Use stderr channel, for compatibility with systems that don't have a
     * tty (like WIN32).  In reality, it doesn't make a difference since
     * the WIN32 console can't handle large streams of data anyways.
     */
    errChannel = Tcl_GetStdChannel(TCL_STDERR);
    if (errChannel == NULL) {
	Tcl_AppendResult(interp, "can't get stderr channel", (char *)NULL);
	Tk_BackgroundError(interp);
	return;
    }
    Tcl_DStringInit(&dString);
    Tcl_DStringAppend(&dString, Blt_Int(level), -1);
    Tcl_DStringAppend(&dString, ">\t", -1);
    Tcl_DStringAppend(&dString, command, -1);
    Tcl_DStringAppend(&dString, "\n\t", -1);
    string = Tcl_Merge(argc, argv);
    Tcl_DStringAppend(&dString, string, -1);
    free(string);
    Tcl_DStringAppend(&dString, "\n", -1);
    Tcl_Write(errChannel, (char *)Tcl_DStringValue(&dString), -1);
    Tcl_Flush(errChannel);
    Tcl_DStringFree(&dString);
}

/*ARGSUSED*/
static int
DebugCmd(clientData, interp, argc, argv)
    ClientData clientData;	/* not used */
    Tcl_Interp *interp;
    int argc;
    char **argv;
{
    static Tcl_Trace token;
    static int level;
    int newLevel;
    char c;
    int length;
    Blt_ListItem item;
    register int i;

    if (argc == 1) {
	Tcl_SetResult(interp, Blt_Int(level), TCL_VOLATILE);
	return TCL_OK;
    }
    c = argv[1][0];
    length = strlen(argv[1]);
    if ((c == 'w') && (strncmp(argv[1], "watch", length) == 0)) {
	/* Add patterns of command names to watch to the list */
	for (i = 2; i < argc; i++) {
	    item = Blt_ListFind(&watchList, argv[i]);
	    if (item == NULL) {
		Blt_ListAppend(&watchList, argv[i], (ClientData)0);
	    }
	}
    } else if ((c == 'i') && (strncmp(argv[1], "ignore", length) == 0)) {
	for (i = 2; i < argc; i++) {
	    Blt_ListDelete(&watchList, argv[i]);
	}
    } else {
	goto levelTest;
    }
    /* Return the current watch patterns */
    for (item = Blt_ListFirstItem(&watchList); item != NULL;
	item = Blt_ListNextItem(item)) {
	Tcl_AppendElement(interp, Blt_ListGetKey(item));
    }
    return TCL_OK;

  levelTest:
    if (Tcl_GetBoolean(interp, argv[1], &newLevel) == TCL_OK) {
	if (newLevel > 0) {
	    newLevel = 10000;	/* Max out the level */
	}
    } else if (Tcl_GetInt(interp, argv[1], &newLevel) == TCL_OK) {
	if (newLevel < 0) {
	    newLevel = 0;
	}
    } else {
	return TCL_ERROR;
    }
    if (token != 0) {
	Tcl_DeleteTrace(interp, token);
    }
    if (newLevel > 0) {
	token = Tcl_CreateTrace(interp, newLevel, DebugProc, (ClientData)0);
    }
    level = newLevel;
    Tcl_SetResult(interp, Blt_Int(level), TCL_VOLATILE);
    return TCL_OK;
}

int
Blt_DebugInit(interp)
    Tcl_Interp *interp;
{
    static Blt_CmdSpec cmdSpec =
    {"bltdebug", DebugCmd,};

    Blt_InitList(&watchList, TCL_STRING_KEYS);
    if (Blt_InitCmd(interp, "blt", &cmdSpec) == NULL) {
	return TCL_ERROR;
    }
    return TCL_OK;
}

#endif /* NO_BLTDEBUG */
