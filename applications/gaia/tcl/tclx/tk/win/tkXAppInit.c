/* 
 * tkXAppInit.c --
 *
 * Provides a default version of the Tcl_AppInit procedure for use with
 * applications built with Extended Tcl and Tk on Windows 95/NT systems.
 * This is based on the the UCB Tk file tkAppInit.c
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
 * $Id: tkXAppInit.c,v 8.9 2000/07/11 04:42:08 welch Exp $
 *-----------------------------------------------------------------------------
 */

/*
 * As a main program, we cannot link against the stubs table.
 */

#ifdef USE_TCL_STUBS
#undef USE_TCL_STUBS
#endif
#ifdef USE_TK_STUBS
#undef USE_TK_STUBS
#endif

#include "tclExtend.h"
#include "tk.h"

#define WIN32_LEAN_AND_MEAN
#include <windows.h>
#undef WIN32_LEAN_AND_MEAN
#include <malloc.h>
#include <locale.h>


/*-----------------------------------------------------------------------------
 * WinMain --
 *
 * This is the main program for the application.
 *-----------------------------------------------------------------------------
 */
int APIENTRY
WinMain(hInstance, hPrevInstance, lpszCmdLine, nCmdShow)
    HINSTANCE hInstance;
    HINSTANCE hPrevInstance;
    LPSTR lpszCmdLine;
    int nCmdShow;
{
    char **argv = NULL;		/* Initialize the arg to avoid warnings. */
    int argc = 0;		/* Initialize the arg to avoid warnings. */

    /*
     * Set up the default locale to be standard "C" locale so parsing
     * is performed correctly.
     */
    setlocale(LC_ALL, "C");

    /*
     * Increase the application queue size from default value of 8.
     * At the default value, cross application SendMessage of WM_KILLFOCUS
     * will fail because the handler will not be able to do a PostMessage!
     * This is only needed for Windows 3.x, since NT dynamically expands
     * the queue.
     */

#ifndef __WIN32__
    SetMessageQueue(64);
#endif /* __WIN32__ */

    TkX_Main(argc, argv, Tcl_AppInit);

    return 0;                   /* Needed only to prevent compiler warning. */
}

/*-----------------------------------------------------------------------------
 * Tcl_AppInit --
 *
 * This procedure performs application-specific initialization. Most
 * applications, especially those that incorporate additional packages, will
 * have their own version of this procedure.
 *
 * Results:
 *    Returns a standard Tcl completion code, and leaves an error message in
 * interp->result if an error occurs.
 *-----------------------------------------------------------------------------
 */
int
Tcl_AppInit (Tcl_Interp *interp)
{
    if (Tcl_Init (interp) == TCL_ERROR) {
        goto errorExit;
    }
    if (Tclx_Init(interp) == TCL_ERROR) {
        goto errorExit;
    }
    Tcl_StaticPackage(interp, "Tclx", Tclx_Init, Tclx_SafeInit);

    if (Tk_Init(interp) == TCL_ERROR) {
        goto errorExit;
    }
    Tcl_StaticPackage(interp, "Tk", Tk_Init, Tk_SafeInit);

    if (Tkx_Init(interp) == TCL_ERROR) {
        goto errorExit;
    }
    Tcl_StaticPackage(interp, "Tkx", Tkx_Init, Tkx_SafeInit);

    /*
     * Create the console channels and install them as the standard
     * channels.  All I/O will be discarded until Tk_CreateConsoleWindow
     * is called to attach the console to a text widget.
     */
    Tk_InitConsoleChannels(interp);
    
    /*
     * Initialize the console for interactive applications.
     */
    if (TkX_ConsoleInit (interp) == TCL_ERROR)
        goto errorExit;

    /*
     * Call Tcl_CreateCommand for application-specific commands, if
     * they weren't already created by the init procedures called above.
     */

    /*
     * Specify a user-specific startup file to invoke if the application
     * is run interactively.  Typically the startup file is "~/.apprc"
     * where "app" is the name of the application.  If this line is deleted
     * then no user-specific startup file will be run under any conditions.
     */
    Tcl_SetVar(interp, "tcl_rcFileName", "~/.wishxrc", TCL_GLOBAL_ONLY);
    return TCL_OK;

  errorExit:
    TkX_Panic ("%s\n", interp->result);
    return TCL_ERROR;
}


