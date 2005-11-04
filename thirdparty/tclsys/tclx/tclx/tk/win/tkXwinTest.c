/* 
 * tclXwinTest.c --
 *
 * Tcl_AppInit and main functions for the Extended Tcl test program on Win32.
 *
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
 * $Id: tkXwinTest.c,v 8.4 1999/06/26 00:25:53 surles Exp $
 *-----------------------------------------------------------------------------
 */

#include "tclExtdInt.h"
#include "tk.h"

#define WIN32_LEAN_AND_MEAN
#include <windows.h>
#undef WIN32_LEAN_AND_MEAN
#include <malloc.h>
#include <locale.h>

int
Tktest_Init (Tcl_Interp *interp);


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
    char **argv;
    int argc;

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
    SetMessageQueue(64);

    TkX_Main(argc, argv, Tcl_AppInit);

    return 0;                   /* Needed only to prevent compiler warning. */
}

/*-----------------------------------------------------------------------------
 * Tcl_AppInit --
 *  Initialize TclX test application.
 *
 * Results:
 *   Returns a standard Tcl completion code, and leaves an error message in
 * interp->result if an error occurs.
 *-----------------------------------------------------------------------------
 */
int
Tcl_AppInit (interp)
    Tcl_Interp *interp;
{
    if (Tcl_Init (interp) == TCL_ERROR) {
        goto errorExit;
    }

    if (Tclx_Init (interp) == TCL_ERROR) {
        goto errorExit;
    }
    Tcl_StaticPackage (interp, "Tclx", Tclx_Init, Tclx_SafeInit);

    if (Tk_Init(interp) == TCL_ERROR) {
        goto errorExit;
    }
    Tcl_StaticPackage(interp, "Tk", Tk_Init, (Tcl_PackageInitProc *) NULL);

    if (Tkx_Init(interp) == TCL_ERROR) {
        goto errorExit;
    }
    Tcl_StaticPackage(interp, "Tkx", Tkx_Init, (Tcl_PackageInitProc *) NULL);

    /*
     * Create the console channels and install them as the standard
     * channels.  All I/O will be discarded until Tk_CreateConsoleWindow
     * is called to attach the console to a text widget.
     */
    Tk_InitConsoleChannels(interp);

    if (TkX_ConsoleInit (interp) == TCL_ERROR)
        goto errorExit;

    if (Tktest_Init(interp) == TCL_ERROR) {
	goto errorExit;
    }
    Tcl_StaticPackage (interp, "Tktest", Tktest_Init,
                      (Tcl_PackageInitProc *) NULL);

    return TCL_OK;

  errorExit:
    TkX_Panic ("%s\n", interp->result);
}


