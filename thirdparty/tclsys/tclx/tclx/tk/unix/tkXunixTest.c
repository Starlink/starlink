/* 
 * tkXunixTest.c --
 *
 *    Tcl_AppInit function for Extended Tcl Tk test program on Unix.
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
 * $Id: tkXunixTest.c,v 8.5 1999/03/31 06:37:56 markd Exp $
 *-----------------------------------------------------------------------------
 */

#include "tclExtend.h"
#include "tk.h"

int
Tktest_Init _ANSI_ARGS_((Tcl_Interp *interp));

/*
 * The following variable is a special hack that insures the tcl
 * version of matherr() is used when linking against shared libraries.
 * Even if matherr is not used on this system, there is a dummy version
 * in libtcl.
 */
extern int matherr ();
int (*tclDummyMathPtr)() = matherr;


/*
 *----------------------------------------------------------------------
 *
 * main --
 *
 *	This is the main program for the application.
 *----------------------------------------------------------------------
 */
int
main (argc, argv)
    int    argc;
    char **argv;
{
    TkX_Main(argc, argv, Tcl_AppInit);
    return 0;			/* Needed only to prevent compiler warning. */
}


/*
 *----------------------------------------------------------------------
 *
 * Tcl_AppInit --
 *
 *  Initialize TclX test application.
 *
 * Results:
 *      Returns a standard Tcl completion code, and leaves an error
 *      message in interp->result if an error occurs.
 *----------------------------------------------------------------------
 */
int
Tcl_AppInit (interp)
    Tcl_Interp *interp;
{
    if (Tcl_Init (interp) == TCL_ERROR) {
        return TCL_ERROR;
    }

    if (Tclx_Init (interp) == TCL_ERROR) {
        return TCL_ERROR;
    }
    Tcl_StaticPackage (interp, "Tclx", Tclx_Init, Tclx_SafeInit);
    if (Tk_Init(interp) == TCL_ERROR) {
	return TCL_ERROR;
    }

    Tcl_StaticPackage (interp, "Tk", Tk_Init, Tk_SafeInit);
    if (Tkx_Init (interp) == TCL_ERROR) {
	return TCL_ERROR;
    }

    Tcl_StaticPackage (interp, "Tkx", Tkx_Init, Tkx_SafeInit);
    if (Tktest_Init (interp) == TCL_ERROR) {
	return TCL_ERROR;
    }
    Tcl_StaticPackage (interp, "Tktest", Tktest_Init,
                      (Tcl_PackageInitProc *) NULL);
    return TCL_OK;
}


