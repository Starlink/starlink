/* 
 * tkAppInit.C -- 
 * "@(#) $Id: tkAppInit.C,v 1.2 1997/12/04 17:46:45 abrighto Exp $"
 * ---------------------------------------------------------------------
 * NOTE: This file was modified by adding the BLT, Itcl and TclX extensions.
 *       It was also modified to be compiled with a C++ compiler,
 *       although this is not strictly necessary. It is only required
 *       that the actual "main()" be compiled with C++, not the rest
 *       of tkAppInit.c -- Allan
 * ---------------------------------------------------------------------
 *
 * Copyright (c) 1993 The Regents of the University of California.
 * Copyright (c) 1994 Sun Microsystems, Inc.
 *
 * See the file "license.terms" for information on usage and redistribution
 * of this file, and for a DISCLAIMER OF ALL WARRANTIES.
 */

#ifndef lint
static char sccsid[] = "@(#) tkAppInit.c 1.12 94/12/17 16:30:56";
#endif /* not lint */

#include <stdlib.h>
#include <new.h>


/* declare command procedures here */
extern "C" {
#include "tk.h"
extern int Blt_Init(Tcl_Interp *interp);
extern int Itcl_Init(Tcl_Interp *interp);
extern int Itk_Init(Tcl_Interp *interp);
extern int Tclx_Init(Tcl_Interp *interp);
#if SHARED == 0
extern int Tclutil_Init(Tcl_Interp *interp);
extern int Astrotcl_Init(Tcl_Interp *interp);
#endif
}

/*
 * The following variable is a special hack that is needed in order for
 * Sun shared libraries to be used for Tcl.
 */

#ifdef NEED_MATHERR
extern "C" int matherr();
int *tclDummyMathPtr = (int *) matherr;
#endif



/*
 *----------------------------------------------------------------------
 *
 * main --
 *
 *	This is the main program for the application.
 *
 * Results:
 *	None: Tk_Main never returns here, so this procedure never
 *	returns either.
 *
 * Side effects:
 *	Whatever the application does.
 *
 *----------------------------------------------------------------------
 */

int
main(int argc, char** argv)
{
    Tk_Main(argc, argv, Tcl_AppInit);
    return 0;			/* Needed only to prevent compiler warning. */
}

/*
 *----------------------------------------------------------------------
 *
 * Tcl_AppInit --
 *
 *	This procedure performs application-specific initialization.
 *	Most applications, especially those that incorporate additional
 *	packages, will have their own version of this procedure.
 *
 * Results:
 *	Returns a standard Tcl completion code, and leaves an error
 *	message in interp->result if an error occurs.
 *
 * Side effects:
 *	Depends on the startup script.
 *
 *----------------------------------------------------------------------
 */

extern "C" int
Tcl_AppInit(Tcl_Interp *interp)
{
    Tk_Window main;

    main = Tk_MainWindow(interp);

    if (Tcl_Init(interp) == TCL_ERROR) {
	return TCL_ERROR;
    }
    if (Tk_Init(interp) == TCL_ERROR) {
	return TCL_ERROR;
    }
    Tcl_StaticPackage(interp, "Tk", Tk_Init, (Tcl_PackageInitProc *) NULL);

    if (Itcl_Init(interp) == TCL_ERROR) {
         return TCL_ERROR;
    }
    Tcl_StaticPackage(interp, "Itcl", Itcl_Init, (Tcl_PackageInitProc *) NULL);

    if (Itk_Init(interp) == TCL_ERROR) {
	return TCL_ERROR;
    }
    Tcl_StaticPackage(interp, "Itk", Itk_Init, (Tcl_PackageInitProc *) NULL);


    if (Blt_Init(interp) == TCL_ERROR) {
	return TCL_ERROR;
    }

    /* add tclX commands, but not the whole tclX env */
    Tclx_Init(interp);
    Tcl_StaticPackage (interp, "Tclx", Tclx_Init, (Tcl_PackageInitProc *) NULL);


#if SHARED == 0
    /* initialize the tclutil package */
    if (Tclutil_Init(interp) == TCL_ERROR) {
	return TCL_ERROR;
    }
    Tcl_StaticPackage (interp, "Tclutil", Tclutil_Init, (Tcl_PackageInitProc *) NULL);

    /* initialize the astrotcl package */
    if (Astrotcl_Init(interp) == TCL_ERROR) {
	return TCL_ERROR;
    }
    Tcl_StaticPackage (interp, "Astrotcl", Astrotcl_Init, (Tcl_PackageInitProc *) NULL);
#endif


    // Required by TCL
    Tcl_SetVar(interp, "tcl_rcFileName", "~/.tclshrc", TCL_GLOBAL_ONLY);
 
    return TCL_OK;
}
