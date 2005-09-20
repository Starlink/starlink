/*
 * tkAppInit.C --
 *
 * ---------------------------------------------------------------------
 * NOTE: This file was modified by adding the rtdimage extension
 *       as well as the BLT, Itcl and TclX extensions.
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
/* static char sccsid[] = "@(#) tkAppInit.c 1.12 94/12/17 16:30:56"; */
#endif /* not lint */

/* #include <config.h> */

#include <stdlib.h>

/* declare command procedures here */
extern "C" {
#include "tk.h"
extern int Blt_Init(Tcl_Interp *interp);
extern int Itcl_Init(Tcl_Interp *interp);
extern int Itk_Init(Tcl_Interp *interp);
extern int Tclx_Init(Tcl_Interp *interp);
extern int Tkhtml_Init(Tcl_Interp *interp);
#if SHARED == 0
extern int Skycat_Init(Tcl_Interp *interp);
extern int Gaia_Init(Tcl_Interp *interp);
#endif
}

/* Fortran initialisation. */
extern "C" {
void initFortran( int argc, char *argv[] );
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
main( int argc, char *argv[] )
{
    /*  Initialise the Fortran runtime, if necessary */
    initFortran(argc, argv);

    Tk_Main( argc, argv, Tcl_AppInit );
    return 0;  /* Needed only to prevent compiler warning. */
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

    if (Tclx_Init(interp) == TCL_ERROR) {
	return TCL_ERROR;
    }
    Tcl_StaticPackage (interp, "Tclx", Tclx_Init, (Tcl_PackageInitProc *) NULL);

    if (Tkhtml_Init(interp) == TCL_ERROR ) {
        return TCL_ERROR;
    }
    Tcl_StaticPackage (interp, "TkHTML", Tkhtml_Init, (Tcl_PackageInitProc *) NULL);

#if SHARED == 0

    /* install the Skycat package */
    if (Skycat_Init(interp) == TCL_ERROR) {
	return TCL_ERROR;
    }
    Tcl_StaticPackage (interp, "Skycat", Skycat_Init, (Tcl_PackageInitProc *) NULL);

    /* install the Gaia package  */
    if (Gaia_Init(interp) == TCL_ERROR) {
	return TCL_ERROR;
    }
    Tcl_StaticPackage (interp, "Gaia", Gaia_Init, (Tcl_PackageInitProc *) NULL);

#endif

    /* ------------------------------------------------------------*/


    return TCL_OK;
}
