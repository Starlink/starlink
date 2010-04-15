/*
 * tclAppInit.C --
 *
 * ---------------------------------------------------------------------
 * Note: this file is used to create a Tcl interpreter without Tk for
 * use in running local Tcl scripts during the make process.
 * ---------------------------------------------------------------------
 *
 * Copyright (c) 1993 The Regents of the University of California.
 * Copyright (c) 1994 Sun Microsystems, Inc.
 *
 * See the Tcl distribution file "license.terms" for information on usage and
 * redistribution of this file, and for a DISCLAIMER OF ALL WARRANTIES.
 */

#ifndef lint
/* static char sccsid[] = "@(#) tclAppInit.c 1.12 94/12/17 16:30:56"; */
#endif /* not lint */

#if HAVE_CONFIG_H
#include <config.h>
#endif

#include <stdlib.h>
#include <tcl.h>


/* Tcl procedure to set the gaia_library global variable and add
 * to the default search path. */
static char initScript[] = \
"global gaia_library \n\
 tcl_findLibrary gaia " PACKAGE_VERSION " " PACKAGE_VERSION " \
 GaiaTclInit.tcl GAIA_LIBRARY gaia_library \n\
 lappend auto_path $gaia_library \n\
";



/* Init functions */
extern "C" {
#include "tcl.h"
extern int Itcl_Init(Tcl_Interp *interp);
extern int Tclx_Init(Tcl_Interp *interp);
extern int Tcladam_Init(Tcl_Interp *interp);
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
 *	None: Tcl_Main never returns here, so this procedure never
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

    Tcl_Main(argc, argv, Tcl_AppInit);
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
  if (Tcl_Init(interp) == TCL_ERROR) {
    return TCL_ERROR;
  }

  if (Itcl_Init(interp) == TCL_ERROR) {
    return TCL_ERROR;
  }
  Tcl_StaticPackage(interp, "Itcl", Itcl_Init, (Tcl_PackageInitProc *) NULL);

  if (Tclx_Init(interp) == TCL_ERROR) {
    return TCL_ERROR;
  }
  if (Tcladam_Init(interp) == TCL_ERROR) {
    return TCL_ERROR;
  }

  //  Run the initialisation command.
  if ( Tcl_Eval( interp, initScript ) != TCL_OK ) {
      return TCL_ERROR;
  }

  return TCL_OK;
}
