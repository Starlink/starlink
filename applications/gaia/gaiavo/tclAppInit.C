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

/* Tcl procedure to set the gaiavo_library global variable and add
 * to the default search path. Only reason this binary exists. */
static char initScript[] = \
"global gaiavo_library \n\
 tcl_findLibrary gaiavo " PACKAGE_VERSION " " PACKAGE_VERSION " \
 GaiaVOTclInit.tcl GAIAVO_LIBRARY gaiavo_library \n\
 lappend auto_path $gaiavo_library \n\
";

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
    Tcl_Main(argc, argv, Tcl_AppInit);
    return 0;        /* Needed only to prevent compiler warning. */
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
  
  //  Run the initialisation command.
  if ( Tcl_Eval( interp, initScript ) != TCL_OK ) {
      return TCL_ERROR;
  }

  return TCL_OK;
}
