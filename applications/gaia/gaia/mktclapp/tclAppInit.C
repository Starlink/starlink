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
 * See the file "license.terms" for information on usage and redistribution
 * of this file, and for a DISCLAIMER OF ALL WARRANTIES.
 */

#ifndef lint
/* static char sccsid[] = "@(#) tclAppInit.c 1.12 94/12/17 16:30:56"; */
#endif /* not lint */

#include <stdlib.h>


/* declare command procedures here */
extern "C" {
#include "tcl.h"
#include "gaia_stcl.h"
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
 * Et_AppInit --
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
Et_AppInit(Tcl_Interp *interp)
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

  //  Add GAIA_LIBRARY to auto_path
  char* libDir = Tcl_GetVar(interp, "gaia_library", TCL_GLOBAL_ONLY);
  if (libDir == NULL) {
      libDir = Tcl_GetVar2(interp, "env", "GAIA_LIBRARY", TCL_GLOBAL_ONLY);
  }
  if (libDir == NULL) {
      libDir = GAIA_LIBRARY;
  } 
  Tcl_SetVar(interp, "gaia_library", libDir, TCL_GLOBAL_ONLY);
  char cmd[1048];
  sprintf(cmd, "set auto_path [linsert $auto_path 0 %s]", libDir );
  if (Tcl_Eval(interp, cmd) != TCL_OK) {
      return TCL_ERROR;
  }
  
  return TCL_OK;
}

/*
 * Main function. Provide our own so that we can do the necessary Fortran
 * initialisations.
 */
int
main( int argc, char *argv[] )
{
    /* Do any work to initialise the Fortran runtime */
    initFortran( argc, argv );

    return ( Et_Init( argc, argv ) != TCL_OK );
}
