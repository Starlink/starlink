/*
 * E.S.O. - VLT project/ESO Archive
 * $Id: tkAppInit.C,v 1.12 1999/01/15 22:06:59 abrighto Exp $
 *
 * tkAppInit.C -- This version is based on ET - Embedded Tk.
 *
 * This file is processed with et2c to produce the C++ source file
 * containing all of the necessary Tcl scripts to form a single
 * binary version of the skycat.
 *
 * who             when       what
 * --------------  --------   ----------------------------------------
 * Allan Brighton  14 Nov 97  Created
 */

#ifndef lint
/* static char sccsid[] = "@(#) tkAppInit.c 1.12 94/12/17 16:30:56"; */
#endif /* not lint */

#include <stdlib.h>
#include <unistd.h>
#include <string.h>
#include <stdio.h>
#include <fcntl.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <iostream.h>

/* declare command procedures here */
extern "C" {
#include "tk.h"
extern int Blt_Init(Tcl_Interp *interp);
extern int Itcl_Init(Tcl_Interp *interp);
extern int Itk_Init(Tcl_Interp *interp);
extern int Tclx_Init(Tcl_Interp *interp);

extern int Skycat_Init(Tcl_Interp *interp);
}


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
    Et_Init(&argc,argv);
    Tcl_Interp* interp = Et_Interp;

    // -------------add extensions---------------------------------
    if (Itcl_Init(interp) == TCL_ERROR) {
	// cerr << interp->result << endl; 
	// return TCL_ERROR;
    }
    Tcl_StaticPackage(interp, "Itcl", Itcl_Init, (Tcl_PackageInitProc *) NULL);

    if (Itk_Init(interp) == TCL_ERROR) {
	// cerr << interp->result << endl; 
	// return TCL_ERROR;
    }
    Tcl_StaticPackage(interp, "Itk", Itk_Init, (Tcl_PackageInitProc *) NULL);

    if (Blt_Init(interp) == TCL_ERROR) {
	// cerr << interp->result << endl; 
	// return TCL_ERROR;
    }

    if (Tclx_Init(interp) == TCL_ERROR) {
	// cerr << interp->result << endl; 
	// return TCL_ERROR;
    }
    Tcl_StaticPackage (interp, "Tclx", Tclx_Init, (Tcl_PackageInitProc *) NULL);

    // install the Skycat package 
    if (Skycat_Init(interp) == TCL_ERROR) {
	cerr << interp->result << endl; 
	return TCL_ERROR;
    }
    Tcl_StaticPackage (interp, "Skycat", Skycat_Init, (Tcl_PackageInitProc *) NULL);


    // ----- load all Tcl source files ----------------------------

    ET_INSTALL_COMMANDS;
    ET_INCLUDE( skycat.tcl );
    ET( skycat::SkyCat::startSkyCat );

    // ------------------------------------------------------------
    Et_MainLoop();
}


