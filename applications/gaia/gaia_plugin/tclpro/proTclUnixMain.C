/* 
 * proTclUnixMain.c --
 *
 *	Provides a default version of the main program and Tcl_AppInit
 *	procedure for Tcl applications (without Tk).
 *
 * Copyright (c) 1998 Scriptics Corporation
 * All rights reserved.
 *
 * RCS: @(#) $Id: proTclUnixMain.C,v 1.1 1999/03/16 21:17:25 abrighto Exp $
 */


extern "C" {

#include "tcl.h"
#include <proTbcLoad.h>
#include <proWrap.h>
#include <math.h>

extern int Itcl_Init(Tcl_Interp *interp);
extern int Tclx_Init(Tcl_Interp *interp);
extern int Tcladam_Init(Tcl_Interp *interp);
extern int Tclnbs_Init(Tcl_Interp *interp);
}


/*
 * The following variable is a special hack that is needed in order for
 * Sun shared libraries to be used for Tcl.
 */

// extern int matherr();
int *tclDummyMathPtr = (int *) matherr;


static int		ProTclAppInit _ANSI_ARGS_((Tcl_Interp *interp));

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
main(int argc, char** argv)
{
    Pro_WrapTclMain(argc, argv, ProTclAppInit);

    return 0;			/* Needed only to prevent compiler warning. */
}

/*
 *----------------------------------------------------------------------
 *
 * ProTclAppInit --
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

static int
ProTclAppInit(Tcl_Interp *interp)
{
    if (Tcl_Init(interp) == TCL_ERROR) {
	return TCL_ERROR;
    }

    if (Tbcload_Init(interp) == TCL_ERROR) {
        return TCL_ERROR;
    }
    Tcl_StaticPackage((Tcl_Interp *)NULL,
		      "tbcload", Tbcload_Init, Tbcload_SafeInit);

    if (Itcl_Init(interp) == TCL_ERROR) {
	 return TCL_ERROR;
    }
    Tcl_StaticPackage(interp, "Itcl", Itcl_Init, (Tcl_PackageInitProc *) NULL);

    if (Tclx_Init(interp) == TCL_ERROR) {
	return TCL_ERROR;
    }

    // add GAIA packages 
    if (Tcladam_Init(interp) == TCL_ERROR) {
        return TCL_ERROR;
    }
    if (Tclnbs_Init(interp) == TCL_ERROR) {
        return TCL_ERROR;
    }

    return TCL_OK;
}
