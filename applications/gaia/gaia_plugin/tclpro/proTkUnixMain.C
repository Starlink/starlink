/* 
 * proTkUnixMain.c -- 
 *
 *	Provides a default version of the Tcl_AppInit procedure for
 *	use in wish and similar Tk-based applications.
 *
 * Copyright (c) 1998 Scriptics Corporation
 * All rights reserved.
 *
 * SCCS: @(#) proTkUnixMain.c 1.8 98/08/21 13:27:34
 *
 * who             when       what
 * --------------  --------   ----------------------------------------
 * Allan Brighton  29 Oct 98  Modified from Tclpro-1.0, changed C to C++
 */

/* declare command procedures here */
extern "C" {
#include "tk.h"
#include "itcl.h"
#include "itk.h"
#include "tclExtend.h"
#include "blt.h"
#include <math.h>
#include <proTbcLoad.h>
#include <proWrap.h>

#define RCFILE "~/.itkshrc"

extern int Blt_Init(Tcl_Interp *interp);
extern int Itcl_Init(Tcl_Interp *interp);
extern int Itk_Init(Tcl_Interp *interp);
extern int Tclx_Init(Tcl_Interp *interp);
extern int Skycat_Init(Tcl_Interp *interp);
extern int Gaia_Init(Tcl_Interp *interp);
}

/*
 * The following variable is a special hack that is needed in order for
 * Sun shared libraries to be used for Tcl.
 */

// extern "C" int matherr(struct exception*);
int *tclDummyMathPtr = (int *) matherr;

#ifdef TK_TEST
EXTERN int		Tktest_Init _ANSI_ARGS_((Tcl_Interp *interp));
#endif /* TK_TEST */

EXTERN Tcl_Namespace *  Tcl_GetGlobalNamespace _ANSI_ARGS_((
			    Tcl_Interp *interp));
EXTERN int		Tcl_Import _ANSI_ARGS_((Tcl_Interp *interp,
			    Tcl_Namespace *nsPtr, char *pattern,
			    int allowOverwrite));

static int		ProTclAppInit _ANSI_ARGS_((Tcl_Interp *interp));

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
    Pro_WrapTkMain(argc, argv, ProTclAppInit);
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

static int
ProTclAppInit(Tcl_Interp* interp)
{
    if (Tcl_Init(interp) == TCL_ERROR) {
	return TCL_ERROR;
    }
    if (Tk_Init(interp) == TCL_ERROR) {
	return TCL_ERROR;
    }
    Tcl_StaticPackage(interp, "Tk", Tk_Init, Tk_SafeInit);

    if (Tbcload_Init(interp) == TCL_ERROR) {
        return TCL_ERROR;
    }
    Tcl_StaticPackage(interp, "tbcload", Tbcload_Init, Tbcload_SafeInit);

    if (Itcl_Init(interp) == TCL_ERROR) {
	return TCL_ERROR;
    }
    if (Itk_Init(interp) == TCL_ERROR) {
	return TCL_ERROR;
    }
    Tcl_StaticPackage(interp, "Itcl", Itcl_Init, Itcl_SafeInit);
    Tcl_StaticPackage(interp, "Itk", Itk_Init, (Tcl_PackageInitProc *) NULL);

    /*
     *  This is itkwish, so import all [incr Tcl] commands by
     *  default into the global namespace.  Set the "itcl::native"
     *  variable so we can do the same kind of import automatically
     *  during the "auto_mkindex" operation.
     */

    if (Tcl_Import(interp, Tcl_GetGlobalNamespace(interp),
            "::itk::*", /* allowOverwrite */ 1) != TCL_OK) {
        return TCL_ERROR;
    }

    if (Tcl_Import(interp, Tcl_GetGlobalNamespace(interp),
            "::itcl::*", /* allowOverwrite */ 1) != TCL_OK) {
        return TCL_ERROR;
    }

    if (!Tcl_SetVar(interp, "::itcl::native", "1", TCL_LEAVE_ERR_MSG)) {
        return TCL_ERROR;
    }

    if (Tcl_PkgRequire(interp, "Iwidgets", "3.0", 0) == NULL) {
	return TCL_ERROR;
    }

    // use relative pathnames for extensions when using prowrap
    char buf[32];
    sprintf(buf, "tclX%s", TCLX_VERSION);
    if (!Tcl_SetVar2(interp, "::env", "TCLX_LIBRARY", buf, TCL_LEAVE_ERR_MSG)) {
        return TCL_ERROR;
    }

    sprintf(buf, "blt%s", BLT_VERSION);
    if (!Tcl_SetVar2(interp, "::env", "BLT_LIBRARY", buf, TCL_LEAVE_ERR_MSG)) {
        return TCL_ERROR;
    }

    if (Tclx_Init(interp) == TCL_ERROR) {
	return TCL_ERROR;
    }
    Tcl_StaticPackage (interp, "Tclx", Tclx_Init, (Tcl_PackageInitProc *) NULL);

    if (Blt_Init(interp) == TCL_ERROR) {
	return TCL_ERROR;
    }

    // use relative paths to the build versions for skycat sources for prowrap
    if (!Tcl_SetVar2(interp, "::env", "TCLUTIL_LIBRARY", "tclutil/library", TCL_LEAVE_ERR_MSG)
	|| !Tcl_SetVar2(interp, "::env", "ASTROTCL_LIBRARY", "astrotcl/library", TCL_LEAVE_ERR_MSG)
	|| !Tcl_SetVar2(interp, "::env", "RTD_LIBRARY", "rtdimg/library", TCL_LEAVE_ERR_MSG)
	|| !Tcl_SetVar2(interp, "::env", "CAT_LIBRARY", "tclcat/library", TCL_LEAVE_ERR_MSG)
	|| !Tcl_SetVar2(interp, "::env", "SKYCAT_LIBRARY", "interp/library", TCL_LEAVE_ERR_MSG)
	|| !Tcl_SetVar2(interp, "::env", "GAIA_LIBRARY", "gaia/library", TCL_LEAVE_ERR_MSG)) {
        return TCL_ERROR;
    }

    // install the Skycat package 
    if (Skycat_Init(interp) == TCL_ERROR) {
	return TCL_ERROR;
    }
    Tcl_StaticPackage (interp, "Skycat", Skycat_Init, (Tcl_PackageInitProc *) NULL);

    // install the Gaia package 
    if (Gaia_Init(interp) == TCL_ERROR) {
	return TCL_ERROR;
    }
    Tcl_StaticPackage (interp, "Gaia", Gaia_Init, (Tcl_PackageInitProc *) NULL);

    // Setup the GAIA environment
    // get the name of the directory
    char* path = strdup(Tcl_GetNameOfExecutable());
    char* p = strrchr(path, '/');
    if (p)
	*p = '\0';

    Tcl_SetVar2(interp, "::env", "TCLADAM_DIR", path, TCL_LEAVE_ERR_MSG);
    Tcl_SetVar2(interp, "::env", "CONVERT_DIR", path, TCL_LEAVE_ERR_MSG);
    Tcl_SetVar2(interp, "::env", "PHOTOM_DIR", path, TCL_LEAVE_ERR_MSG);
    Tcl_SetVar2(interp, "::env", "KAPPA_DIR", path, TCL_LEAVE_ERR_MSG);
    Tcl_SetVar2(interp, "::env", "STARLINK", path, TCL_LEAVE_ERR_MSG);

    /*
     * Specify a user-specific startup file to invoke if the application
     * is run interactively.  Typically the startup file is "~/.apprc"
     * where "app" is the name of the application.  If this line is deleted
     * then no user-specific startup file will be run under any conditions.
     */

    Tcl_SetVar(interp, "tcl_rcFileName", RCFILE, TCL_GLOBAL_ONLY);
    return TCL_OK;
}
