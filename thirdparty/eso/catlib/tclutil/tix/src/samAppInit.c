/* 
 * samAppInit.c --
 *
 *	Provides a default version of the Tcl_AppInit procedure for
 *	use in stand-alone Tcl, Tk or Tix applications.
 *
 * Copyright (c) 1996, Expert Interface Technologies
 * Copyright (c) 1995 Ioi K Lam
 * Copyright (c) 1993 The Regents of the University of California.
 * Copyright (c) 1994 Sun Microsystems, Inc.
 *
 * See the file "license.terms" for information on usage and redistribution
 * of this file, and for a DISCLAIMER OF ALL WARRANTIES.
 */

#include <tcl.h>

#ifdef USE_TIX
#  ifndef USE_TK
#  define USE_TK
#  endif
#endif

#ifdef USE_TK
#include <tk.h>
#endif

#ifdef USE_TIX
#  include <tix.h>
#else
#  if (TCL_MAJOR_VERSION > 7)
#    define TCL_7_5_OR_LATER
#  else
#    if ((TCL_MAJOR_VERSION == 7) && (TCL_MINOR_VERSION >= 5))
#      define TCL_7_5_OR_LATER
#    endif
#  endif
#endif

EXTERN int		Tclsam_Init _ANSI_ARGS_((Tcl_Interp *interp));
EXTERN int		Tksam_Init _ANSI_ARGS_((Tcl_Interp *interp));
EXTERN int		Tixsam_Init _ANSI_ARGS_((Tcl_Interp *interp));

/*
 * The following variable is a special hack that is needed in order for
 * Sun shared libraries to be used for Tcl.
 */

extern int matherr();
int *tclDummyMathPtr = (int *) matherr;

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
main(argc, argv)
    int argc;			/* Number of command-line arguments. */
    char **argv;		/* Values of command-line arguments. */
{
#ifdef USE_TK
    Tk_Main(argc, argv, Tcl_AppInit);
#else
    Tcl_Main(argc, argv, Tcl_AppInit);
#endif

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

int
Tcl_AppInit(interp)
    Tcl_Interp *interp;		/* Interpreter for application. */
{
    if (Tclsam_Init(interp) == TCL_ERROR) {
	return TCL_ERROR;
    }
#ifdef USE_TK
    if (Tksam_Init(interp) == TCL_ERROR) {
	return TCL_ERROR;
    }
#endif

#ifdef USE_TIX
    if (Tixsam_Init(interp) == TCL_ERROR) {
	return TCL_ERROR;
    }
#endif

#ifdef TCL_7_5_OR_LATER
    Tcl_StaticPackage(interp, "Tclsam", Tclsam_Init, NULL);
#ifdef USE_TK
    Tcl_StaticPackage(interp, "Tk",     Tk_Init,     NULL);
    Tcl_StaticPackage(interp, "Tksam",  Tksam_Init,  NULL);
#endif
#ifdef USE_TIX
    Tcl_StaticPackage(interp, "Tix",    Tix_Init,    NULL);
    Tcl_StaticPackage(interp, "Tixsam", Tixsam_Init, NULL);
#endif
#endif

    /*
     * Call the init procedures for included packages.  Each call should
     * look like this:
     *
     * if (Mod_Init(interp) == TCL_ERROR) {
     *     return TCL_ERROR;
     * }
     *
     * where "Mod" is the name of the module.
     */

    /*
     * Call Tcl_CreateCommand for application-specific commands, if
     * they weren't already created by the init procedures called above.
     */

    /*
     * Specify a user-specific startup file to invoke if the application
     * is run interactively.  Typically the startup file is "~/.apprc"
     * where "app" is the name of the application.  If this line is deleted
     * then no user-specific startup file will be run under any conditions.
     */
#if defined(USE_TIX)
#  define RC_FILENAME "~/.tixwishrc"
#else
#  if defined(USE_TK)
#    define RC_FILENAME "~/.wishrc"
#  else
#    define RC_FILENAME "~/.tclshrc"
#  endif
#endif

#ifdef TCL_7_5_OR_LATER
    /*
     * Starting from TCL 7.5, the symbol tcl_rcFileName is no longer
     * exported by libtcl.a. Instead, this variable must be set using
     * a TCL global variable
     */
    Tcl_SetVar(interp, "tcl_rcFileName", RC_FILENAME, TCL_GLOBAL_ONLY);
#else
    tcl_RcFileName = RC_FILENAME;
#endif

    return TCL_OK;
}
