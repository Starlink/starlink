/*
 * tkXinit.c --
 *
 * Initialization code for the wishx and other Tk & Extended Tcl based
 * applications.
 *-----------------------------------------------------------------------------
 * Copyright 1991-1997 Karl Lehenbauer and Mark Diekhans.
 *
 * Permission to use, copy, modify, and distribute this software and its
 * documentation for any purpose and without fee is hereby granted, provided
 * that the above copyright notice appear in all copies.  Karl Lehenbauer and
 * Mark Diekhans make no representations about the suitability of this
 * software for any purpose.  It is provided "as is" without express or
 * implied warranty.
 *-----------------------------------------------------------------------------
 * $Id: tkXinit.c,v 8.3 1997/08/23 18:56:10 markd Exp $
 *-----------------------------------------------------------------------------
 */

#include "tclExtdInt.h"
#include "tk.h"


/*-----------------------------------------------------------------------------
 * InitSetup --
 *
 *   Common initialize function.
 *
 * Parameters:
 *   o interp - A pointer to the interpreter.
 * Returns:
 *   TCL_OK or TCL_ERROR.
 *-----------------------------------------------------------------------------
 */
static int
InitSetup (interp)
    Tcl_Interp  *interp;
{
    if (Tcl_PkgRequire (interp, "Tk", TK_VERSION, 1) == NULL) {
 	return TCL_ERROR;
    }
    if (Tcl_PkgRequire (interp, "Tclx", TCLX_VERSION, 1) == NULL) {
 	return TCL_ERROR;
    }
    return Tcl_PkgProvide (interp, "Tkx", TKX_VERSION);
}


/*-----------------------------------------------------------------------------
 * Tkx_Init --
 *
 *   Do TkX initialization.
 *
 * Parameters:
 *   o interp - A pointer to the interpreter.
 * Returns:
 *   TCL_OK or TCL_ERROR.
 *-----------------------------------------------------------------------------
 */
int Tkx_Init (interp)
    Tcl_Interp *interp;
{
    if (InitSetup(interp) != TCL_OK) {
	goto errorExit;
    }
    if (TclXRuntimeInit (interp,
                         "tk",
                         TKX_LIBRARY,
                         TKX_FULL_VERSION) == TCL_ERROR)
        goto errorExit;

    return TCL_OK;

  errorExit:
    Tcl_AddErrorInfo (interp,
                     "\n    (while initializing TkX)");
    return TCL_ERROR;
}


/*-----------------------------------------------------------------------------
 * Tkx_Init --
 *
 *   Do safe TkX initialization.
 *
 * Parameters:
 *   o interp - A pointer to the interpreter.
 * Returns:
 *   TCL_OK or TCL_ERROR.
 *-----------------------------------------------------------------------------
 */
int Tkx_SafeInit (interp)
    Tcl_Interp *interp;
{
    if (InitSetup (interp) != TCL_OK) {
	Tcl_AddErrorInfo (interp,
		     "\n    (while initializing safe TkX)");
	return TCL_ERROR;
    }
    return TCL_OK;
}
