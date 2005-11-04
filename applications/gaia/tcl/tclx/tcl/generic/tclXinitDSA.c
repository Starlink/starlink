/* 
 * tclXinitDSA.c --
 *
 * Dummy version of Tclx_InitStandalone for shared libraries.
 *-----------------------------------------------------------------------------
 * Copyright 1997-1999 Karl Lehenbauer and Mark Diekhans.
 *
 * Permission to use, copy, modify, and distribute this software and its
 * documentation for any purpose and without fee is hereby granted, provided
 * that the above copyright notice appear in all copies.  Karl Lehenbauer and
 * Mark Diekhans make no representations about the suitability of this
 * software for any purpose.  It is provided "as is" without express or
 * implied warranty.
 *-----------------------------------------------------------------------------
 * This file donated bu Jan Nijtman <nijtmans@worldaccess.nl>
 *-----------------------------------------------------------------------------
 * $Id: tclXinitDSA.c,v 8.2 1999/03/31 06:37:44 markd Exp $
 *-----------------------------------------------------------------------------
 */
#include "tclExtend.h"

int
Tclx_InitStandAlone (interp)
    Tcl_Interp *interp;
{
    Tcl_AppendResult (interp,
	"The function \"Tclx_InitStandAlone\" is not available in\n",
	"the shared TclX library. Link your standalone application\n",
	"with the static TclX library (libtclx.a) explicitely.",
	(char *) NULL);
    return TCL_ERROR;
}
