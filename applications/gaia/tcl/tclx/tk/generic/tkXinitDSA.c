/* 
 * tkXinitDSA.c --
 *
 * Dummy version of Tkx_InitStandalone for shared libraries.
 *-----------------------------------------------------------------------------
 * Copyright 1997-1997 Karl Lehenbauer and Mark Diekhans.
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
 * $Id: tkXinitDSA.c,v 8.1 1997/08/30 22:30:03 markd Exp $
 *-----------------------------------------------------------------------------
 */
#include "tclExtend.h"

int
Tkx_InitStandAlone (interp)
    Tcl_Interp *interp;
{
    Tcl_AppendResult (interp,
	"The function \"Tkx_InitStandAlone\" is not available in\n",
	"the shared TkX library. Link your standalone application\n",
	"with the static TkX library (libtkx.a) explicitely.",
	(char *) NULL);
    return TCL_ERROR;
}
