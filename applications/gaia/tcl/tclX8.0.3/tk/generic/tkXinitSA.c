/*
 * tkXinitSA.c --
 *
 * Standalone TkX initialization.
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
 * $Id: tkXinitSA.c,v 8.1 1997/08/30 22:30:04 markd Exp $
 *-----------------------------------------------------------------------------
 */

#include "tclExtend.h"

static char *tclxIndexScript[] = {
    (char *) NULL
};

#include "./tkx.c"

static Tcl_StaticFile table[] = {
    {"tkx:tclIndex", tclxIndexScript},
    {"tkx", tkx_c},
    {(char *) NULL, (char **) NULL}
};

int
Tkx_InitStandAlone (interp)
    Tcl_Interp *interp;
{
    Tcl_DefineStaticFile(table);
    return Tkx_Init(interp);
}
