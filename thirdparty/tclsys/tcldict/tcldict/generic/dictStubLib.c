/*
 * dictStubLib.c --
 *
 *	This file contains stubs init code for the dict extension.
 *
 * Shamelessly copied from http://wiki.tcl.tk/3358
 * Copyright (c) 2004 by Pascal Scheffers
 *
 * See the file "license.terms" for information on usage and redistribution
 * of this file, and for a DISCLAIMER OF ALL WARRANTIES.
 *
 * RCS: @(#) $Id$
 */
#include "tcl.h"

#ifndef USE_TCL_STUBS
#define USE_TCL_STUBS
#endif
#undef USE_TCL_STUB_PROCS

#include "tclDict.h"

#undef TCL_STORAGE_CLASS
#define TCL_STORAGE_CLASS DLLEXPORT

DictStubs *dictStubsPtr;

/*
**----------------------------------------------------------------------
**
** Dict_InitStubs --
**
**	Checks that the correct version of dict is loaded and that it
**	supports stubs. It then initialises the stub table pointers.
**
** Results:
**	The actual version of dict that satisfies the request, or
**	NULL to indicate that an error occurred.
**
** Side effects:
**	Sets the stub table pointers.
**
**----------------------------------------------------------------------
*/

CONST char *
Dict_InitStubs (Tcl_Interp *interp, CONST char *version, int exact)
{
    char *actualVersion;

    actualVersion = Tcl_PkgRequireEx(interp, "dict", version, exact,
				     (ClientData *) &dictStubsPtr);
    if (!actualVersion) {
        return NULL;
    }

    if (!dictStubsPtr) {
        Tcl_SetResult(interp,
                      "This implementation of Extension does not support stubs",
                      TCL_STATIC);
        return NULL;
    }

    return actualVersion;
}
