/*
 * tixUnixWm.c --
 *
 *	Implement the Windows specific function calls for window management.
 *
 * Copyright (c) 1996, Expert Interface Technologies
 *
 * See the file "license.terms" for information on usage and redistribution
 * of this file, and for a DISCLAIMER OF ALL WARRANTIES.
 *
 */

#include "tixUnixInt.h"

int
TixpSetWindowParent(interp, tkwin, newParent, parentId)
    Tcl_Interp * interp;
    Tk_Window tkwin;
    Tk_Window newParent;
    int parentId;
{
    return TCL_OK;
}
