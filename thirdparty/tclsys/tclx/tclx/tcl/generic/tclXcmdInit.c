/*
 * tclXcmdInit.c
 *
 * Function to add the Extented Tcl commands into an interpreter.  The TclX
 * library commands are not added here, to make it easier to build applications
 * that don't use the extended libraries.
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
 * $Id: tclXcmdInit.c,v 8.7 1997/07/01 02:26:14 markd Exp $
 *-----------------------------------------------------------------------------
 */

#include "tclExtdInt.h"


/*-----------------------------------------------------------------------------
 * Tclxcmd_Init --
 *
 *   Add the Extended Tcl commands to the specified interpreter (except for
 * the library commands that override that standard Tcl procedures).  This
 * does no other startup.
 *-----------------------------------------------------------------------------
 */
int
Tclxcmd_Init (interp)
    Tcl_Interp *interp;
{
    if (Tclxcmd_SafeInit (interp) == TCL_ERROR)
        return TCL_ERROR;

#ifdef TCL_MEM_DEBUG    
    /*
     * from tclCkalloc.c (now part of the UCB Tcl).
     */
    Tcl_InitMemory (interp);
#endif

    TclX_ChmodInit (interp);
    TclX_CmdloopInit (interp);
    TclX_DebugInit (interp);
    TclX_DupInit (interp);
    TclX_FcntlInit (interp);
    TclX_FilecmdsInit (interp);
    TclX_FstatInit (interp);
    TclX_MsgCatInit (interp);
    TclX_ProcessInit (interp);
    TclX_SignalInit (interp);
    TclX_OsCmdsInit (interp);
    TclX_PlatformCmdsInit (interp);
    TclX_SocketInit (interp);
    TclX_ServerInit (interp);

    return TCL_OK;
}


/*-----------------------------------------------------------------------------
 * Tclxcmd_SafeInit --
 *
 *   Add the safe Extended Tcl commands to the specified interpreter.
 *-----------------------------------------------------------------------------
 */
int
Tclxcmd_SafeInit (interp)
    Tcl_Interp *interp;
{
    TclX_SetAppInfo (TRUE,
                     "TclX",
                     "Extended Tcl",
                     TCLX_FULL_VERSION,
                     TCLX_PATCHLEVEL);

    TclX_BsearchInit (interp);
    TclX_FstatInit (interp);
    TclX_FlockInit (interp);
    TclX_FilescanInit (interp);
    TclX_GeneralInit (interp);
    TclX_IdInit (interp);
    TclX_KeyedListInit (interp);
    TclX_LgetsInit (interp);
    TclX_ListInit (interp);
    TclX_MathInit (interp);
    TclX_ProfileInit (interp);
    TclX_SelectInit (interp);
    TclX_StringInit (interp);

    return TCL_OK;
}


