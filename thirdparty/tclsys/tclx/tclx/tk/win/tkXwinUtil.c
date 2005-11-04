/*
 * tkXwinUtil.c --
 *
 * Support routines for TkX based programs.
 *-----------------------------------------------------------------------------
 * Copyright 1996-1999 Karl Lehenbauer and Mark Diekhans.
 *
 * Permission to use, copy, modify, and distribute this software and its
 * documentation for any purpose and without fee is hereby granted, provided
 * that the above copyright notice appear in all copies.  Karl Lehenbauer and
 * Mark Diekhans make no representations about the suitability of this
 * software for any purpose.  It is provided "as is" without express or
 * implied warranty.
 *-----------------------------------------------------------------------------
 * $Id: tkXwinUtil.c,v 8.4 1999/06/26 00:25:53 surles Exp $
 *-----------------------------------------------------------------------------
 */
#include "tclExtdInt.h"
#include <tk.h>
#define WIN32_LEAN_AND_MEAN
#include <windows.h>
#undef WIN32_LEAN_AND_MEAN


/*-----------------------------------------------------------------------------
 * TkX_ConsoleInit --
 *
 * Initialize a console if the session is interacive, otherwise do nothing.
 *
 * Parameters
 *   o interp (I) - A pointer to the interpreter, error returned in result.
 * Returns:
 *   TCL_OK or TCL_ERROR.
 *-----------------------------------------------------------------------------
 */
int
TkX_ConsoleInit (Tcl_Interp *interp)
{
    char *flag = Tcl_GetVar(interp, "tcl_interactive", TCL_GLOBAL_ONLY);

    if ((flag != NULL) && !STREQU (flag, "0")) {
	if (Tk_CreateConsoleWindow (interp) == TCL_ERROR) {
            return TCL_ERROR;
	}
    }
    return TCL_OK;
}



/*-----------------------------------------------------------------------------
 * TkX_Panic --
 *
 * Output an error message in a dialog box before Tk has fully initialized.
 * Exit the process upon Ok.
 *
 * Parameters
 *   o fmt, ... (I) - Sprintf style format strinng and arguments.
 *-----------------------------------------------------------------------------
 */
void
TkX_Panic TCL_VARARGS_DEF(char *,fmt)
{
    va_list argList;
    char buf [1024];
    char *format;
    
    format = TCL_VARARGS_START (char *, fmt, argList);
    vsprintf (buf, format, argList);

    MessageBeep (MB_ICONEXCLAMATION);
    MessageBox (NULL, buf, "Fatal Error in Wish",
                MB_ICONSTOP | MB_OK | MB_TASKMODAL | MB_SETFOREGROUND);
    ExitProcess (1);
}


