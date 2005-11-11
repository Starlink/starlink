/*
 * E.S.O. - VLT project 
 * "@(#) $Id: Astrotcl.C,v 1.6 2005/02/02 01:43:04 brighton Exp $"
 *
 * Astrotcl.C - Initialize Astrotcl package
 * 
 * who             when       what
 * --------------  --------   ----------------------------------------
 * Allan Brighton  21 Nov 97  Created
 * pbiereic        26/08/99   Changed Astrotcl_Init()
 */
static const char* const rcsId="@(#) $Id: Astrotcl.C,v 1.6 2005/02/02 01:43:04 brighton Exp $";

#include <cstdlib>
#include <cstring>
#include <csignal>
#include <cstdio>
#include <iostream>
#include <sys/types.h>
#include <unistd.h>
#include <cmath>
#include <cassert>
#include <tcl.h>
#include <tk.h>
#include "error.h"
#include "define.h"

extern "C" int TclWorldCoords_Init(Tcl_Interp* interp);

// generated code for bitmaps used in tcl scripts
void defineAstrotclBitmaps(Tcl_Interp*);

// dummy Tcl command implementation
static int astrotcl_cmd(ClientData, Tcl_Interp* interp, int argc, char** argv)
{
    return TCL_OK;
}


/*
 * A call to this function is made from the tkAppInit file at startup
 * to initialize this package
 */
extern "C"
int Astrotcl_Init(Tcl_Interp* interp)  
{
    char buf[1024];

    static int initialized = 0;
    if (initialized++)
	return TCL_OK;

    // set up Tcl package
    if (Tcl_PkgProvide (interp, "Astrotcl", ASTROTCL_VERSION) != TCL_OK) {
	return TCL_ERROR;
    }

    // define bitmaps used by Tcl library
    defineAstrotclBitmaps(interp);

    // add a dummy tcl command (this command doesn't do anything currently)
    Tcl_CreateCommand(interp, "astrotcl", (Tcl_CmdProc*)astrotcl_cmd, NULL, NULL);
    
    // add the wcs command
    TclWorldCoords_Init(interp);

    // Set the global Tcl variable astrotcl_version 
    Tcl_SetVar(interp, "astrotcl_version", ASTROTCL_VERSION, TCL_GLOBAL_ONLY);

    /*
     * Use Tcl script to search for AstrotclInit.tcl and run the initialization tcl script
     */

    // defines for AstrotclInit.icc:
#   define Pkg_findinit Astrotcl_findinit
#   include "AstrotclInit.icc"

    // defines for calling the script
#   define Pkg "Astrotcl"
#   define Pkg_proc "Astrotcl_findinit"
    char* pkg_library = ASTROTCL_LIBRARY;

    Tcl_SetVar(interp, "Pkg_findinit", Pkg_proc, TCL_GLOBAL_ONLY);

    if (Tcl_GlobalEval(interp, Pkg_findinit) == TCL_ERROR)
        return TCL_ERROR;

    sprintf(buf, "%s %s %s", Pkg_proc, Pkg, pkg_library);
    if (Tcl_GlobalEval(interp, buf) == TCL_ERROR)
        return TCL_ERROR;
    
    return TCL_OK;
}
