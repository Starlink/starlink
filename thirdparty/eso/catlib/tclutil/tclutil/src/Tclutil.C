/*
 * E.S.O. - VLT project 
 * "@(#) $Id: Tclutil.C,v 1.10 2001/08/27 10:11:14 abrighto Exp $"
 *
 * Tclutil.C - Initialize Tclutil package
 * 
 * who             when       what
 * --------------  --------   ----------------------------------------
 * Allan Brighton  21 Nov 97  Created
 * pbiereic        26/08/99   Changed Tclutil_Init()

 */
static const char* const rcsId="@(#) $Id: Tclutil.C,v 1.10 2001/08/27 10:11:14 abrighto Exp $";

#include <stdlib.h>
#include <signal.h>
#include <stdio.h>
#include <iostream.h>
#include <strstream.h>
#include <sys/types.h>
#include <unistd.h>
#include <math.h>
#include <assert.h>
#include <string.h>
#include <tcl.h>
#include <tk.h>
#include "error.h"
#include "define.h"
#include "config.h"

// generated code for bitmaps used in tcl scripts
void defineTclutilBitmaps(Tcl_Interp*);

// local extension to enable postscript printing for images
extern "C" void TkCanvasPsImage_Init();

// dummy Tcl command implementation
static int tclutil_cmd(ClientData, Tcl_Interp* interp, int argc, char** argv)
{
    return TCL_OK;
}


/*
 * A call to this function is made from the tkAppInit file at startup
 * to initialize this package
 */
extern "C"
int Tclutil_Init(Tcl_Interp* interp)  
{
    char buf[1024];
    static int initialized = 0;
    if (initialized++)
	return TCL_OK;

    // enable postscript printing for images (local ext)
    TkCanvasPsImage_Init();

    // set up Tcl package
    if (Tcl_PkgProvide (interp, "Tclutil", TCLUTIL_VERSION) != TCL_OK) {
	return TCL_ERROR;
    }

    // define bitmaps used by Tcl library
    defineTclutilBitmaps(interp);

    // add a dummy tcl command (this command doesn't do anything currently)
    Tcl_CreateCommand(interp, "tclutil", tclutil_cmd, NULL, NULL);

    // Set the global Tcl variable  tclutil_version 
    Tcl_SetVar(interp, "tclutil_version", TCLUTIL_VERSION, TCL_GLOBAL_ONLY);

    /*
     * Use Tcl script to search for TclutilInit.tcl and run the initialization tcl script
     */

    // defines for TclutilInit.icc:
#   define Pkg_findinit Tclutil_findinit
#   include "TclutilInit.icc"

    // defines for calling the script
#   define Pkg "Tclutil"
#   define Pkg_proc "Tclutil_findinit"
    char* pkg_library = TCLUTIL_LIBRARY;

    Tcl_SetVar(interp, "Pkg_findinit", Pkg_proc, TCL_GLOBAL_ONLY);

    if (Tcl_GlobalEval(interp, Pkg_findinit) == TCL_ERROR)
	return TCL_ERROR;

    sprintf(buf, "%s %s %s", Pkg_proc, Pkg, pkg_library);
    if (Tcl_GlobalEval(interp, buf) == TCL_ERROR)
	return TCL_ERROR;

    return TCL_OK; 
}
