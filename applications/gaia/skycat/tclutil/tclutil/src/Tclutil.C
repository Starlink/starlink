/*
 * E.S.O. - VLT project 
 * "@(#) $Id: Tclutil.C,v 1.6 1999/02/02 21:51:02 abrighto Exp $"
 *
 * Tclutil.C - Initialize Tclutil package
 * 
 * who             when       what
 * --------------  --------   ----------------------------------------
 * Allan Brighton  21 Nov 97  Created
 */
static const char* const rcsId="@(#) $Id: Tclutil.C,v 1.6 1999/02/02 21:51:02 abrighto Exp $";

#include <string.h>
#include <stdlib.h>
#include <signal.h>
#include <stdio.h>
#include <iostream.h>
#include <strstream.h>
#include <sys/types.h>
#include <new.h>
#include <unistd.h>
#include <math.h>
#include <assert.h>
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
    static initialized = 0;
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
    
    // The tclutil_library path can be found in several places.  Here is the order
    // in which the are searched.
    //		1) the variable may already exist
    //		2) env array
    //		3) the compiled in value of TCLUTIL_LIBRARY
    char* libDir = Tcl_GetVar(interp, "tclutil_library", TCL_GLOBAL_ONLY);
    if (libDir == NULL) {
	libDir = Tcl_GetVar2(interp, "env", "TCLUTIL_LIBRARY", TCL_GLOBAL_ONLY);
    }
    if (libDir == NULL) {
	libDir = TCLUTIL_LIBRARY;
    }

    // Set the global Tcl variables tclutil_library and tclutil_version 
    // and add tclutil_library to the auto_path.
    Tcl_SetVar(interp, "tclutil_library", libDir, TCL_GLOBAL_ONLY);
    Tcl_SetVar(interp, "tclutil_version", TCLUTIL_VERSION, TCL_GLOBAL_ONLY);

    char cmd[1048];
    // sprintf(cmd, "lappend auto_path %s", libDir);
    sprintf(cmd, "set auto_path [linsert $auto_path 0 %s]", libDir);
    if (Tcl_Eval(interp, cmd) != TCL_OK)
	return TCL_ERROR;

    // set up the namespaces used by the itcl/itk classes
    // (for convenience, import the Itcl and BLT namespaces also)
    if (Tcl_Eval(interp, 
#if (TCL_MAJOR_VERSION >= 8)
		 "namespace import -force blt::*;"
		 "namespace import -force itcl::*;"
		 "namespace import -force itk::*;"
		 "namespace eval util {namespace export *};"
		 "namespace import -force util::*;"
#else
		 "import add blt;"
		 "import add itcl;"
		 "import add itk;"
		 "namespace ::util {};"
		 "import add util;"
#endif
	) != TCL_OK)
	return TCL_ERROR;

    return TCL_OK; 
}
