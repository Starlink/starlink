/*
 * E.S.O. - VLT project 
 * "@(#) $Id: Astrotcl.C,v 1.6 1999/03/11 20:58:40 abrighto Exp $"
 *
 * Astrotcl.C - Initialize Astrotcl package
 * 
 * who             when       what
 * --------------  --------   ----------------------------------------
 * Allan Brighton  21 Nov 97  Created
 */
static const char* const rcsId="@(#) $Id: Astrotcl.C,v 1.6 1999/03/11 20:58:40 abrighto Exp $";

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
    Tcl_CreateCommand(interp, "astrotcl", astrotcl_cmd, NULL, NULL);
    
    // add the wcs command
    TclWorldCoords_Init(interp);
    
    // The astrotcl_library path can be found in several places.  Here is the order
    // in which the are searched.
    //		1) the variable may already exist
    //		2) env array
    //		3) the compiled in value of ASTROTCL_LIBRARY
    char* libDir = Tcl_GetVar(interp, "astrotcl_library", TCL_GLOBAL_ONLY);
    if (libDir == NULL) {
	libDir = Tcl_GetVar2(interp, "env", "ASTROTCL_LIBRARY", TCL_GLOBAL_ONLY);
    }
    if (libDir == NULL) {
	libDir = ASTROTCL_LIBRARY;
    }

    // Set the global Tcl variables astrotcl_library and astrotcl_version 
    // and add astrotcl_library to the auto_path.
    Tcl_SetVar(interp, "astrotcl_library", libDir, TCL_GLOBAL_ONLY);
    Tcl_SetVar(interp, "astrotcl_version", ASTROTCL_VERSION, TCL_GLOBAL_ONLY);

    char cmd[1048];
    sprintf(cmd, "lappend auto_path %s", libDir);
    int status = Tcl_Eval(interp, cmd);
    return status; 
}
