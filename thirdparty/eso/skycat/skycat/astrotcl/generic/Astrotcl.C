/*
 * E.S.O. - VLT project 
 * "@(#) $Id: Astrotcl.C,v 1.1.1.1 2006/01/12 16:44:01 abrighto Exp $"
 *
 * Astrotcl.C - Initialize Astrotcl package
 * 
 * who             when       what
 * --------------  --------   ----------------------------------------
 * Allan Brighton  21 Nov 97  Created
 * pbiereic        26/08/99   Changed Astrotcl_Init()
 */
static const char* const rcsId="@(#) $Id: Astrotcl.C,v 1.1.1.1 2006/01/12 16:44:01 abrighto Exp $";

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
#ifdef HAVE_CONFIG_H
#include "config.h"
#endif
#include "define.h"

extern "C" int TclWorldCoords_Init(Tcl_Interp* interp);

// Tcl procedure to search for an init for Astrotcl startup file.  
static char initScript[] = "if {[info proc ::util::Init]==\"\"} {\n\
  namespace eval ::util {}\n\
  proc ::util::Init {} {\n"
#ifdef MAC_TCL
"    source -rsrc AstrotclInit.tcl\n"
#else
"    global astrotcl_library\n\
    tcl_findLibrary astrotcl " PACKAGE_VERSION " " PACKAGE_VERSION " AstrotclInit.tcl ASTROTCL_LIBRARY astrotcl_library\n"
#endif
"  }\n\
}\n\
::util::Init";

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
    if (Tcl_PkgProvide (interp, "Astrotcl", PACKAGE_VERSION) != TCL_OK) {
	return TCL_ERROR;
    }

    // add a dummy tcl command (this command doesn't do anything currently)
    Tcl_CreateCommand(interp, "astrotcl", (Tcl_CmdProc*)astrotcl_cmd, NULL, NULL);
    
    // add the wcs command
    TclWorldCoords_Init(interp);

    // Set the global Tcl variable astrotcl_version 
    Tcl_SetVar(interp, "astrotcl_version", PACKAGE_VERSION, TCL_GLOBAL_ONLY);

    return Tcl_Eval(interp, initScript);
}
