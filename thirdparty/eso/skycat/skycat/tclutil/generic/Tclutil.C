/*
 * E.S.O. - VLT project 
 * "@(#) $Id: Tclutil.C,v 1.5 2006/01/25 22:21:38 abrighto Exp $"
 *
 * Tclutil.C - Initialize Tclutil package
 * 
 * who             when       what
 * --------------  --------   ----------------------------------------
 * Allan Brighton  21 Nov 97  Created
 * pbiereic        26/08/99   Changed Tclutil_Init()
 * pbiereic        17/02/03   Added 'using namespace std'.
 * Allan Brighton  28/12/05   Replaced init script
 */
static const char* const rcsId="@(#) $Id: Tclutil.C,v 1.5 2006/01/25 22:21:38 abrighto Exp $";

using namespace std;
#include <cstdlib>
#include <csignal>
#include <cstdio>
#include <iostream>
#include <sys/types.h>
#include <unistd.h>
#include <cmath>
#include <cassert>
#include <cstring>
#ifdef HAVE_CONFIG_H
#include "config.h"
#endif
#include "define.h"
#include "tcl.h"
#include "tk.h"
#include "error.h"

// Since we have to link the BLT library anyway (because we're using the BLT C interface) 
// call the init routine directly
extern "C" int Blt_Init(Tcl_Interp *interp);

// generated code for bitmaps used in tcl scripts
void defineTclutilBitmaps(Tcl_Interp*);

// Tcl procedure to search for an init for Tclutil startup file.  
static char initScript[] = "if {[info proc ::util::Init]==\"\"} {\n\
  namespace eval ::util {}\n\
  proc ::util::Init {} {\n"
#ifdef MAC_TCL
"    source -rsrc TclutilInit.tcl\n"
#else
"    global tclutil_library\n\
    tcl_findLibrary tclutil " PACKAGE_VERSION " " PACKAGE_VERSION " TclutilInit.tcl TCLUTIL_LIBRARY tclutil_library\n"
#endif
"  }\n\
}\n\
::util::Init";


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

    // initialize the required BLT package 
    if (Blt_Init(interp) == TCL_ERROR) {
	return TCL_ERROR; 
    }

    // set up Tcl package
    if (Tcl_PkgProvide(interp, "Tclutil", PACKAGE_VERSION) != TCL_OK) {
	return TCL_ERROR;
    }

    // define bitmaps used by Tcl library
    defineTclutilBitmaps(interp);

    // add a dummy tcl command (this command doesn't do anything currently)
    Tcl_CreateCommand(interp, "tclutil", (Tcl_CmdProc*)tclutil_cmd, NULL, NULL);

    // Set the global Tcl variable  tclutil_version 
    Tcl_SetVar(interp, "tclutil_version", PACKAGE_VERSION, TCL_GLOBAL_ONLY);

    // Hack to work around problem in older tcl-8.4.x versions: see ./tcl_findLibrary.h
#if ((TCL_MAJOR_VERSION == 8 && TCL_MINOR_VERSION == 4 && TCL_RELEASE_SERIAL < 11) || TCL_MAJOR_VERSION < 8 || TCL_MINOR_VERSION < 4)
#include "tcl_findLibrary.h"
    if (Tcl_Eval(interp, tcl_findLibrary) != TCL_OK) return TCL_ERROR;
#endif

    return Tcl_Eval(interp, initScript);
}
