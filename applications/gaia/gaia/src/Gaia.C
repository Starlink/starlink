/*
 * E.S.O. - VLT project/ ESO Archive 
 * "@(#) $Id$"
 *
 * Gaia.C - Initialize Gaia Tcl package
 * 
 * who             when       what
 * --------------  --------   ----------------------------------------
 * Allan Brighton  25 Mar 98  Created
 */
static const char* const rcsId="@(#) $Id$";

#include <string.h>
#include <stdlib.h>
#include <signal.h>
#include <iostream.h>
#include "tcl.h"

extern "C" {
    int StarRtd_Init(Tcl_Interp *interp);
    int Ellipse_Init();
    int Word_Init();
    int Mark_Init();
    int Segment_Init();
    int RotBox_Init();
    int Tcladam_Init(Tcl_Interp *interp);
#ifdef __GNUC__
    void f_init();
    int main(int argc, char** argv);
    int MAIN__(int argc, char** argv) {return main(argc,argv);}
#endif
}

// generated code for bitmaps used in tcl scripts
void defineGaiaBitmaps(Tcl_Interp*);

#ifdef __GNUC__
int xargc = 1;
char* xargv[] = {"gaia", NULL};
#endif

/*
 * A call to this function is made from the tkAppInit file at startup
 * to initialize this package
 */
extern "C" int Gaia_Init( Tcl_Interp *interp )
{
#ifdef __GNUC__
    f_init();
#endif

    // set up Tcl package
    if (Tcl_PkgProvide(interp, "Gaia", "2.3") != TCL_OK) { //XXX GAIA_VERSION
	return TCL_ERROR;
    }

    // define bitmaps used by Tcl library
    defineGaiaBitmaps(interp);

    // initialize the new image type and Tcl commands
    if (StarRtd_Init(interp) != TCL_OK)
	return TCL_ERROR;

    /* Add rtd_ellipse item to canvases */
    if (Ellipse_Init() != TCL_OK) {
	return TCL_ERROR;
    }
    if (RotBox_Init() != TCL_OK) { 
	return TCL_ERROR;
    }

    /* Add rtd_mark and rtd_word items to canvases */
    if (Word_Init() != TCL_OK) {
	return TCL_ERROR;
    }
    if (Mark_Init() != TCL_OK) {
	return TCL_ERROR;
    }

    /*  Add rtd_segment for drawing many lines as segments */
    if (Segment_Init() != TCL_OK) {
	return TCL_ERROR;
    }
        
    /* Add Starlink task control commands*/
    if (Tcladam_Init(interp) != TCL_OK) {
	return TCL_ERROR;
    }

    // create the namespaces used by the itcl/itk classes
    // (This is just a convenience so you don't have to always call some init proc)
    if (Tcl_Eval(interp, "namespace ::gaia {}; import add gaia") != 0)
	return TCL_ERROR;

    // The gaia_library path can be found in several places.  Here is the order
    // in which the are searched.
    //		1) the variable may already exist
    //		2) env array
    //		3) the compiled in value of GAIA_LIBRARY
    char* libDir = Tcl_GetVar(interp, "gaia_library", TCL_GLOBAL_ONLY);
    if (libDir == NULL) {
	libDir = Tcl_GetVar2(interp, "env", "GAIA_LIBRARY", TCL_GLOBAL_ONLY);
    }
    if (libDir == NULL) {
	libDir = "/star/bin/gaia"; //XXX GAIA_LIBRARY
    }

    // Set the global Tcl variables gaia_library and gaia_version 
    // and add gaia_library to the auto_path.
    Tcl_SetVar(interp, "gaia_library", libDir, TCL_GLOBAL_ONLY);
    Tcl_SetVar(interp, "gaia_version", "2.3", TCL_GLOBAL_ONLY); //XXX GAIA_VERSION

    char cmd[1048];
    sprintf(cmd, "lappend auto_path %s", libDir);
    int status = Tcl_Eval(interp, cmd);
    return status; 
}

