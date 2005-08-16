/*
 * E.S.O. - VLT project/ ESO Archive
 * "@(#) $Id$"
 *
 * Gaia.C - Initialize Gaia Tcl package
 *
 * who             when       what
 * --------------  --------   ----------------------------------------
 * Allan Brighton  25 Mar 98  Created
 * Peter W. Draper 22 Feb 99  Added GaiaCat_Init
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
    int Polyline_Init();
    int RotBox_Init();
    int Tcladam_Init(Tcl_Interp *interp);
    int GaiaCat_Init(Tcl_Interp *interp);
}

//  Generated code for bitmaps used in tcl scripts.
void defineGaiaBitmaps(Tcl_Interp*);

//  Generated code for colormaps.
void defineGaiaColormaps();

/*
 * A call to this function is made from the tkAppInit file at startup
 * to initialize this package
 */
extern "C" int Gaia_Init( Tcl_Interp *interp )
{
    //  Set up Tcl package.
    if (Tcl_PkgProvide(interp, "Gaia", GAIA_VERSION ) != TCL_OK) {
	return TCL_ERROR;
    }

    //  Define bitmaps used by Tcl library.
    defineGaiaBitmaps(interp);

    //  Define colormaps added by GAIA.
    defineGaiaColormaps();

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

    /*  Add rtd_polyline for drawing many polyline at speed */
    if (Polyline_Init() != TCL_OK) {
	return TCL_ERROR;
    }

    /* Add Starlink task control commands*/
    if (Tcladam_Init(interp) != TCL_OK) {
	return TCL_ERROR;
    }

    //  Add GaiaCat command.
    if (GaiaCat_Init(interp) != TCL_OK) {
      return TCL_ERROR;
    }

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
	libDir = GAIA_LIBRARY;
    }

    // Set the global Tcl variables gaia_library and gaia_version
    // and add gaia_library to the auto_path (goes first, so used
    // first). Also set global var env(GAIA_VERSION) so this is
    // available to all sub-shells.
    Tcl_SetVar(interp, "gaia_library", libDir, TCL_GLOBAL_ONLY);
    Tcl_SetVar(interp, "gaia_version", GAIA_VERSION, TCL_GLOBAL_ONLY);
    char cmd[1048];
    sprintf(cmd, "set auto_path [linsert $auto_path 0 %s]", libDir );
    if (Tcl_Eval(interp, cmd) != TCL_OK) {
	return TCL_ERROR;
    }
    Tcl_SetVar2(interp, "env", "GAIA_VERSION", GAIA_VERSION, TCL_GLOBAL_ONLY);

    //  Do the Iwidgets initialisation, needed for single binary as
    //  Iwidgets doesn't have a builtin init function (so the script
    //  method of doing the init gets confused).
    libDir = Tcl_GetVar(interp, "iwidgets_library", TCL_GLOBAL_ONLY);
    if (libDir == NULL) {
        libDir = Tcl_GetVar2(interp, "env", "IWIDGETS_LIBRARY",
                             TCL_GLOBAL_ONLY);
    }
    if (libDir == NULL) {
        libDir = IWIDGETS_LIBRARY;
    }
    Tcl_SetVar(interp, "iwidgets_library", libDir, TCL_GLOBAL_ONLY);
    Tcl_SetVar(interp, "iwidgets_version", IWIDGETS_VERSION, TCL_GLOBAL_ONLY);
    sprintf(cmd, "lappend auto_path %s", libDir);
    if (Tcl_Eval(interp, cmd) != TCL_OK)
        return TCL_ERROR;
    
    //  Also do the package provide.
    if ( Tcl_Eval( interp,
                   "global iwidgets_library iwidgets_version\n"
                   "package require Tcl 8.0\n"
                   "package require Tk 8.0\n"
                   "package require Itcl 3.0\n"
                   "package require Itk 3.0\n"
                   "namespace eval ::iwidgets {\n"
                   "  namespace export *\n"
                   "  variable library $iwidgets_library\n"
                   "  variable version $iwidgets_version\n"
                   "}\n"
                   "lappend auto_path \"${iwidgets_library}/scripts\"\n"
                   "package provide Iwidgets $iwidgets_version\n"
        ) != TCL_OK ) {
        return TCL_ERROR;
    }
    
    //  Set up the namespaces used by the itcl/itk classes.
    if (Tcl_Eval(interp,
		 "namespace eval gaia {namespace export *}\n"
		 "namespace import -force gaia::*\n"
//		 "namespace import -force iwidgets::*\n"
        ) != TCL_OK) {
        return TCL_ERROR;
    }
    return TCL_OK;
}

