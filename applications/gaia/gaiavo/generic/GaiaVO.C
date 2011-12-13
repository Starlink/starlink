/*+
 *   Name:
 *      GaiaVO

 *   Purpose:
 *      GAIAVO initialisation functions.

 *   Language:
 *      C++

 *  Copyright:
 *     Copyright (C) 2008 Science and Technology Facilities Council
 *     All Rights Reserved.

 *  Licence:
 *     This program is free software; you can redistribute it and/or
 *     modify it under the terms of the GNU General Public License as
 *     published by the Free Software Foundation; either version 2 of the
 *     License, or (at your option) any later version.
 *
 *     This program is distributed in the hope that it will be
 *     useful, but WITHOUT ANY WARRANTY; without even the implied warranty
 *     of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 *     GNU General Public License for more details.
 *
 *     You should have received a copy of the GNU General Public License
 *     along with this program; if not, write to the Free Software
 *     Foundation, Inc., 51 Franklin Street,Fifth Floor, Boston, MA
 *     02110-1301, USA

 *   Authors:
 *      PWD: Peter W. Draper, Starlink - University of Durham

 *   History:
 *      18-JUL-2008 (PWD):
 *         Original version.
 *      {enter_changes_here}
 *-
 */

#if HAVE_CONFIG_H
#include <config.h>
#endif

#include <cstring>
#include <cstdlib>
#include <csignal>
#include <iostream>
#include <tcl.h>

/* Tcl procedure to search for a GAIAVO startup file. */
static char initScript[] = \
"if {[info proc ::gaiavo::Init]==\"\"} {\n\
    namespace eval ::gaiavo {}\n\
    proc ::gaiavo::Init {} {\n"
#ifdef MAC_TCL
"      source -rsrc GaiaVOInit.tcl\n"
#else
"      global gaia_library\n\
       tcl_findLibrary gaiavo " PACKAGE_VERSION " " PACKAGE_VERSION " \
                            GaiaVOInit.tcl GAIAVO_LIBRARY gaiavo_library\n"
#endif
"  }\n\
}\n\
::gaiavo::Init";

/*  Declarations. Need C linkage so this can be loaded by Tcl. */
extern "C" {
    int Gaiavo_Init( Tcl_Interp *interp );
}
int GaiaVOTable_Init( Tcl_Interp *interp );

/*
 * A call to this function is made from the tkAppInit file at startup.
 */
int Gaiavo_Init( Tcl_Interp *interp )
{
    /*  Set up the GaiaVO Tcl package. */
    if ( Tcl_PkgProvide( interp, "GaiaVO", PACKAGE_VERSION ) != TCL_OK) {
        return TCL_ERROR;
    }
    Tcl_SetVar( interp, "gaiavo_version", PACKAGE_VERSION, TCL_GLOBAL_ONLY );

    /*  Run the initialisation command. */
    if ( Tcl_Eval( interp, initScript ) != TCL_OK ) {
        return TCL_ERROR;
    }

    if ( GaiaVOTable_Init( interp ) != TCL_OK ) {
        return TCL_ERROR;
    }

    return TCL_OK;
}
