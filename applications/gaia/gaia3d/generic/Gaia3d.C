/*+
 *   Name:
 *      Gaia3d

 *   Purpose:
 *      GAIA3D initialisation functions.

 *   Language:
 *      C++

 *  Copyright:
 *     Copyright (C) 2007 Science and Technology Facilities Council
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
 *      14-JUN-2007 (PWD):
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

/* Tcl procedure to search for an init for GAIA3D startup file. */
static char initScript[] = \
"if {[info proc ::gaia3d::Init]==\"\"} {\n\
    namespace eval ::gaia3d {}\n\
    proc ::gaia3d::Init {} {\n"
#ifdef MAC_TCL
"      source -rsrc Gaia3dInit.tcl\n"
#else
"      global gaia_library\n\
       tcl_findLibrary gaia3d " PACKAGE_VERSION " " PACKAGE_VERSION " \
                            Gaia3dInit.tcl GAIA3D_LIBRARY gaia3d_library\n"
#endif
"  }\n\
}\n\
::gaia3d::Init";


/* Prototypes */

int Gaia3dVtk_Init( Tcl_Interp *interp );

/*
 * A call to this function is made from the tkAppInit file at startup.
 */
extern "C" int Gaia3d_Init( Tcl_Interp *interp )
{
    /*  Set up the Gaia3d Tcl package. */
    if ( Tcl_PkgProvide( interp, "Gaia3d", PACKAGE_VERSION ) != TCL_OK) {
        return TCL_ERROR;
    }
    Tcl_SetVar( interp, "gaia3d_version", PACKAGE_VERSION, TCL_GLOBAL_ONLY );

    //  Run the initialisation command.
    if ( Tcl_Eval( interp, initScript ) != TCL_OK ) {
        return TCL_ERROR;
    }

    /*  VTK interop interface. */
    if ( Gaia3dVtk_Init( interp ) != TCL_OK ) {
        return TCL_ERROR;
    }

    return TCL_OK;
}
