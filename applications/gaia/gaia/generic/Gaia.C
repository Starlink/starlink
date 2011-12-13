/*
 * E.S.O. - VLT project/ ESO Archive
 * "@(#) $Id$"
 *
 * Gaia.C - Initialize Gaia Tcl package
 *
 *  Copyright:
 *     Copyright (C) 1999-2005 Central Laboratory of the Research Councils.
 *     Copyright (C) 2006 Particle Physics & Astronomy Research Council.
 *     All Rights Reserved.
 *
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
 *
 * who             when       what
 * --------------  --------   ----------------------------------------
 * Allan Brighton  25 Mar 98  Created
 * Peter W. Draper 22 Feb 99  Added GaiaCat_Init
 */
static const char* const rcsId="@(#) $Id$";

#if HAVE_CONFIG_H
#include <config.h>
#endif

#include <cstring>
#include <cstdlib>
#include <csignal>
#include <iostream>
#include <tcl.h>

/* Tcl procedure to search for an init for GAIA startup file. */
static char initScript[] = \
"if {[info proc ::gaia::Init]==\"\"} {\n\
    namespace eval ::gaia {}\n\
    proc ::gaia::Init {} {\n"
#ifdef MAC_TCL
"      source -rsrc GaiaInit.tcl\n"
#else
"      global gaia_library\n\
       tcl_findLibrary gaia " PACKAGE_VERSION " " PACKAGE_VERSION " \
                            GaiaInit.tcl GAIA_LIBRARY gaia_library\n"
#endif
"  }\n\
}\n\
::gaia::Init";


/* These have C++ linkage */
int GaiaUtils_Init( Tcl_Interp *interp );
int Fits_Init( Tcl_Interp *interp );

/* And these C linkage */
extern "C" {
#include <ast.h>
    int StarRtd_Init( Tcl_Interp *interp );

    int Array_Init( Tcl_Interp *interp );
    int Ellipse_Init();
    int GaiaCat_Init( Tcl_Interp *interp );
    int Hds_Init( Tcl_Interp *interp );
    int Mark_Init();
    int Ndf_Init( Tcl_Interp *interp );
    int Polyline_Init();
    int RotBox_Init();
    int Segment_Init();
    int Sla_Init( Tcl_Interp *interp );
    int SpectralPlot_Init();
    int Tcladam_Init( Tcl_Interp *interp );
    int Word_Init();

    int Tkhtml_Init( Tcl_Interp *interp );
}

//  Generated code for bitmaps used in tcl scripts.
void defineGaiaBitmaps( Tcl_Interp *interp );

//  Generated code for colormaps.
void defineGaiaColormaps();

/*
 * A call to this function is made from the tkAppInit file at startup.
 */
extern "C" int Gaia_Init( Tcl_Interp *interp )
{
    /*  Set up the Gaia Tcl package. */
    if ( Tcl_PkgProvide( interp, "Gaia", PACKAGE_VERSION ) != TCL_OK) {
        return TCL_ERROR;
    }
    Tcl_SetVar( interp, "gaia_version", PACKAGE_VERSION, TCL_GLOBAL_ONLY );

    //  Run the initialisation command.
    if ( Tcl_Eval( interp, initScript ) != TCL_OK ) {
        return TCL_ERROR;
    }

    /*  Define bitmaps used by Tcl library. */
    defineGaiaBitmaps( interp );

    /*  Define colormaps added by GAIA. */
    defineGaiaColormaps();

    /* Initialize the new rtd_image type */
    if ( StarRtd_Init( interp ) != TCL_OK)
        return TCL_ERROR;

    /* Add rtd_ellipse and rtd_rotbox items to canvases */
    if ( Ellipse_Init() != TCL_OK ) {
        return TCL_ERROR;
    }
    if ( RotBox_Init() != TCL_OK ) {
        return TCL_ERROR;
    }

    /* Add rtd_mark and rtd_word items to canvases */
    if ( Word_Init() != TCL_OK ) {
        return TCL_ERROR;
    }
    if ( Mark_Init() != TCL_OK ) {
        return TCL_ERROR;
    }

    /*  Add rtd_segment canvas item for drawing many lines as segments */
    if ( Segment_Init() != TCL_OK ) {
        return TCL_ERROR;
    }

    /*  Add rtd_polyline canvas item for drawing many polylines at speed */
    if ( Polyline_Init() != TCL_OK ) {
        return TCL_ERROR;
    }

    /* Add spectral_plot canavs item for interactive spectral drawing */
    if ( SpectralPlot_Init() != TCL_OK ) {
        return TCL_ERROR;
    }

    /* Add Starlink task control commands*/
    if ( Tcladam_Init( interp ) != TCL_OK ) {
        return TCL_ERROR;
    }

    //  Add GaiaCat command.
    if ( GaiaCat_Init( interp ) != TCL_OK ) {
        return TCL_ERROR;
    }

    //  Simple NDF interface.
    if ( Ndf_Init( interp ) != TCL_OK ) {
        return TCL_ERROR;
    }

    //  Simple FITS interface.
    if ( Fits_Init( interp ) != TCL_OK ) {
        return TCL_ERROR;
    }

    //  HDS commands.
    if ( Hds_Init( interp ) != TCL_OK ) {
        return TCL_ERROR;
    }

    //  Array handling interface.
    if ( Array_Init( interp ) != TCL_OK ) {
        return TCL_ERROR;
    }

    //  Simple SLALIB-like commands.
    if ( Sla_Init( interp ) != TCL_OK ) {
        return TCL_ERROR;
    }

    //  Local utility commands.
    if ( GaiaUtils_Init( interp ) ) {
        return TCL_ERROR;
    }

    //  HTML viewer widget.

    if (Tkhtml_Init(interp) == TCL_ERROR ) {
        return TCL_ERROR;
    }

    //  AST tuning. MemoryCaching give 3-4% speed up, ObjectCaching
    //  is not noticable, so consensus is leave off.
    //astTune( "ObjectCaching", 1 );
    astTune( "MemoryCaching", 1 );

    return TCL_OK;
}
