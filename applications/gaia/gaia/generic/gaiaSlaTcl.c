/*+
 *   Name:
 *      gaiaSlaTcl

 *   Purpose:
 *      SLALIB-like utility routines.

 *   Language:
 *      C

 *  Copyright:
 *     Copyright (C) 2006 Particle Physics & Astronomy Research Council.
 *     Copyright (C) 2008 Science and Technology Facilities Council.
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
 *      30-MAR-2006 (PWD):
 *         Original version. Extracted from StarRtdImage.
 *      {enter_changes_here}
 *-
 */

#if HAVE_CONFIG_H
#include "config.h"
#endif

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include <tcl.h>
#include <fitshead.h>
#include <fitsfile.h>
#include "GaiaArray.h"
#include "star/slalib.h"

/* Local prototypes */
static int gaiaSlaObs( ClientData clientData, Tcl_Interp *interp,
                       int objc, Tcl_Obj *CONST objv[] );

static int gaiaSlaDat( ClientData clientData, Tcl_Interp *interp,
                       int objc, Tcl_Obj *CONST objv[] );

static int gaiaSlaDateObs2Je( ClientData clientData, Tcl_Interp *interp,
                              int objc, Tcl_Obj *CONST objv[] );

static int gaiaSlaDateJe2Obs( ClientData clientData, Tcl_Interp *interp,
                              int objc, Tcl_Obj *CONST objv[] );

static int gaiaSlaDateMjd2Obs( ClientData clientData, Tcl_Interp *interp,
                               int objc, Tcl_Obj *CONST objv[] );


/**
 * Register all the sla:: commands.
 */
int Sla_Init( Tcl_Interp *interp )
{
    Tcl_CreateObjCommand( interp, "sla::dat", gaiaSlaDat,
                          (ClientData) NULL,
                          (Tcl_CmdDeleteProc *) NULL );

    Tcl_CreateObjCommand( interp, "sla::dateobs2je", gaiaSlaDateObs2Je,
                          (ClientData) NULL,
                          (Tcl_CmdDeleteProc *) NULL );

    Tcl_CreateObjCommand( interp, "sla::dateje2obs", gaiaSlaDateJe2Obs,
                          (ClientData) NULL,
                          (Tcl_CmdDeleteProc *) NULL );

    Tcl_CreateObjCommand( interp, "sla::datemjd2obs", gaiaSlaDateMjd2Obs,
                          (ClientData) NULL,
                          (Tcl_CmdDeleteProc *) NULL );

    Tcl_CreateObjCommand( interp, "sla::obs", gaiaSlaObs,
                          (ClientData) NULL,
                          (Tcl_CmdDeleteProc *) NULL );

    return TCL_OK;
}

/**
 * Get details about observatories.
 *
 * There are two arguments, the observation index and the observation
 * station identifier. If the observation index is set to -1 then the
 * observation station identifier value is used, otherwise it is ignored.
 * See the SLA_OBS description in SUN/67.
 *
 * The results are a list of values, the observation station identifier,
 * name, longtitude, latitude and height.
 */
static int gaiaSlaObs( ClientData clientData, Tcl_Interp *interp,
                       int objc, Tcl_Obj *CONST objv[] )
{
    Tcl_Obj *resultObj;
    char c[11];
    char name[41];
    double h;
    double p;
    double w;
    int n = 0;

    /* Check arguments, only one or two */
    if ( objc != 2 && objc != 3 ) {
        Tcl_WrongNumArgs( interp, 1, objv, "station_index [station_id]" );
        return TCL_ERROR;
    }

    if ( Tcl_GetIntFromObj( interp, objv[1], &n ) != TCL_OK ) {
        return TCL_ERROR;
    }
    if ( objc >= 3 ) {
        strncpy( c, Tcl_GetString( objv[2] ), 11 );
    }
    else {
        c[0] = '\0';
    }

    /* The call */
    slaObs( n, c, name, &w, &p, &h );

    /* Construct a list return of all the parameters. */
    resultObj = Tcl_GetObjResult( interp );
    Tcl_ListObjAppendElement( interp, resultObj, Tcl_NewStringObj( c, -1 ) );
    Tcl_ListObjAppendElement( interp, resultObj, Tcl_NewStringObj( name, -1 ));
    Tcl_ListObjAppendElement( interp, resultObj, Tcl_NewDoubleObj( w ) );
    Tcl_ListObjAppendElement( interp, resultObj, Tcl_NewDoubleObj( p ) );
    Tcl_ListObjAppendElement( interp, resultObj, Tcl_NewDoubleObj( h ) );

    return TCL_OK;
}



/**
 * Convert a DATE-OBS FITS date string to a Julian Epoch. Two arguments are
 * needed, the name of the keyword (so that old-style DATE is allowed) and
 * the complete FITS card.
 *
 * Note: not SLALIB really, just a wcslib function.
 *
 * The result is the julian epoch.
 */
static int gaiaSlaDateObs2Je( ClientData clientData, Tcl_Interp *interp,
                              int objc, Tcl_Obj *CONST objv[] )
{
    Tcl_Obj *resultObj;
    double value;
    int result;

    /* Check arguments, require two */
    if ( objc != 3 ) {
        Tcl_WrongNumArgs( interp, 1, objv, "FITS-keyword FITS-card" );
        return TCL_ERROR;
    }
    result = hgetdate( Tcl_GetString( objv[2] ), Tcl_GetString( objv[1] ),
                       &value );
    if ( result == 0 ) {
        resultObj = Tcl_GetObjResult( interp );
        Tcl_SetStringObj( resultObj, Tcl_GetString( objv[2] ), -1 );
        Tcl_AppendStringsToObj( resultObj, " : not a valid date",
                                (char *) NULL );
        return TCL_ERROR;
    }
    Tcl_SetObjResult( interp, Tcl_NewDoubleObj( value ) );
    return TCL_OK;
}


/**
 * Convert a Julian epoch into a DATE-OBS FITS string.
 * One argument is needed, the Julian epoch.
 *
 * The result is the DATE-OBS value.
 */
static int gaiaSlaDateJe2Obs( ClientData clientData, Tcl_Interp *interp,
                              int objc, Tcl_Obj *CONST objv[] )
{
    char date[256];
    char sign[2];
    double fd;
    double je;
    double mjd;
    int ihmsf[4];
    int iymdf[4];
    int jj;

    /* Check arguments, require one */
    if ( objc != 2 ) {
        Tcl_WrongNumArgs( interp, 1, objv, "Julian_Epoch" );
        return TCL_ERROR;
    }

    if ( Tcl_GetDoubleFromObj( interp, objv[1], &je ) != TCL_OK ) {
        return TCL_ERROR;
    }

    /* Use "ccyy-mm-ddThh:mm:ss[.ssss]" format. */
    mjd = slaEpj2d( je );

    slaDjcl( mjd, iymdf, iymdf+1, iymdf+2, &fd, &jj );
    slaDd2tf( 3, fd, sign, ihmsf );

    sprintf( date, "%4.4d-%2.2d-%2.2dT%2.2d:%2.2d:%2.2d.%3.3d",
             iymdf[0], iymdf[1], iymdf[2], ihmsf[0], ihmsf[1],
             ihmsf[2], ihmsf[3] );

    Tcl_SetResult( interp, date, TCL_VOLATILE );
    return TCL_OK;
}

/**
 * Convert a modified Julian date into a DATE-OBS FITS string.
 * One argument is needed, the MJD.
 *
 * The result is the DATE-OBS value.
 */
static int gaiaSlaDateMjd2Obs( ClientData clientData, Tcl_Interp *interp,
                              int objc, Tcl_Obj *CONST objv[] )
{
    char date[256];
    char sign[2];
    double fd;
    double mjd;
    int ihmsf[4];
    int iymdf[4];
    int jj;

    /* Check arguments, require one */
    if ( objc != 2 ) {
        Tcl_WrongNumArgs( interp, 1, objv, "Modified_Julian_Date" );
        return TCL_ERROR;
    }

    if ( Tcl_GetDoubleFromObj( interp, objv[1], &mjd ) != TCL_OK ) {
        return TCL_ERROR;
    }

    /* Use "ccyy-mm-ddThh:mm:ss[.ssss]" format. */
    slaDjcl( mjd, iymdf, iymdf+1, iymdf+2, &fd, &jj );
    slaDd2tf( 3, fd, sign, ihmsf );
    sprintf( date, "%4.4d-%2.2d-%2.2dT%2.2d:%2.2d:%2.2d.%3.3d",
             iymdf[0], iymdf[1], iymdf[2], ihmsf[0], ihmsf[1],
             ihmsf[2], ihmsf[3] );

    Tcl_SetResult( interp, date, TCL_VOLATILE );
    return TCL_OK;
}

/**
 * Given an UTC as an MJD return the correction to TAI.
 */
static int gaiaSlaDat( ClientData clientData, Tcl_Interp *interp,
                       int objc, Tcl_Obj *CONST objv[] )
{
    double mjd;
    double delta;

    /* Check arguments, require one */
    if ( objc != 2 ) {
        Tcl_WrongNumArgs( interp, 1, objv, "Modified_Julian_Date" );
        return TCL_ERROR;
    }

    if ( Tcl_GetDoubleFromObj( interp, objv[1], &mjd ) != TCL_OK ) {
        return TCL_ERROR;
    }

    delta = slaDat( mjd );
    Tcl_SetObjResult( interp, Tcl_NewDoubleObj( delta ) );
    return TCL_OK;
}

