/*+
 *   Name:
 *      GaiaUtilsTcl

 *   Purpose:
 *      Utility routines from Tcl scripts.

 *   Language:
 *      C++

 *  Copyright:
 *     Copyright (C) 2006 Particle Physics & Astronomy Research Council.
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
 *     Foundation, Inc., 59 Temple Place,Suite 330, Boston, MA
 *     02111-1307, USA

 *  Notes:
 *      C++ as it makes the Skycat class available, does not define a class.

 *   Authors:
 *      PWD: Peter W. Draper, Starlink - University of Durham

 *   History:
 *      31-MAR-2006 (PWD):
 *         Original version.
 *      {enter_changes_here}
 *-
 */

#include <float.h>
#include <tcl.h>
#include <HTTP.h>
extern "C" {
#include <ast.h>
}
#include "gaiaUtils.h"

/* Local prototypes */
static int GaiaUtilsAstCopy( ClientData clientData, Tcl_Interp *interp,
                            int objc, Tcl_Obj *CONST objv[] );
static int GaiaUtilsAstGet( ClientData clientData, Tcl_Interp *interp,
                            int objc, Tcl_Obj *CONST objv[] );
static int GaiaUtilsAstSet( ClientData clientData, Tcl_Interp *interp,
                            int objc, Tcl_Obj *CONST objv[] );
static int GaiaUtilsAstShow( ClientData clientData, Tcl_Interp *interp,
                             int objc, Tcl_Obj *CONST objv[] );
static int GaiaUtilsAstTest( ClientData clientData, Tcl_Interp *interp,
                             int objc, Tcl_Obj *CONST objv[] );
static int GaiaUtilsFrameIsA( ClientData clientData, Tcl_Interp *interp,
                              int objc, Tcl_Obj *CONST objv[] );
static int GaiaUtilsGt2DWcs( ClientData clientData, Tcl_Interp *interp,
                             int objc, Tcl_Obj *CONST objv[] );
static int GaiaUtilsGtAxisCoord( ClientData clientData, Tcl_Interp *interp,
                                 int objc, Tcl_Obj *CONST objv[] );
static int GaiaUtilsGtAxisWcs( ClientData clientData, Tcl_Interp *interp,
                               int objc, Tcl_Obj *CONST objv[] );
static int GaiaUtilsGtROIPlots( ClientData clientData, Tcl_Interp *interp,
                                int objc, Tcl_Obj *CONST objv[] );
static int GaiaUtilsUrlGet( ClientData clientData, Tcl_Interp *interp,
                            int objc, Tcl_Obj *CONST objv[] );

/**
 * Register all the gaiautils:: commands.
 */
int GaiaUtils_Init( Tcl_Interp *interp )
{
    Tcl_CreateObjCommand( interp, "gaiautils::getroiplots",
                          GaiaUtilsGtROIPlots,
                          (ClientData) NULL,
                          (Tcl_CmdDeleteProc *) NULL );

    Tcl_CreateObjCommand( interp, "gaiautils::astcopy", GaiaUtilsAstCopy,
                          (ClientData) NULL,
                          (Tcl_CmdDeleteProc *) NULL );

    Tcl_CreateObjCommand( interp, "gaiautils::astget", GaiaUtilsAstGet,
                          (ClientData) NULL,
                          (Tcl_CmdDeleteProc *) NULL );

    Tcl_CreateObjCommand( interp, "gaiautils::astset", GaiaUtilsAstSet,
                          (ClientData) NULL,
                          (Tcl_CmdDeleteProc *) NULL );

    Tcl_CreateObjCommand( interp, "gaiautils::astshow", GaiaUtilsAstShow,
                          (ClientData) NULL,
                          (Tcl_CmdDeleteProc *) NULL );

    Tcl_CreateObjCommand( interp, "gaiautils::asttest", GaiaUtilsAstTest,
                          (ClientData) NULL,
                          (Tcl_CmdDeleteProc *) NULL );

    Tcl_CreateObjCommand( interp, "gaiautils::frameisa", GaiaUtilsFrameIsA,
                          (ClientData) NULL,
                          (Tcl_CmdDeleteProc *) NULL );

    Tcl_CreateObjCommand( interp, "gaiautils::get2dwcs", GaiaUtilsGt2DWcs,
                          (ClientData) NULL,
                          (Tcl_CmdDeleteProc *) NULL );

    Tcl_CreateObjCommand( interp, "gaiautils::getaxiscoord",
                          GaiaUtilsGtAxisCoord, (ClientData) NULL,
                          (Tcl_CmdDeleteProc *) NULL );

    Tcl_CreateObjCommand( interp, "gaiautils::getaxiswcs", GaiaUtilsGtAxisWcs,
                          (ClientData) NULL,
                          (Tcl_CmdDeleteProc *) NULL );

    Tcl_CreateObjCommand( interp, "gaiautils::urlget", GaiaUtilsUrlGet,
                          (ClientData) NULL,
                          (Tcl_CmdDeleteProc *) NULL );

    return TCL_OK;
}

/**
 * Create a deep copy of an AST object.
 *
 * There is one argument, the address of the AST object. The result
 * is the address of the new object.
 */
static int GaiaUtilsAstCopy( ClientData clientData, Tcl_Interp *interp,
                             int objc, Tcl_Obj *CONST objv[] )
{
    AstObject *newObject;
    AstObject *oldObject;
    long adr;

    /* Check arguments, only allow one. */
    if ( objc != 2 ) {
        Tcl_WrongNumArgs( interp, 1, objv, "object" );
        return TCL_ERROR;
    }

    /* Get the object */
    if ( Tcl_GetLongFromObj( interp, objv[1], &adr ) != TCL_OK ) {
        return TCL_ERROR;
    }
    oldObject = (AstObject *) adr;

    /* Export the new object as a long containing the address */
    newObject = (AstObject *) astCopy( oldObject );
    if ( astOK ) {
        Tcl_SetObjResult( interp, Tcl_NewLongObj( (long) newObject ) );
        return TCL_OK;
    }
    astClearStatus;
    Tcl_SetResult( interp, "Failed to copy an AST object", TCL_VOLATILE );
    return TCL_ERROR;
}


/**
 * Get the value of an AST attribute from a FrameSet.
 *
 * There are two arguments, the address of the AST FrameSet and the attribute.
 */
static int GaiaUtilsAstGet( ClientData clientData, Tcl_Interp *interp,
                            int objc, Tcl_Obj *CONST objv[] )
{
    AstFrameSet *wcs;
    long adr;
    const char *value;

    /* Check arguments, only allow two. */
    if ( objc != 3 ) {
        Tcl_WrongNumArgs( interp, 1, objv, "frameset attribute" );
        return TCL_ERROR;
    }

    /* Get the frameset */
    if ( Tcl_GetLongFromObj( interp, objv[1], &adr ) != TCL_OK ) {
        return TCL_ERROR;
    }
    wcs = (AstFrameSet *) adr;

    /* Get the value */
    value = astGetC( wcs, Tcl_GetString( objv[2] ) );
    if ( ! astOK ) {
        char *buf = ckalloc( 1024 );
        sprintf( buf, "Failed to get AST attribute (%s)",
                 Tcl_GetString( objv[2] ) );
        astClearStatus;
        Tcl_SetResult( interp, buf, TCL_DYNAMIC );
        return TCL_ERROR;
    }
    Tcl_SetResult( interp, (char *) value, TCL_VOLATILE );
    return TCL_OK;
}

/**
 * Apply AST attributes to a FrameSet.
 *
 * There are two arguments, the address of the AST FrameSet and the attributes
 * string ("option=value,option=value" etc.).
 */
static int GaiaUtilsAstSet( ClientData clientData, Tcl_Interp *interp,
                            int objc, Tcl_Obj *CONST objv[] )
{
    AstFrameSet *wcs;
    long adr;

    /* Check arguments, only allow two. */
    if ( objc != 3 ) {
        Tcl_WrongNumArgs( interp, 1, objv, "frameset attributes" );
        return TCL_ERROR;
    }

    /* Get the frameset */
    if ( Tcl_GetLongFromObj( interp, objv[1], &adr ) != TCL_OK ) {
        return TCL_ERROR;
    }
    wcs = (AstFrameSet *) adr;

    /* Set the attributes */
    astSet( wcs, Tcl_GetString( objv[2] ) );
    if ( ! astOK ) {
        char *buf = ckalloc( 1024 );
        sprintf( buf, "Failed to set AST attribute (%s)",
                 Tcl_GetString( objv[2] ) );
        astClearStatus;
        Tcl_SetResult( interp, buf, TCL_DYNAMIC );
        return TCL_ERROR;
    }
    return TCL_OK;
}

/**
 * Show an AST object, useful for debugging.
 *
 * First argument is the address of the object and the second the type of
 * display required, native, FITS or XML. Default is native. If a third
 * argument is given it can only be the encoding type for a FITS channel.
 */
static void write_out( const char *card )
{
    fprintf( stdout, "%s\n", card );
}
static int GaiaUtilsAstShow( ClientData clientData, Tcl_Interp *interp,
                             int objc, Tcl_Obj *CONST objv[] )
{
    AstFitsChan *fitschan;
    AstObject *object;
    AstXmlChan *xmlchan;
    int nwrite;
    long adr;

    /* Check arguments, allo up to three  */
    if ( objc < 2 || objc > 4 ) {
        Tcl_WrongNumArgs( interp, 1, objv, 
                          "ast_object [native|FITS|XML] [FITSencoding]" );
        return TCL_ERROR;
    }

    /* Get the object */
    if ( Tcl_GetLongFromObj( interp, objv[1], &adr ) != TCL_OK ) {
        return TCL_ERROR;
    }
    object = (AstObject *) adr;

    /* Determine type of channel */
    nwrite = 0;
    if ( objc == 2 || strcmp( Tcl_GetString( objv[2] ), "native" ) == 0 ) {
        astShow( object );
        nwrite = 1;
        if ( !astOK ) {
            Tcl_SetResult( interp, "Failed to write object via native channel",
                           TCL_VOLATILE );
        }
    }
    else if ( strcmp( Tcl_GetString( objv[2] ), "FITS" ) == 0 ) {
        if ( objc == 4 ) {
            fitschan = (AstFitsChan *) astFitsChan( NULL, &write_out,
                                                    "Encoding=%s",
                                                    Tcl_GetString( objv[3] ) );
        } 
        else {
            fitschan = (AstFitsChan *) astFitsChan( NULL, &write_out, "" );
        }
        nwrite = astWrite( fitschan, object );
        if ( !astOK || nwrite == 0 ) {
            Tcl_SetResult( interp, "Failed to write object via FITS channel",
                           TCL_VOLATILE );
        }
        fitschan = (AstFitsChan *) astAnnul( fitschan );
    }
    else if ( strcmp( Tcl_GetString( objv[2] ), "XML" ) == 0 ) {
        xmlchan = (AstXmlChan *) astXmlChan( NULL, &write_out, "" );
        nwrite = astWrite( xmlchan, object );
        if ( !astOK || nwrite == 0 ) {
            Tcl_SetResult( interp, "Failed to write object via XML channel",
                           TCL_VOLATILE );
        }
        xmlchan = (AstXmlChan *) astAnnul( xmlchan );
    }
    if ( !astOK || nwrite == 0 ) {
        if ( !astOK ) astClearStatus;
        return TCL_ERROR;
    }
    return TCL_OK;
}

/**
 * Test if the value of an AST attribute has been set.
 *
 * There are two arguments, the address of the AST FrameSet and the attribute.
 */
static int GaiaUtilsAstTest( ClientData clientData, Tcl_Interp *interp,
                             int objc, Tcl_Obj *CONST objv[] )
{
    AstFrameSet *wcs;
    long adr;
    int test;

    /* Check arguments, only allow two. */
    if ( objc != 3 ) {
        Tcl_WrongNumArgs( interp, 1, objv, "frameset attribute" );
        return TCL_ERROR;
    }

    /* Get the frameset */
    if ( Tcl_GetLongFromObj( interp, objv[1], &adr ) != TCL_OK ) {
        return TCL_ERROR;
    }
    wcs = (AstFrameSet *) adr;

    /* Do the test, an error equals not set */
    test = astTest( wcs, Tcl_GetString( objv[2] ) );
    if ( ! astOK ) {
        astClearStatus;
        test = 0;
    }
    Tcl_SetObjResult( interp, Tcl_NewBooleanObj( test ) );
    return TCL_OK;
}

/**
 * Check if a given AST FrameSet has an axis of a given frame type.
 *
 * Note we only support a pre-defined set of comparison frame types,
 * at present these are "specframe" and "fluxframe".
 */
static int GaiaUtilsFrameIsA( ClientData clientData, Tcl_Interp *interp,
                              int objc, Tcl_Obj *CONST objv[] )
{
    AstFrame *picked;
    AstFrameSet *wcs;
    char *type;
    int axes[1];
    int axis;
    int isa;
    long adr;

    /* Check arguments, only allow three, the frameset, axis and object type */
    if ( objc != 4 ) {
        Tcl_WrongNumArgs( interp, 1, objv, "frameset axis object_type" );
        return TCL_ERROR;
    }

    /* Get the frameset */
    if ( Tcl_GetLongFromObj( interp, objv[1], &adr ) != TCL_OK ) {
        return TCL_ERROR;
    }
    wcs = (AstFrameSet *) adr;

    /* Get the axis (AST index) */
    if ( Tcl_GetIntFromObj( interp, objv[2], &axis ) != TCL_OK ) {
        return TCL_ERROR;
    }
    axes[0] = axis;
    picked = (AstFrame *) astPickAxes( wcs, 1, axes, NULL );

    /* Check type and do compare */
    type = Tcl_GetString( objv[3] );
    isa = 0;
    if ( strcmp( type, "specframe" ) == 0 ) {
        isa = astIsASpecFrame( picked );
    }
    else if ( strcmp( type, "dsbspecframe" ) == 0 ) {
        isa = astIsADSBSpecFrame( picked );
    }
    else if ( strcmp( type, "fluxframe" ) == 0 ) {
        isa = astIsAFluxFrame( picked );
    }
    else {
        char *buf = ckalloc( 1024 );
        sprintf( buf, "not a known frame type (%s)", type );
        astClearStatus;
        Tcl_SetResult( interp, buf, TCL_DYNAMIC );
        astAnnul( picked );
        return TCL_ERROR;
    }

    Tcl_SetObjResult( interp, Tcl_NewBooleanObj( isa ) );
    astAnnul( picked );
    return TCL_OK;
}

/**
 * Extract a WCS for a two axes from a full WCS. The result is
 * the address of an AST FrameSet.
 *
 * There are five required arguments, the address of the AST FrameSet, the two
 * axes to extract and their lengths (in base units, usually the width and
 * height of the image) and one optional coordinate to use as a replacement
 * value for the removed axis (a const to the PermMap).
 */
static int GaiaUtilsGt2DWcs( ClientData clientData, Tcl_Interp *interp,
                             int objc, Tcl_Obj *CONST objv[] )
{
    AstFrameSet *fullwcs;
    AstFrameSet *newwcs;
    char *error_mess;
    int axis1;
    int axis2;
    int index;
    int length1;
    int length2;
    int result;
    long adr;

    /* Check arguments, only allow five or six, the frameset, the two axes and
     * some scale lengths along the axes (base units), plus the replacement
     * value. */
    if ( objc != 6 && objc != 7 ) {
        Tcl_WrongNumArgs( interp, 1, objv, "frameset axis1 axis2 length1 "
                          "length2 [axis3_coord]" );
        return TCL_ERROR;
    }

    /* Get the frameset */
    if ( Tcl_GetLongFromObj( interp, objv[1], &adr ) != TCL_OK ) {
        return TCL_ERROR;
    }
    fullwcs = (AstFrameSet *) adr;

    /* Get the axes, these are AST ones.*/
    if ( Tcl_GetIntFromObj( interp, objv[2], &axis1 ) != TCL_OK ) {
        return TCL_ERROR;
    }
    if ( Tcl_GetIntFromObj( interp, objv[3], &axis2 ) != TCL_OK ) {
        return TCL_ERROR;
    }

    /* Get the lengths */
    if ( Tcl_GetIntFromObj( interp, objv[4], &length1 ) != TCL_OK ) {
        return TCL_ERROR;
    }
    if ( Tcl_GetIntFromObj( interp, objv[5], &length2 ) != TCL_OK ) {
        return TCL_ERROR;
    }

    /* And the replacement index */
    index = 0;
    if ( objc == 7 ) {
        if ( Tcl_GetIntFromObj( interp, objv[6], &index ) != TCL_OK ) {
            return TCL_ERROR;
        }
    }

    /* Extract 2D WCS */
    result = gaiaUtilsGt2DWcs( fullwcs, axis1, axis2, length1, length2,
                               index, &newwcs, &error_mess );

    /* Export the new WCS as a long containing the address */
    if ( result == TCL_OK ) {
        Tcl_SetObjResult( interp, Tcl_NewLongObj( (long) newwcs ) );
    }
    else {
        Tcl_SetResult( interp, error_mess, TCL_VOLATILE );
        free( error_mess );
    }
    return result;
}

/**
 * Convert a 1D coordinate using a 1D frameset or mapping.
 *
 * There are three arguments, the address of the AST FrameSet or mapping, the
 * coordinate to transform and whether to transform using the forward or
 * inverse transform.
 */
static int GaiaUtilsGtAxisCoord( ClientData clientData, Tcl_Interp *interp,
                                 int objc, Tcl_Obj *CONST objv[] )
{
    AstMapping *mapping;
    double xin[1];
    double xout[1];
    int forward;
    long adr;

    /* Check arguments, only allow three, the frameset or mapping, the
     * coordinate and whether to transform forward or backwards.
     */
    if ( objc != 4 ) {
        Tcl_WrongNumArgs( interp, 1, objv, 
                          "frameset!mapping coordinate ?forward?" );
        return TCL_ERROR;
    }

    /* Get the frameset or mapping */
    if ( Tcl_GetLongFromObj( interp, objv[1], &adr ) != TCL_OK ) {
        return TCL_ERROR;
    }
    mapping = (AstMapping *) adr;

    /* Get the coordinate */
    if ( Tcl_GetDoubleFromObj( interp, objv[2], &xin[0] ) != TCL_OK ) {
        return TCL_ERROR;
    }

    /* Transformation direction */
    if ( Tcl_GetBooleanFromObj( interp, objv[3], &forward ) != TCL_OK ) {
        return TCL_ERROR;
    }

    /* Do the transformation. */
    astTran1( mapping, 1, xin, forward, xout );
    if ( ! astOK ) {
        Tcl_SetResult( interp, "Failed to transform axis coordinate",
                       TCL_VOLATILE );
        astClearStatus;
        return TCL_ERROR;
    }
    if ( xout[0] != AST__BAD ) {
        Tcl_SetObjResult( interp, Tcl_NewDoubleObj( xout[0] ) );
    }
    else {
        Tcl_SetObjResult( interp, Tcl_NewDoubleObj( 0.0 ) );
    }
    return TCL_OK;
}

/**
 * Extract a WCS for a specific axis from a full WCS. The result is
 * the address of an AST FrameSet.
 *
 * There are two arguments, the address of the AST FrameSet and the axis to
 * extract.
 */
static int GaiaUtilsGtAxisWcs( ClientData clientData, Tcl_Interp *interp,
                               int objc, Tcl_Obj *CONST objv[] )
{
    AstFrameSet *fullwcs;
    AstFrameSet *axiswcs;
    char *error_mess;
    int axis;
    int result;
    long adr;

    /* Check arguments, only allow two, the frameset and an axis number. */
    if ( objc != 3 ) {
        Tcl_WrongNumArgs( interp, 1, objv, "frameset axis" );
        return TCL_ERROR;
    }

    /* Get the frameset */
    if ( Tcl_GetLongFromObj( interp, objv[1], &adr ) != TCL_OK ) {
        return TCL_ERROR;
    }
    fullwcs = (AstFrameSet *) adr;

    /* Get the axis */
    axis = -1;
    if ( Tcl_GetIntFromObj( interp, objv[2], &axis ) != TCL_OK ) {
        return TCL_ERROR;
    }
    result= gaiaUtilsGtAxisWcs( fullwcs, axis, &axiswcs, &error_mess );

    /* Export the new WCS as a long containing the address */
    if ( result == TCL_OK ) {
        Tcl_SetObjResult( interp, Tcl_NewLongObj( (long) axiswcs ) );
    }
    else {
        Tcl_SetResult( interp, error_mess, TCL_VOLATILE );
        free( error_mess );
    }
    return result;
}

/**
 * Return a list of Plots that cover each ROI found in a Plot.
 *
 * The arguments are the address of a Plot.
 */
static int GaiaUtilsGtROIPlots( ClientData clientData, Tcl_Interp *interp,
                                int objc, Tcl_Obj *CONST objv[] )
{
    AstKeyMap *rplots;
    AstPlot *roiPlot;
    AstPlot *plot;
    Tcl_Obj *resultObj;
    char *error_mess;
    const char *key;
    int i;
    int size;
    long adr;

    /* Check arguments, only allow one, the Plot frameset. */
    if ( objc != 2 ) {
        Tcl_WrongNumArgs( interp, 1, objv, "plot" );
        return TCL_ERROR;
    }

    /* Get the Plot */
    if ( Tcl_GetLongFromObj( interp, objv[1], &adr ) != TCL_OK ) {
        return TCL_ERROR;
    }
    plot = (AstPlot *) adr;

    /* Do the work (slight deferral to deal with ADAM status). */
    if ( gaiaUtilsAtlPlROI( plot, &rplots, &error_mess ) != TCL_OK ) {
        Tcl_SetResult( interp, error_mess, TCL_VOLATILE );
        free( error_mess );
        return TCL_ERROR;
    }

    /* Convert keymap into a Tcl list */
    resultObj = Tcl_GetObjResult( interp );

    size = astMapSize( rplots );
    for ( i = 0; i < size; i++ ) {
        key = astMapKey( rplots, i );
        astMapGet0A( rplots, key, &roiPlot );
        Tcl_ListObjAppendElement( interp, resultObj,
                                  Tcl_NewLongObj( (long) roiPlot ) );
    }
    astAnnul( rplots );

    return TCL_OK;
}


/**
 * Get a file from a specified URL and return its content as the result.
 *
 * One argument the URL.
 */
static int GaiaUtilsUrlGet( ClientData clientData, Tcl_Interp *interp,
                            int objc, Tcl_Obj *CONST objv[] )
{
    HTTP http;
    char *result = NULL;
    int nlines = 0;

    /* Check arguments, require one */
    if ( objc != 2 ) {
        Tcl_WrongNumArgs( interp, 1, objv, "URL" );
        return TCL_ERROR;
    }

    //  Get the file.
    result = http.get( Tcl_GetString( objv[1] ), nlines, 1 );

    //  And return its content as the result.
    if ( result != NULL ) {
        Tcl_SetResult( interp, result, TCL_VOLATILE );
        return TCL_OK;
    }
    return TCL_ERROR;
}

