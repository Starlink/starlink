/*+
 *   Name:
 *      GaiaUtilsTcl

 *   Purpose:
 *      Utility routines from Tcl scripts.

 *   Language:
 *      C++

 *  Copyright:
 *     Copyright (C) 2006 Particle Physics & Astronomy Research Council.
 *     Copyright (C) 2007-2009 Science and Technology Facilities Council.
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

 *  Notes:
 *      C++ as it makes the Skycat class available, does not define a class.

 *   Authors:
 *      PWD: Peter W. Draper (Starlink - University of Durham)

 *   History:
 *      31-MAR-2006 (PWD):
 *         Original version.
 *      {enter_changes_here}
 *-
 */

#if HAVE_CONFIG_H
#include "config.h"
#endif

#include <float.h>
#include <tcl.h>
#include <HTTP.h>
extern "C" {
#include <ast.h>
}
#include "StarFitsIO.h"
#include "GaiaFITS.h"
#include "gaiaUtils.h"
#include "grf_tkcan.h"

static const double pi_ = 3.14159265358979323846;
static const double r2d_ = 180.0/pi_;
static const double d2r_ = pi_/180.0;

/* Struct for controlling line sent to AST via source function. */
struct sourceInfo {
    const char **lines;
    int nlines;
    int next;
};
typedef struct sourceInfo sourceInfo;
static sourceInfo SOURCEInfo;

/* Local function to supply as AST channel source. */
static const char *channel_source()
{
    if ( SOURCEInfo.next < SOURCEInfo.nlines ) {
        return SOURCEInfo.lines[SOURCEInfo.next++];
    }
    return NULL;
}

/* Local prototypes */
static int GaiaUtilsAstAnnul( ClientData clientData, Tcl_Interp *interp,
                              int objc, Tcl_Obj *CONST objv[] );
static int GaiaUtilsAstAxDistance( ClientData clientData, Tcl_Interp *interp,
                                   int objc, Tcl_Obj *CONST objv[] );
static int GaiaUtilsAstAxOffsets( ClientData clientData, Tcl_Interp *interp,
                                  int objc, Tcl_Obj *CONST objv[] );
static int GaiaUtilsAstClear( ClientData clientData, Tcl_Interp *interp,
                              int objc, Tcl_Obj *CONST objv[] );
static int GaiaUtilsAstConvert( ClientData clientData, Tcl_Interp *interp,
                                int objc, Tcl_Obj *CONST objv[] );
static int GaiaUtilsAstCopy( ClientData clientData, Tcl_Interp *interp,
                             int objc, Tcl_Obj *CONST objv[] );
static int GaiaUtilsAstCreate( ClientData clientData, Tcl_Interp *interp,
                               int objc, Tcl_Obj *CONST objv[] );
static int GaiaUtilsAstDomains( ClientData clientData, Tcl_Interp *interp,
                                int objc, Tcl_Obj *CONST objv[] );
static int GaiaUtilsAstFindFrame( ClientData clientData, Tcl_Interp *interp,
                                  int objc, Tcl_Obj *CONST objv[] );
static int GaiaUtilsAstFormat( ClientData clientData, Tcl_Interp *interp,
                               int objc, Tcl_Obj *CONST objv[] );
static int GaiaUtilsAstGet( ClientData clientData, Tcl_Interp *interp,
                            int objc, Tcl_Obj *CONST objv[] );
static int GaiaUtilsAstGetRefPos( ClientData clientData, Tcl_Interp *interp,
                                  int objc, Tcl_Obj *CONST objv[] );
static int GaiaUtilsAstLinearApprox( ClientData clientData, Tcl_Interp *interp,
                                     int objc, Tcl_Obj *CONST objv[] );
static int GaiaUtilsAstRegionPars( ClientData clientData, Tcl_Interp *interp,
                                   int objc, Tcl_Obj *CONST objv[] );
static int GaiaUtilsAstSet( ClientData clientData, Tcl_Interp *interp,
                            int objc, Tcl_Obj *CONST objv[] );
static int GaiaUtilsAstShow( ClientData clientData, Tcl_Interp *interp,
                             int objc, Tcl_Obj *CONST objv[] );
static int GaiaUtilsAstSkyFrame( ClientData clientData, Tcl_Interp *interp,
                                 int objc, Tcl_Obj *CONST objv[] );
static int GaiaUtilsAstSkyFrameSet( ClientData clientData, Tcl_Interp *interp,
                                    int objc, Tcl_Obj *CONST objv[] );
static int GaiaUtilsAstTest( ClientData clientData, Tcl_Interp *interp,
                             int objc, Tcl_Obj *CONST objv[] );
static int GaiaUtilsAstTran2( ClientData clientData, Tcl_Interp *interp,
                              int objc, Tcl_Obj *CONST objv[] );
static int GaiaUtilsAstTranN( ClientData clientData, Tcl_Interp *interp,
                              int objc, Tcl_Obj *CONST objv[] );
static int GaiaUtilsAstUnFormat( ClientData clientData, Tcl_Interp *interp,
                                 int objc, Tcl_Obj *CONST objv[] );
static int GaiaUtilsDescribeAxes( ClientData clientData, Tcl_Interp *interp,
                                  int objc, Tcl_Obj *CONST objv[] );
static int GaiaUtilsFrameIsA( ClientData clientData, Tcl_Interp *interp,
                              int objc, Tcl_Obj *CONST objv[] );
static int GaiaUtilsGrfAddColour( ClientData clientData, Tcl_Interp *interp,
                                  int objc, Tcl_Obj *CONST objv[] );
static int GaiaUtilsGt2DWcs( ClientData clientData, Tcl_Interp *interp,
                             int objc, Tcl_Obj *CONST objv[] );
static int GaiaUtilsGtAxis( ClientData clientData, Tcl_Interp *interp,
                            int objc, Tcl_Obj *CONST objv[] );
static int GaiaUtilsGtAxisCoord( ClientData clientData, Tcl_Interp *interp,
                                 int objc, Tcl_Obj *CONST objv[] );
static int GaiaUtilsGtAxisWcs( ClientData clientData, Tcl_Interp *interp,
                               int objc, Tcl_Obj *CONST objv[] );
static int GaiaUtilsGtFrame( ClientData clientData, Tcl_Interp *interp,
                             int objc, Tcl_Obj *CONST objv[] );
static int GaiaUtilsGtROIPlots( ClientData clientData, Tcl_Interp *interp,
                                int objc, Tcl_Obj *CONST objv[] );
static int GaiaUtilsFitsMoc( ClientData clientData, Tcl_Interp *interp,
                             int objc, Tcl_Obj *CONST objv[] );
static int GaiaUtilsRegionType( ClientData clientData, Tcl_Interp *interp,
                                int objc, Tcl_Obj *CONST objv[] );
static int GaiaUtilsShiftWcs( ClientData clientData, Tcl_Interp *interp,
                              int objc, Tcl_Obj *CONST objv[] );
static int GaiaUtilsStcRegion( ClientData clientData, Tcl_Interp *interp,
                               int objc, Tcl_Obj *CONST objv[] );
static int GaiaUtilsUrlGet( ClientData clientData, Tcl_Interp *interp,
                            int objc, Tcl_Obj *CONST objv[] );

/**
 * Register all the gaiautils:: commands.
 */
int GaiaUtils_Init( Tcl_Interp *interp )
{
    Tcl_CreateObjCommand( interp, "gaiautils::astannul", GaiaUtilsAstAnnul,
                          (ClientData) NULL, (Tcl_CmdDeleteProc *) NULL );

    Tcl_CreateObjCommand( interp, "gaiautils::astaxdistance",
                          GaiaUtilsAstAxDistance, (ClientData) NULL,
                          (Tcl_CmdDeleteProc *) NULL );

    Tcl_CreateObjCommand( interp, "gaiautils::astaxoffset",
                          GaiaUtilsAstAxOffsets, (ClientData) NULL,
                          (Tcl_CmdDeleteProc *) NULL );

    Tcl_CreateObjCommand( interp, "gaiautils::astclear", GaiaUtilsAstClear,
                          (ClientData) NULL, (Tcl_CmdDeleteProc *) NULL );

    Tcl_CreateObjCommand( interp, "gaiautils::astconvert", GaiaUtilsAstConvert,
                          (ClientData) NULL, (Tcl_CmdDeleteProc *) NULL );

    Tcl_CreateObjCommand( interp, "gaiautils::astcreate", GaiaUtilsAstCreate,
                          (ClientData) NULL, (Tcl_CmdDeleteProc *) NULL );

    Tcl_CreateObjCommand( interp, "gaiautils::astcopy", GaiaUtilsAstCopy,
                          (ClientData) NULL, (Tcl_CmdDeleteProc *) NULL );

    Tcl_CreateObjCommand( interp, "gaiautils::astdomains", GaiaUtilsAstDomains,
                          (ClientData) NULL, (Tcl_CmdDeleteProc *) NULL );

    Tcl_CreateObjCommand( interp, "gaiautils::astfindframe",
                          GaiaUtilsAstFindFrame,
                          (ClientData) NULL, (Tcl_CmdDeleteProc *) NULL );

    Tcl_CreateObjCommand( interp, "gaiautils::astget", GaiaUtilsAstGet,
                          (ClientData) NULL, (Tcl_CmdDeleteProc *) NULL );

    Tcl_CreateObjCommand( interp, "gaiautils::astgetrefpos",
                          GaiaUtilsAstGetRefPos, (ClientData) NULL,
                          (Tcl_CmdDeleteProc *) NULL );

    Tcl_CreateObjCommand( interp, "gaiautils::astformat", GaiaUtilsAstFormat,
                          (ClientData) NULL, (Tcl_CmdDeleteProc *) NULL );

    Tcl_CreateObjCommand( interp, "gaiautils::astlinearapprox",
                          GaiaUtilsAstLinearApprox, (ClientData) NULL,
                          (Tcl_CmdDeleteProc *) NULL );

    Tcl_CreateObjCommand( interp, "gaiautils::astregionpars",
                          GaiaUtilsAstRegionPars,
                          (ClientData) NULL, (Tcl_CmdDeleteProc *) NULL );

    Tcl_CreateObjCommand( interp, "gaiautils::astset", GaiaUtilsAstSet,
                          (ClientData) NULL, (Tcl_CmdDeleteProc *) NULL );

    Tcl_CreateObjCommand( interp, "gaiautils::astshow", GaiaUtilsAstShow,
                          (ClientData) NULL, (Tcl_CmdDeleteProc *) NULL );

    Tcl_CreateObjCommand( interp, "gaiautils::astskyframe",
                          GaiaUtilsAstSkyFrame, (ClientData) NULL,
                          (Tcl_CmdDeleteProc *) NULL );

    Tcl_CreateObjCommand( interp, "gaiautils::astskyframeset",
                          GaiaUtilsAstSkyFrameSet, (ClientData) NULL,
                          (Tcl_CmdDeleteProc *) NULL );

    Tcl_CreateObjCommand( interp, "gaiautils::asttest", GaiaUtilsAstTest,
                          (ClientData) NULL, (Tcl_CmdDeleteProc *) NULL );

    Tcl_CreateObjCommand( interp, "gaiautils::asttran2", GaiaUtilsAstTran2,
                          (ClientData) NULL, (Tcl_CmdDeleteProc *) NULL );

    Tcl_CreateObjCommand( interp, "gaiautils::asttrann", GaiaUtilsAstTranN,
                          (ClientData) NULL, (Tcl_CmdDeleteProc *) NULL );

    Tcl_CreateObjCommand( interp, "gaiautils::astunformat",
                          GaiaUtilsAstUnFormat, (ClientData) NULL,
                          (Tcl_CmdDeleteProc *) NULL );

    Tcl_CreateObjCommand( interp, "gaiautils::describeaxes",
                          GaiaUtilsDescribeAxes, (ClientData) NULL,
                          (Tcl_CmdDeleteProc *) NULL );

    Tcl_CreateObjCommand( interp, "gaiautils::frameisa", GaiaUtilsFrameIsA,
                          (ClientData) NULL, (Tcl_CmdDeleteProc *) NULL );

    Tcl_CreateObjCommand( interp, "gaiautils::grfaddcolour",
                          GaiaUtilsGrfAddColour, (ClientData) NULL,
                          (Tcl_CmdDeleteProc *) NULL );

    Tcl_CreateObjCommand( interp, "gaiautils::get2dwcs", GaiaUtilsGt2DWcs,
                          (ClientData) NULL, (Tcl_CmdDeleteProc *) NULL );

    Tcl_CreateObjCommand( interp, "gaiautils::getaxis", GaiaUtilsGtAxis,
                          (ClientData) NULL, (Tcl_CmdDeleteProc *) NULL );

    Tcl_CreateObjCommand( interp, "gaiautils::getaxiscoord",
                          GaiaUtilsGtAxisCoord, (ClientData) NULL,
                          (Tcl_CmdDeleteProc *) NULL );

    Tcl_CreateObjCommand( interp, "gaiautils::getaxiswcs", GaiaUtilsGtAxisWcs,
                          (ClientData) NULL, (Tcl_CmdDeleteProc *) NULL );

    Tcl_CreateObjCommand( interp, "gaiautils::getframe", GaiaUtilsGtFrame,
                          (ClientData) NULL, (Tcl_CmdDeleteProc *) NULL );

    Tcl_CreateObjCommand( interp, "gaiautils::getroiplots",
                          GaiaUtilsGtROIPlots, (ClientData) NULL,
                          (Tcl_CmdDeleteProc *) NULL );

    Tcl_CreateObjCommand( interp, "gaiautils::fitsmoc", GaiaUtilsFitsMoc,
                          (ClientData) NULL, (Tcl_CmdDeleteProc *) NULL );

    Tcl_CreateObjCommand( interp, "gaiautils::regiontype", GaiaUtilsRegionType,
                          (ClientData) NULL, (Tcl_CmdDeleteProc *) NULL );

    Tcl_CreateObjCommand( interp, "gaiautils::shiftwcs", GaiaUtilsShiftWcs,
                          (ClientData) NULL, (Tcl_CmdDeleteProc *) NULL );

    Tcl_CreateObjCommand( interp, "gaiautils::stcregion", GaiaUtilsStcRegion,
                          (ClientData) NULL, (Tcl_CmdDeleteProc *) NULL );

    Tcl_CreateObjCommand( interp, "gaiautils::urlget", GaiaUtilsUrlGet,
                          (ClientData) NULL, (Tcl_CmdDeleteProc *) NULL );

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
 * Get the value of an AST attribute from an AST object.
 *
 * There are two arguments, the address of the AST object and the attribute.
 */
static int GaiaUtilsAstGet( ClientData clientData, Tcl_Interp *interp,
                            int objc, Tcl_Obj *CONST objv[] )
{
    AstObject *object;
    long adr;
    const char *value;

    /* Check arguments, only allow two. */
    if ( objc != 3 ) {
        Tcl_WrongNumArgs( interp, 1, objv, "AST_object attribute" );
        return TCL_ERROR;
    }

    /* Get the object. */
    if ( Tcl_GetLongFromObj( interp, objv[1], &adr ) != TCL_OK ) {
        return TCL_ERROR;
    }
    object = (AstObject *) adr;

    /* Get the value. */
    value = astGetC( object, Tcl_GetString( objv[2] ) );
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
 * Apply AST attributes to an AST object.
 *
 * There are two arguments, the address of the AST object and the attributes
 * string ("option=value,option=value" etc.).
 */
static int GaiaUtilsAstSet( ClientData clientData, Tcl_Interp *interp,
                            int objc, Tcl_Obj *CONST objv[] )
{
    AstObject *object;
    long adr;

    /* Check arguments, only allow two. */
    if ( objc != 3 ) {
        Tcl_WrongNumArgs( interp, 1, objv, "AST_object attributes" );
        return TCL_ERROR;
    }

    /* Get the object. */
    if ( Tcl_GetLongFromObj( interp, objv[1], &adr ) != TCL_OK ) {
        return TCL_ERROR;
    }
    object = (AstObject *) adr;

    /* Set the attributes */
    astSet( object, Tcl_GetString( objv[2] ), " " );
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
 * Clear an AST attribute.
 *
 * There are two arguments, the address of the AST object and the attribute
 * to clear.
 */
static int GaiaUtilsAstClear( ClientData clientData, Tcl_Interp *interp,
                              int objc, Tcl_Obj *CONST objv[] )
{
    AstObject *object;
    long adr;

    /* Check arguments, only allow two. */
    if ( objc != 3 ) {
        Tcl_WrongNumArgs( interp, 1, objv, "AST_object attribute" );
        return TCL_ERROR;
    }

    /* Get the AST object */
    if ( Tcl_GetLongFromObj( interp, objv[1], &adr ) != TCL_OK ) {
        return TCL_ERROR;
    }
    object = (AstObject *) adr;

    /* Clear the attribute */
    astClear( object, Tcl_GetString( objv[2] ) );
    if ( ! astOK ) {
        astClearStatus;
        Tcl_SetResult( interp, "Failed to clear AST attribute", TCL_VOLATILE );
        return TCL_ERROR;
    }
    return TCL_OK;
}

/**
 * Test if the value of an AST attribute has been set.
 *
 * There are two arguments, the address of the AST object and the attribute.
 */
static int GaiaUtilsAstTest( ClientData clientData, Tcl_Interp *interp,
                             int objc, Tcl_Obj *CONST objv[] )
{
    AstObject *object;
    long adr;
    int test;

    /* Check arguments, only allow two. */
    if ( objc != 3 ) {
        Tcl_WrongNumArgs( interp, 1, objv, "AST_object attribute" );
        return TCL_ERROR;
    }

    /* Get the AST object. */
    if ( Tcl_GetLongFromObj( interp, objv[1], &adr ) != TCL_OK ) {
        return TCL_ERROR;
    }
    object = (AstObject *) adr;

    /* Do the test, an error equals not set */
    test = astTest( object, Tcl_GetString( objv[2] ) );
    if ( ! astOK ) {
        astClearStatus;
        test = 0;
    }
    Tcl_SetObjResult( interp, Tcl_NewBooleanObj( test ) );
    return TCL_OK;
}

/**
 * Annul an AST object.
 *
 * There is one argument, the address of the AST object.
 */
static int GaiaUtilsAstAnnul( ClientData clientData, Tcl_Interp *interp,
                              int objc, Tcl_Obj *CONST objv[] )
{
    long adr;

    /* Check arguments, only allow one. */
    if ( objc != 2 ) {
        Tcl_WrongNumArgs( interp, 1, objv, "AST_object" );
        return TCL_ERROR;
    }

    /* Get the object */
    if ( Tcl_GetLongFromObj( interp, objv[1], &adr ) != TCL_OK ) {
        return TCL_ERROR;
    }
    (void) astAnnul( (AstObject *) adr );

    if ( ! astOK ) {
        astClearStatus;
        Tcl_SetResult( interp, "Failed to annul AST object", TCL_VOLATILE );
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
    const char* encoding;
    int nwrite;
    long adr;

    /* Check arguments, allow up to three  */
    if ( objc < 2 || objc > 4 ) {
        Tcl_WrongNumArgs( interp, 1, objv,
                          "AST_object [native|FITS|XML] [FITSencoding]" );
        return TCL_ERROR;
    }

    /* Get the object */
    if ( Tcl_GetLongFromObj( interp, objv[1], &adr ) != TCL_OK ) {
        return TCL_ERROR;
    }
    object = (AstObject *) adr;

    /* Determine type of channel */
    nwrite = 0;
    objc == 2 ? encoding = "native" : encoding = Tcl_GetString( objv[2] );

    if ( strcmp( encoding, "native" ) == 0 ) {
        astShow( object );
        nwrite = 1;
        if ( !astOK ) {
            Tcl_SetResult( interp, "Failed to write object via native channel",
                           TCL_VOLATILE );
        }
    }
    else if ( strcmp( encoding, "FITS" ) == 0 ) {
        if ( objc == 4 ) {
            fitschan = (AstFitsChan *) astFitsChan( NULL, &write_out,
                                                    "Encoding=%s",
                                                    Tcl_GetString( objv[3] ) );
        }
        else {
            fitschan = (AstFitsChan *) astFitsChan( NULL, &write_out, " " );
        }
        nwrite = astWrite( fitschan, object );
        if ( !astOK || nwrite == 0 ) {
            Tcl_SetResult( interp, "Failed to write object via FITS channel",
                           TCL_VOLATILE );
        }
        fitschan = (AstFitsChan *) astAnnul( fitschan );
    }
    else if ( strcmp( encoding, "XML" ) == 0 ) {
        xmlchan = (AstXmlChan *) astXmlChan( NULL, &write_out, " " );
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
 * Create an AST object from an encoding.
 *
 * The first argument should be the encoding type, native, FITS or XML.
 * The second argument a string containing the encoding.
 */
static int GaiaUtilsAstCreate( ClientData clientData, Tcl_Interp *interp,
                               int objc, Tcl_Obj *CONST objv[] )
{
    AstObject *object = NULL;
    AstChannel *chan = NULL;
    const char *encoding = NULL;
    const char *content = NULL;

    /* Check arguments, allow two  */
    if ( objc != 3 ) {
        Tcl_WrongNumArgs( interp, 1, objv, "native|FITS|XML encoding" );
        return TCL_ERROR;
    }

    /* Get the type of encoding and content. */
    encoding = Tcl_GetString( objv[1] );
    content = Tcl_GetString( objv[2] );

    /* Determine type of channel, XXX FITS and XML not tested. */
    if ( strcmp( encoding, "native" ) == 0 ) {
        chan = (AstChannel *) astChannel( &channel_source, NULL, " " );
    }
    else if ( strcmp( encoding, "FITS" ) == 0 ) {
        chan = (AstChannel *) astFitsChan( &channel_source, NULL, " " );
    }
    else if ( strcmp( encoding, "XML" ) == 0 ) {
        chan = (AstChannel *) astXmlChan( &channel_source, NULL, " " );
    }
    else {
        char *buf = ckalloc( 1024 );
        sprintf( buf, "Unknown AST channel type: %s", encoding );
        Tcl_SetResult( interp, buf, TCL_DYNAMIC );
        return TCL_ERROR;
    }

    /* Break content down into NULL terminated lines. */
    SOURCEInfo.next = 0;
    char *buf = strdup( content );
    char *p = buf;
    int nlines = 0;
    while( *p ) {
        if ( *p == '\n' ) {
            nlines++;
        }
        p++;
    }
    SOURCEInfo.lines = (const char **) malloc( nlines * sizeof( char * ) );
    SOURCEInfo.nlines = nlines;

    char *c = buf;
    p = buf;
    nlines = 0;
    while( *p ) {
        if ( *p == '\n' ) {
            SOURCEInfo.lines[nlines++] = c;
            *p = '\0';
            c = p + 1;
        }
        p++;
    }

    /* Read the object */
    object = (AstObject *) astRead( chan );
    (void) astAnnul( chan );

    free( buf );
    free( SOURCEInfo.lines );

    if ( !astOK ) {
        astClearStatus;
        return TCL_ERROR;
    }

    /* Export object */
    Tcl_SetObjResult( interp, Tcl_NewLongObj( (long) object ) );
    return TCL_OK;
}

/**
 * Get the spectral reference position (RefRA,RefDec) in decimal degrees J2000.
 *
 * There is one argument, the address of a SpecFrame. Returns the
 * pair of values.
 */
static int GaiaUtilsAstGetRefPos( ClientData clientData, Tcl_Interp *interp,
                                  int objc, Tcl_Obj *CONST objv[] )
{
    AstSpecFrame *object;
    Tcl_Obj *resultObj;
    double lat;
    double lon;
    long adr;

    /* Check arguments, only allow one. */
    if ( objc != 2 ) {
        Tcl_WrongNumArgs( interp, 1, objv, "specframe" );
        return TCL_ERROR;
    }

    /* Get the AstSpecFrame */
    if ( Tcl_GetLongFromObj( interp, objv[1], &adr ) != TCL_OK ) {
        return TCL_ERROR;
    }
    object = (AstSpecFrame *) adr;

    /* Get the value */
    astGetRefPos( object, (AstSkyFrame *) NULL, &lon, &lat );
    if ( ! astOK ) {
        astClearStatus;
        Tcl_SetResult( interp, "Failed to get reference positions",
                       TCL_VOLATILE );
        return TCL_ERROR;
    }

    /* Radians to degrees */
    lat *= r2d_;
    lon *= r2d_;

    Tcl_ResetResult( interp );
    resultObj = Tcl_GetObjResult( interp );
    Tcl_ListObjAppendElement( interp, resultObj, Tcl_NewDoubleObj( lon ) );
    Tcl_ListObjAppendElement( interp, resultObj, Tcl_NewDoubleObj( lat ) );
    return TCL_OK;
}

/**
 * Check if the current frame of a FrameSet has an axis of a given frame type.
 *
 * Note we only support a pre-defined set of comparison frame types, at
 * present these are "specframe", "dsbspecframe", "fluxframe" and "timeframe".
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
    else if ( strcmp( type, "timeframe" ) == 0 ) {
        isa = astIsATimeFrame( picked );
    }
    else {
        char *buf = ckalloc( 1024 );
        sprintf( buf, "not a known frame type (%s)", type );
        astClearStatus;
        Tcl_SetResult( interp, buf, TCL_DYNAMIC );
        (void) astAnnul( picked );
        return TCL_ERROR;
    }

    Tcl_SetObjResult( interp, Tcl_NewBooleanObj( isa ) );
    (void) astAnnul( picked );
    return TCL_OK;
}

/**
 * Return a list describing the current axes of a given FrameSet.
 *
 * The list will consist of the KAPPA friendly axes descriptions, "spec",
 * "skylon", "skylat" and "time". Unrecognised axes will just be given the
 * name "unknown".
 */
static int GaiaUtilsDescribeAxes( ClientData clientData, Tcl_Interp *interp,
                                  int objc, Tcl_Obj *CONST objv[] )
{
    AstFrameSet *wcs;
    Tcl_Obj *resultObj;
    char buffer[16];
    const char *domain;
    int lataxis;
    int naxes;
    long adr;

    /* Check arguments, only allow one, the frameset. */
    if ( objc != 2 ) {
        Tcl_WrongNumArgs( interp, 1, objv, "frameset" );
        return TCL_ERROR;
    }

    /* Get the frameset */
    if ( Tcl_GetLongFromObj( interp, objv[1], &adr ) != TCL_OK ) {
        return TCL_ERROR;
    }
    wcs = (AstFrameSet *) adr;

    /*  Get the number of axes in the supplied Frame. */
    naxes = astGetI( wcs, "naxes" );

    resultObj = Tcl_GetObjResult( interp );

    /*  For each axis get the domain. */
    for ( int i = 1; i <= naxes; i++ ) {
        sprintf( buffer, "domain(%d)", i );
        domain = astGetC( wcs, buffer );
        if ( strcmp( domain, "SPECTRUM" ) == 0 ||
             strcmp( domain, "DSBSPECTRUM" ) == 0 ) {
            Tcl_ListObjAppendElement( interp, resultObj,
                                      Tcl_NewStringObj( "spec", -1 ) );
        }
        else if ( strcmp( domain, "TIME" ) == 0 ) {
            Tcl_ListObjAppendElement( interp, resultObj,
                                      Tcl_NewStringObj( "time", -1 ) );
        }
        else if ( strcmp( domain, "SKY" ) == 0 ) {

            /*  Latitude or longitude? */
            sprintf( buffer, "lataxis(%d)", i );
            lataxis = astGetI( wcs, buffer );
            if ( i == lataxis ) {
                Tcl_ListObjAppendElement( interp, resultObj,
                                          Tcl_NewStringObj( "skylat", -1 ) );
            }
            else {
                Tcl_ListObjAppendElement( interp, resultObj,
                                          Tcl_NewStringObj( "skylon", -1 ) );
            }
        }
        else {
            Tcl_ListObjAppendElement( interp, resultObj,
                                      Tcl_NewStringObj( "unknown", -1 ) );
        }
    }
    if ( ! astOK ) {
        astClearStatus;
        Tcl_SetResult( interp, "Failed to describe frameset axes",
                       TCL_VOLATILE );
        return TCL_ERROR;
    }
    return TCL_OK;
}

/**
 * Return a list of the domains in a given FrameSet.
 */
static int GaiaUtilsAstDomains( ClientData clientData, Tcl_Interp *interp,
                                int objc, Tcl_Obj *CONST objv[] )
{
    AstFrameSet *wcs;
    Tcl_Obj *resultObj;
    char buffer[16];
    const char *domain;
    int nframes;
    long adr;
    int current;

    /* Check arguments, only allow one, the frameset. */
    if ( objc != 2 ) {
        Tcl_WrongNumArgs( interp, 1, objv, "frameset" );
        return TCL_ERROR;
    }

    /* Get the frameset */
    if ( Tcl_GetLongFromObj( interp, objv[1], &adr ) != TCL_OK ) {
        return TCL_ERROR;
    }
    wcs = (AstFrameSet *) adr;

    /* Get the number of domains. */
    nframes = astGetI( wcs, "Nframe" );

    /* Get current domain, need to restore this before exiting. */
    current = astGetI( wcs, "Current" );

    resultObj = Tcl_GetObjResult( interp );

    for ( int i = 1; i <= nframes; i++ ) {

        /* Switch current frame so we can query the domain. */
        astSetI( wcs, "Current", i );
        domain = astGetC( wcs, "Domain" );

        /* If no domain, create one. */
        if ( strlen( domain ) == 0 ) {
            domain = buffer;
            sprintf( buffer, "Domain%d", i );
        }
        Tcl_ListObjAppendElement( interp, resultObj,
                                  Tcl_NewStringObj( domain, -1 ) );
    }

    /* Restore current frame. */
    astSetI( wcs, "Current", current );

    if ( ! astOK ) {
        astClearStatus;
        Tcl_SetResult( interp, "Failed to list coordinate domains",
                       TCL_VOLATILE );
        return TCL_ERROR;
    }
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
 * Return an axis of the given Frame (which can be the current frame of a
 * FrameSet)
 *
 * Two arguments the Frame and the axis. The result is the address of the
 * picked AstFrame.
 */
static int GaiaUtilsGtAxis( ClientData clientData, Tcl_Interp *interp,
                            int objc, Tcl_Obj *CONST objv[] )
{
    AstFrame *picked;
    AstObject *wcs;
    int axes[1];
    int axis;
    long adr;

    /* Check arguments, only allow two, the frame and the axis */
    if ( objc != 3 ) {
        Tcl_WrongNumArgs( interp, 1, objv, "frame axis" );
        return TCL_ERROR;
    }

    /* Get the frame/frameset */
    if ( Tcl_GetLongFromObj( interp, objv[1], &adr ) != TCL_OK ) {
        return TCL_ERROR;
    }
    wcs = (AstObject *) adr;

    /* Get the axis (AST index) */
    if ( Tcl_GetIntFromObj( interp, objv[2], &axis ) != TCL_OK ) {
        return TCL_ERROR;
    }
    axes[0] = axis;

    /* Pick the axis */
    picked = (AstFrame *) astPickAxes( wcs, 1, axes, NULL );

    /* Export Frame as a long containing the address */
    if ( astOK ) {
        Tcl_SetObjResult( interp, Tcl_NewLongObj( (long) picked ) );
        return TCL_OK;
    }
    Tcl_SetResult( interp, "Failed to extract axis from WCS",
                   TCL_VOLATILE );
    return TCL_ERROR;
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
                          "frameset|mapping coordinate ?forward?" );
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
 * There are three arguments, the address of the AST FrameSet, the axis to
 * extract and an offset along the axis to adjust for (NDF origin when
 * extracting a section, 0 to ignore).
 */
static int GaiaUtilsGtAxisWcs( ClientData clientData, Tcl_Interp *interp,
                               int objc, Tcl_Obj *CONST objv[] )
{
    AstFrameSet *fullwcs;
    AstFrameSet *axiswcs;
    char *error_mess;
    int axis;
    int offset;
    int result;
    long adr;

    /* Check arguments, the frameset, axis number and offset. */
    if ( objc != 4 ) {
        Tcl_WrongNumArgs( interp, 1, objv, "frameset axis offset" );
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

    /* Get the offset  */
    offset =01;
    if ( Tcl_GetIntFromObj( interp, objv[3], &offset ) != TCL_OK ) {
        return TCL_ERROR;
    }

    result= gaiaUtilsGtAxisWcs( fullwcs, axis, offset, &axiswcs, &error_mess );

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
 * Determine the separation of two values along an axis.
 *
 * There are four arguments, the address of an AstFrame (or subclass),
 * the axis, and two coordinate values. Returns the separation.
 */
static int GaiaUtilsAstAxDistance( ClientData clientData, Tcl_Interp *interp,
                                   int objc, Tcl_Obj *CONST objv[] )
{
    AstFrame *frame;
    double dist;
    double value1;
    double value2;
    int axis;
    long adr;

    /* Check arguments, only allow four, the frame, the axis and
     * the two axis values */
    if ( objc != 5 ) {
        Tcl_WrongNumArgs( interp, 1, objv,
                          "frame axis axis_coord1 axis_coord2" );
        return TCL_ERROR;
    }

    /* Get the frame */
    if ( Tcl_GetLongFromObj( interp, objv[1], &adr ) != TCL_OK ) {
        return TCL_ERROR;
    }
    frame = (AstFrame *) adr;

    /* Get the axis, an AST one. */
    if ( Tcl_GetIntFromObj( interp, objv[2], &axis ) != TCL_OK ) {
        return TCL_ERROR;
    }

    /* Get the coordinates */
    if ( Tcl_GetDoubleFromObj( interp, objv[3], &value1 ) != TCL_OK ) {
        return TCL_ERROR;
    }
    if ( Tcl_GetDoubleFromObj( interp, objv[4], &value2 ) != TCL_OK ) {
        return TCL_ERROR;
    }

    /* Get the separation */
    dist = astAxDistance( frame, axis, value1, value2 );

    /* Export the result */
    if ( astOK ) {
        Tcl_SetObjResult( interp, Tcl_NewDoubleObj( dist ) );
        return TCL_OK;
    }
    astClearStatus;
    Tcl_SetResult( interp, "Failed to determine separation of two points",
                   TCL_VOLATILE );
    return TCL_ERROR;
}

/**
 * Determine the axis aligned offsets between two 2D positions.
 *
 * There are five arguments, the address of an AstFrame (or subclass),
 * and the four coordinate values. Returns the offsets along the
 * current frame's axes between the two points. Since both axes are
 * determined together any positional changes needed in the offsets
 * (like latitude corrections) are applied by AST.
 */
static int GaiaUtilsAstAxOffsets( ClientData clientData, Tcl_Interp *interp,
                                  int objc, Tcl_Obj *CONST objv[] )
{
    AstFrame *frame;
    Tcl_Obj *resultObj;
    double dist;
    double offset1;
    double offset2;
    double p1[2];
    double p2[2];
    double p3[2];
    double p4[2];
    long adr;

    /* Check arguments, only allow five, the frame and the four coordinates. */
    if ( objc != 6 ) {
        Tcl_WrongNumArgs( interp, 1, objv, "frame axis1_coord1 axis1_coord2 "
                          "axis2_coord1 axis2_coord2" );
        return TCL_ERROR;
    }

    /* Get the frame */
    if ( Tcl_GetLongFromObj( interp, objv[1], &adr ) != TCL_OK ) {
        return TCL_ERROR;
    }
    frame = (AstFrame *) adr;

    /* Get the coordinates, these are positions 1 and 3 for astResolve. */
    if ( Tcl_GetDoubleFromObj( interp, objv[2], &p1[0] ) != TCL_OK ) {
        return TCL_ERROR;
    }
    if ( Tcl_GetDoubleFromObj( interp, objv[3], &p1[1] ) != TCL_OK ) {
        return TCL_ERROR;
    }

    if ( Tcl_GetDoubleFromObj( interp, objv[4], &p3[0] ) != TCL_OK ) {
        return TCL_ERROR;
    }
    if ( Tcl_GetDoubleFromObj( interp, objv[5], &p3[1] ) != TCL_OK ) {
        return TCL_ERROR;
    }

    /* Second position is dropped line onto first axis.
     *           p3
     *           |
     *    p1-----p2
     */
    p2[0] = p3[0];
    p2[1] = p1[1];

    /* And get the lengths representing the offset magnitude. */
    astResolve( frame, p1, p2, p3, p4, &offset1, &offset2 );

    /* Keep the correct signs using the full axis offsets. */
    dist = astAxDistance( frame, 1, p1[0], p3[0] );
    if ( dist < 0.0 ) {
        offset1 *= -1.0;
    }
    dist = astAxDistance( frame, 2, p1[1], p3[1] );
    if ( dist < 0.0 ) {
        offset2 *= -1.0;
    }

    /* Export the offsets */
    if ( astOK ) {
        Tcl_ResetResult( interp );
        resultObj = Tcl_GetObjResult( interp );
        Tcl_ListObjAppendElement(interp, resultObj, Tcl_NewDoubleObj(offset1));
        Tcl_ListObjAppendElement(interp, resultObj, Tcl_NewDoubleObj(offset2));
        return TCL_OK;
    }
    astClearStatus;
    Tcl_SetResult( interp, "Failed to determine offsets between two points",
                   TCL_VOLATILE );
    return TCL_ERROR;
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
    Tcl_ResetResult( interp );
    resultObj = Tcl_GetObjResult( interp );

    size = astMapSize( rplots );
    for ( i = 0; i < size; i++ ) {
        key = astMapKey( rplots, i );
        astMapGet0A( rplots, key, &roiPlot );
        Tcl_ListObjAppendElement( interp, resultObj,
                                  Tcl_NewLongObj( (long) roiPlot ) );
    }
    (void) astAnnul( rplots );

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

/**
 * Create an AST SkyFrame
 *
 * There is one argument the attributes to use when creating the SkyFrame.
 * The result is the address of the new object.
 */
static int GaiaUtilsAstSkyFrame( ClientData clientData, Tcl_Interp *interp,
                                 int objc, Tcl_Obj *CONST objv[] )
{
    AstSkyFrame *skyframe = NULL;

    /* Check arguments, only allow one. */
    if ( objc != 2 ) {
        Tcl_WrongNumArgs( interp, 1, objv, "attributes" );
        return TCL_ERROR;
    }

    /* Create the SkyFrame */
    skyframe = (AstSkyFrame*) astSkyFrame( Tcl_GetString( objv[1] ), " " );

    /* Export the new object as a long containing the address */
    if ( astOK ) {
        Tcl_SetObjResult( interp, Tcl_NewLongObj( (long) skyframe ) );
        return TCL_OK;
    }
    astClearStatus;
    Tcl_SetResult( interp, "Failed to create SkyFrame", TCL_VOLATILE );
    return TCL_ERROR;
}

/**
 * Create an AST FrameSet with a single SkyFrame.
 *
 * There is one argument the attributes to use when creating the SkyFrame.
 * The result is the address of the new object. If these are invalid then
 * a default SkyFrame is returned.
 */
static int GaiaUtilsAstSkyFrameSet( ClientData clientData, Tcl_Interp *interp,
                                    int objc, Tcl_Obj *CONST objv[] )
{
    AstSkyFrame *skyframe = NULL;
    AstFrameSet *frameset = NULL;

    /* Check arguments, only allow one. */
    if ( objc != 2 ) {
        Tcl_WrongNumArgs( interp, 1, objv, "attributes" );
        return TCL_ERROR;
    }

    /* Create the SkyFrame, if this fails just return a default SkyFrame */
    skyframe = (AstSkyFrame*) astSkyFrame( Tcl_GetString( objv[1] ), " " );
    if ( !astOK ) {
        astClearStatus;
        skyframe = (AstSkyFrame*) astSkyFrame( " " );
    }

    /* Create the FrameSet */
    frameset = (AstFrameSet *) astFrameSet( skyframe, " " );
    (void) astAnnul( skyframe );

    /* Export the new object as a long containing the address */
    if ( astOK ) {
        Tcl_SetObjResult( interp, Tcl_NewLongObj( (long) frameset ) );
        return TCL_OK;
    }
    astClearStatus;
    Tcl_SetResult( interp, "Failed to create SkyFrameSet", TCL_VOLATILE );
    return TCL_ERROR;
}

/**
 * Format a value along an axis of a given AST Frame or FrameSet.
 *
 * There are three arguments, the address of the AST FrameSet, the axis
 * to use and the value to format. The result is the formatted value.
 */
static int GaiaUtilsAstFormat( ClientData clientData, Tcl_Interp *interp,
                               int objc, Tcl_Obj *CONST objv[] )
{
    AstFrameSet *wcs;
    const char *result;
    double value;
    int axis;
    long adr;

    /* Check arguments, only allow three. */
    if ( objc != 4 ) {
        Tcl_WrongNumArgs( interp, 1, objv, "frameset axis value" );
        return TCL_ERROR;
    }

    /* Get the frameset */
    if ( Tcl_GetLongFromObj( interp, objv[1], &adr ) != TCL_OK ) {
        return TCL_ERROR;
    }
    wcs = (AstFrameSet *) adr;

    /* Get the axis */
    if ( Tcl_GetIntFromObj( interp, objv[2], &axis ) != TCL_OK ) {
        return TCL_ERROR;
    }

    /* Get the value */
    if ( Tcl_GetDoubleFromObj( interp, objv[3], &value ) != TCL_OK ) {
        return TCL_ERROR;
    }

    /* Do the formatting */
    result = astFormat( wcs, axis, value );

    /* Return result */
    if ( ! astOK ) {
        astClearStatus;
        Tcl_SetResult( interp, "Failed to format value" , TCL_VOLATILE );
        return TCL_ERROR;
    }

    Tcl_SetResult( interp, (char *) result, TCL_VOLATILE );
    return TCL_OK;
}

/**
 * Un-format a value along an axis of a given AST Frame or FrameSet.
 *
 * There are three arguments, the address of the AST FrameSet, the axis
 * to use and the value to unformat. The result is the unformatted value.
 */
static int GaiaUtilsAstUnFormat( ClientData clientData, Tcl_Interp *interp,
                                 int objc, Tcl_Obj *CONST objv[] )
{
    AstFrameSet *wcs;
    const char *value;
    double result;
    int axis;
    int nchar;
    long adr;

    /* Check arguments, only allow three. */
    if ( objc != 4 ) {
        Tcl_WrongNumArgs( interp, 1, objv, "frameset axis value" );
        return TCL_ERROR;
    }

    /* Get the frameset */
    if ( Tcl_GetLongFromObj( interp, objv[1], &adr ) != TCL_OK ) {
        return TCL_ERROR;
    }
    wcs = (AstFrameSet *) adr;

    /* Get the axis */
    if ( Tcl_GetIntFromObj( interp, objv[2], &axis ) != TCL_OK ) {
        return TCL_ERROR;
    }

    /* Get the value */
    value = Tcl_GetString( objv[3] );

    /* Do the unformatting */
    nchar = astUnformat( wcs, axis, value, &result );

    /* Return result */
    if ( ! astOK || nchar ==0 ) {
        if ( !astOK ) {
            astClearStatus;
        }
        Tcl_SetResult( interp, "Failed to unformat value" , TCL_VOLATILE );
        return TCL_ERROR;
    }

    Tcl_SetObjResult( interp, Tcl_NewDoubleObj( result ) );
    return TCL_OK;
}

/**
 * Create an AST FrameSet that represents the mapping between two Frames.
 *
 * There are three arguments, the two Frames and the domain to use when
 * transforming between the domains (using astConvert). The result is the
 * address of an AstFrameSet (can be used as a Mapping).
 */
static int GaiaUtilsAstConvert( ClientData clientData, Tcl_Interp *interp,
                                int objc, Tcl_Obj *CONST objv[] )
{
    AstFrame *frame1 = NULL;
    AstFrame *frame2 = NULL;
    AstFrameSet *mapping = NULL;
    long adr;

    /* Check arguments, only allow three. */
    if ( objc != 4 ) {
        Tcl_WrongNumArgs( interp, 1, objv, "frame frame domain_list" );
        return TCL_ERROR;
    }

    /* Get the Frames */
    if ( Tcl_GetLongFromObj( interp, objv[1], &adr ) != TCL_OK ) {
        return TCL_ERROR;
    }
    frame1 = (AstFrame *) adr;

    if ( Tcl_GetLongFromObj( interp, objv[2], &adr ) != TCL_OK ) {
        return TCL_ERROR;
    }
    frame2 = (AstFrame *) adr;

    /* Do the astConvert */
    mapping = (AstFrameSet *) astConvert( frame1, frame2,
                                          Tcl_GetString( objv[3] ) );

    /* Export the new object as a long containing the address */
    if ( astOK ) {
        Tcl_SetObjResult( interp, Tcl_NewLongObj( (long) mapping ) );
        return TCL_OK;
    }
    astClearStatus;
    Tcl_SetResult( interp, "Failed to convert between systems", TCL_VOLATILE );
    return TCL_ERROR;
}

/*
 * Transform a set of 2D coordinates using a FrameSet as a Mapping.
 *
 * There are three arguments, the address of a FrameSet and the two
 * coordinate values (doubles). The result is the two transformed values.
 */
static int GaiaUtilsAstTran2( ClientData clientData, Tcl_Interp *interp,
                              int objc, Tcl_Obj *CONST objv[] )
{
    AstFrameSet *mapping = NULL;
    Tcl_Obj *resultObj;
    double xin[1];
    double xout[1];
    double yin[1];
    double yout[1];
    double p[2];
    long adr;

    /* Check arguments, only allow three. */
    if ( objc != 4 ) {
        Tcl_WrongNumArgs( interp, 1, objv, "mapping pos1 pos2" );
        return TCL_ERROR;
    }

    /* Get the FrameSet */
    if ( Tcl_GetLongFromObj( interp, objv[1], &adr ) != TCL_OK ) {
        return TCL_ERROR;
    }
    mapping = (AstFrameSet *) adr;

    /* Get the position */
    if ( Tcl_GetDoubleFromObj( interp, objv[2], &xin[0] ) != TCL_OK ) {
        return TCL_ERROR;
    }
    if ( Tcl_GetDoubleFromObj( interp, objv[3], &yin[0] ) != TCL_OK ) {
        return TCL_ERROR;
    }

    /* Do the transform */
    astTran2( mapping, 1, xin, yin, 1, xout, yout );
    p[0] = xout[0];
    p[1] = yout[0];
    astNorm( mapping, p );
    if ( astOK ) {
        Tcl_ResetResult( interp );
        resultObj = Tcl_GetObjResult( interp );
        Tcl_ListObjAppendElement( interp, resultObj, Tcl_NewDoubleObj(p[0]) );
        Tcl_ListObjAppendElement( interp, resultObj, Tcl_NewDoubleObj(p[1]) );
        return TCL_OK;
    }
    astClearStatus;
    Tcl_SetResult( interp, "Failed to convert between systems", TCL_VOLATILE );
    return TCL_ERROR;
}

/*
 * Transform an nD coordinate using a FrameSet as a Mapping.
 *
 * There are three required arguments, the address of a FrameSet, whether to
 * use the forward or backward transform and a list of coordinates that define
 * the position to be transformed. Clearly there should be as many coordinates
 * as the "nin" or "nout" values of the FrameSet.
 *
 * By default the result is a list of unformatted transformed values,
 * but if the third optional argument is set to 1 then formatted values
 * will be returned.
 *
 * The result will be normalised if the forward mapping is used.
 * This can be evaded by setting the fourth optional argument to
 * false.
 */
static int GaiaUtilsAstTranN( ClientData clientData, Tcl_Interp *interp,
                              int objc, Tcl_Obj *CONST objv[] )
{
    AstFrameSet *mapping = NULL;
    Tcl_Obj **listObjv;
    Tcl_Obj *resultObj;
    const char *result;
    double in[MAX_DIMS];
    double out[MAX_DIMS];
    int formatted = 0;
    int forward = 1;
    int norm = -1;  //  Not set
    int i;
    int indims;
    int outdims;
    long adr;

    /* Check arguments, only allow three or four. */
    if ( objc < 4 || objc > 6 ) {
        Tcl_WrongNumArgs( interp, 1, objv, "mapping forward "
                          "{coord1 coord2 ...} [formatted] [normalised]" );
        return TCL_ERROR;
    }

    /* Get the FrameSet */
    if ( Tcl_GetLongFromObj( interp, objv[1], &adr ) != TCL_OK ) {
        return TCL_ERROR;
    }
    mapping = (AstFrameSet *) adr;

    /* Forward or inverse transformation */
    if ( Tcl_GetBooleanFromObj( interp, objv[2], &forward ) != TCL_OK ) {
        return TCL_ERROR;
    }

    /* Get the position */
    if ( Tcl_ListObjGetElements( interp, objv[3], &indims, &listObjv )
         != TCL_OK ) {
        return TCL_ERROR;
    }
    for ( i = 0; i < indims; i++ ) {
        if ( Tcl_GetDoubleFromObj( interp, listObjv[i], &in[i] ) != TCL_OK ) {
            return TCL_ERROR;
        }
    }

    /* Are formatted arguments required? */
    if ( objc == 5 ) {
        if ( Tcl_GetBooleanFromObj( interp, objv[4], &formatted ) != TCL_OK ) {
            return TCL_ERROR;
        }
    }

    /* Should we normalise the result. If true always, if false never,
     * if not set only for forward transformations. */
    if ( objc == 6 ) {
        if ( Tcl_GetBooleanFromObj( interp, objv[5], &norm ) != TCL_OK ) {
            return TCL_ERROR;
        }
    }
    else {
        norm = forward;
    }

    /* The output dimensions maybe be different to the input ones */
    if ( forward ) {
        outdims = astGetI( mapping, "Nout" );
    }
    else {
        outdims = astGetI( mapping, "Nin" );
    }

    /* Do the transform */
    astTranN( mapping, 1, indims, 1, in, forward, outdims, 1, out );

    /* Normalise against if this is the forward transform and not asked not
     * to, or we have been asked to normalise (default position assumes only
     * current coordinates are likely to be celestial). */
    if ( norm ) {
        astNorm( mapping, out );
    }

    if ( astOK ) {
        Tcl_ResetResult( interp );
        resultObj = Tcl_GetObjResult( interp );
        if ( formatted ) {
            for ( i = 0; i < outdims; i++ ) {
                result = astFormat( mapping, i + 1, out[i] );
                Tcl_ListObjAppendElement( interp, resultObj,
                                          Tcl_NewStringObj( result, -1 ) );
            }
        }
        else {
            for ( i = 0; i < outdims; i++ ) {
                Tcl_ListObjAppendElement( interp, resultObj,
                                          Tcl_NewDoubleObj( out[i] ) );
            }
        }
        return TCL_OK;
    }
    astClearStatus;
    Tcl_SetResult( interp, "Failed to convert nD position", TCL_VOLATILE );
    return TCL_ERROR;
}

/**
 * Return a Frame extracted from a FrameSet.
 *
 * Two arguments the FrameSet and the index of the Frame to extract.
 * The result is the address of the picked Frame. The index can be the
 * strings "current" and "base" to select AST__CURRENT and AST__BASE.
 */
static int GaiaUtilsGtFrame( ClientData clientData, Tcl_Interp *interp,
                            int objc, Tcl_Obj *CONST objv[] )
{
    AstFrame *frame;
    AstFrameSet *frameset;
    const char *cindex;
    int index;
    long adr;

    /* Check arguments, only allow two, the frameset and the index */
    if ( objc != 3 ) {
        Tcl_WrongNumArgs( interp, 1, objv, "frameset index" );
        return TCL_ERROR;
    }

    /* Get the frameset */
    if ( Tcl_GetLongFromObj( interp, objv[1], &adr ) != TCL_OK ) {
        return TCL_ERROR;
    }
    frameset = (AstFrameSet *) adr;

    /* Get the index */
    cindex = Tcl_GetString( objv[2] );
    if ( cindex[0] == 'c' ) {
        index = AST__CURRENT;
    }
    else if ( cindex[0] == 'b' ) {
        index = AST__BASE;
    }
    else {
        if ( Tcl_GetIntFromObj( interp, objv[2], &index ) != TCL_OK ) {
            return TCL_ERROR;
        }
    }

    /* Get the frame */
    frame = (AstFrame *) astGetFrame( frameset, index );

    /* Export Frame as a long containing the address */
    if ( astOK ) {
        Tcl_SetObjResult( interp, Tcl_NewLongObj( (long) frame ) );
        return TCL_OK;
    }
    Tcl_SetResult( interp, "Failed to extract frame from frameset",
                   TCL_VOLATILE );
    return TCL_ERROR;
}

/**
 * Find an AST frame in a frameset.
 *
 * There are three arguments, the FrameSet to search and the Frame to locate,
 * followed by the domainlist to constrain the search. See the astFindFrame
 * function description for how to use this.
 *
 * The result is the address of a FrameSet, if found, otherwise 0.
 */
static int GaiaUtilsAstFindFrame( ClientData clientData, Tcl_Interp *interp,
                                  int objc, Tcl_Obj *CONST objv[] )
{
    AstFrameSet *frameset = NULL;
    AstFrame *findframe = NULL;
    AstFrameSet *foundframe = NULL;
    char *domainlist;
    long adr;

    /* Check arguments, only allow three. */
    if ( objc != 4 ) {
        Tcl_WrongNumArgs( interp, 1, objv, "frameset frame domainlist" );
        return TCL_ERROR;
    }

    /* Get the FrameSet and Frame. */
    if ( Tcl_GetLongFromObj( interp, objv[1], &adr ) != TCL_OK ) {
        return TCL_ERROR;
    }
    frameset = (AstFrameSet *) adr;

    if ( Tcl_GetLongFromObj( interp, objv[2], &adr ) != TCL_OK ) {
        return TCL_ERROR;
    }
    findframe = (AstFrame *) adr;

    /* Do the find. */
    domainlist = Tcl_GetString( objv[3] );
    foundframe = (AstFrameSet *) astFindFrame( frameset, findframe,
                                               domainlist );

    /* Export the new object as a long containing the address. */
    if ( astOK ) {
        Tcl_SetObjResult( interp, Tcl_NewLongObj( (long) foundframe ) );
        return TCL_OK;
    }

    /* Otherwise return an error (NULL is a valid return). */
    astClearStatus;
    Tcl_SetResult( interp, "Failed finding a frame in a frameset",
                   TCL_VOLATILE );
    return TCL_ERROR;
}

/*
 * See if there's a linear approximation to a mapping and if so
 * return the coefficients.
 *
 * There are four arguments, the address of a Mapping/FrameSet, the
 * lower bounds and upper bounds of a region in the base coordinate system
 * to check the linear over, and a tolerance a distance in current coordinates.
 *
 * The result is a list of the coefficient values.
 */
static int GaiaUtilsAstLinearApprox( ClientData clientData, Tcl_Interp *interp,
                                     int objc, Tcl_Obj *CONST objv[] )
{
    AstFrameSet *mapping = NULL;
    Tcl_Obj **listObjv;
    Tcl_Obj *resultObj;
    double fit[MAX_DIMS*MAX_DIMS+MAX_DIMS];
    double lbnd[MAX_DIMS];
    double tol;
    double ubnd[MAX_DIMS];
    int nbnd;
    int nin;
    int nout;
    long adr;

    /* Check arguments, only allow four. */
    if ( objc != 5 ) {
        Tcl_WrongNumArgs( interp, 1, objv, "mapping {lbnd1 lbnd2 ...} "
                          "{ubnd1 ubnd2 ...} tol" );
        return TCL_ERROR;
    }

    /* Get the FrameSet */
    if ( Tcl_GetLongFromObj( interp, objv[1], &adr ) != TCL_OK ) {
        return TCL_ERROR;
    }
    mapping = (AstFrameSet *) adr;

    /* Input and output coordinates */
    nin = astGetI( mapping, "Nin" );
    nout = astGetI( mapping, "Nout" );

    /* Get the lower bounds */
    if ( Tcl_ListObjGetElements( interp, objv[2], &nbnd, &listObjv )
         != TCL_OK ) {
        return TCL_ERROR;
    }
    if ( nbnd != nin ) {
        Tcl_SetResult( interp, "Wrong no. of coordinates", TCL_VOLATILE );
        return TCL_ERROR;
    }
    for ( int i = 0; i < nbnd; i++ ) {
        if ( Tcl_GetDoubleFromObj( interp, listObjv[i], &lbnd[i] )
             != TCL_OK ) {
            return TCL_ERROR;
        }
    }

    /* Get the upper bounds */
    if ( nbnd != nin ) {
        Tcl_SetResult( interp, "Wrong no. of coordinates", TCL_VOLATILE );
        return TCL_ERROR;
    }
    if ( Tcl_ListObjGetElements( interp, objv[3], &nbnd, &listObjv )
         != TCL_OK ) {
        return TCL_ERROR;
    }
    for ( int i = 0; i < nbnd; i++ ) {
        if ( Tcl_GetDoubleFromObj( interp, listObjv[i], &ubnd[i] )
             != TCL_OK ) {
            return TCL_ERROR;
        }
    }

    /* Tolerance */
    if ( Tcl_GetDoubleFromObj( interp, objv[4], &tol ) != TCL_OK ) {
        return TCL_ERROR;
    }

    /* Do the check */
    for ( int i = 0; i < (MAX_DIMS*MAX_DIMS+MAX_DIMS); i++ ) fit[i] = AST__BAD;
    if ( astLinearApprox( mapping, lbnd, ubnd, tol, fit ) ) {
        Tcl_ResetResult( interp );
        resultObj = Tcl_GetObjResult( interp );
        for ( int i = 0; i < (nin+1)*nout; i++ ) {
            Tcl_ListObjAppendElement( interp, resultObj,
                                      Tcl_NewDoubleObj( fit[i] ) );
        }
        return TCL_OK;
    }
    astClearStatus;
    Tcl_SetResult( interp, "Failed to get a linear approximation",
                   TCL_VOLATILE );
    return TCL_ERROR;
}


/*
 * Create a new AST FrameSet or modify an existing FrameSet that represents a
 * shift in base coordinates.
 *
 * There are three arguments, the address of a FrameSet, the shifts (these are
 * in the sense of NDF sections based on GRID coordinates) followed by whether
 * the given FrameSet should be modified or a copy returned.
 */
static int GaiaUtilsShiftWcs( ClientData clientData, Tcl_Interp *interp,
                              int objc, Tcl_Obj *CONST objv[] )
{
    /* Check arguments, only allow three. */
    if ( objc != 4 ) {
        Tcl_WrongNumArgs( interp, 1, objv,
                          "frameset {shift1 shift2 ...} copy" );
        return TCL_ERROR;
    }

    /* Get the FrameSet */
    long adr;
    if ( Tcl_GetLongFromObj( interp, objv[1], &adr ) != TCL_OK ) {
        return TCL_ERROR;
    }
    AstFrameSet *frameset = (AstFrameSet *) adr;

    /* Number of base coordinates. */
    int nin = astGetI( frameset, "Nin" );

    /* Get the shifts. */
    Tcl_Obj **listObjv;
    int nvals;
    if ( Tcl_ListObjGetElements( interp, objv[2], &nvals, &listObjv )
         != TCL_OK ) {
        return TCL_ERROR;
    }
    if ( nvals != nin ) {
        Tcl_SetResult( interp, "Wrong no. of bounds", TCL_VOLATILE );
        return TCL_ERROR;
    }
    double shifts[MAX_DIMS];
    for ( int i = 0; i < nvals; i++ ) {
        if (Tcl_GetDoubleFromObj(interp, listObjv[i], &shifts[i]) != TCL_OK) {
            return TCL_ERROR;
        }

        /* To apply these in a shiftmap we need to reflect about 0. */
        shifts[i] = -shifts[i];
    }

    /* Copy or modify? */
    int copy = 1;
    if ( Tcl_GetBooleanFromObj( interp, objv[3], &copy ) != TCL_OK ) {
        return TCL_ERROR;
    }

    /* Construct a mapping using the shifts. */
    AstShiftMap *shiftMap = astShiftMap( nvals, shifts, " " );

    if ( copy ) {

        /* Create copies of the input FrameSet and remap then base frame. */
        AstFrameSet *copy = (AstFrameSet *) astCopy( frameset );
        astRemapFrame( copy, AST__BASE, shiftMap );
        (void) astAnnul( shiftMap );

        /* Export the new FrameSet. */
        if ( astOK ) {
            Tcl_SetObjResult( interp, Tcl_NewLongObj( (long) copy ) );
            return TCL_OK;
        }
        Tcl_SetResult( interp, "Failed to apply shift to frameset",
                       TCL_VOLATILE );
    }
    else {
        /* Just remap the base frame. */
        astRemapFrame( frameset, AST__BASE, shiftMap );
        (void) astAnnul( shiftMap );
        return TCL_OK;
    }
    return TCL_ERROR;
}

/*  ============
 *  GRF commands
 *  ============
 */

/*
 * Make a Tcl colour available in the AST graphics interface.
 *
 * The two input values are an index for the colour (the maximum index is 63
 * and the first 15 are protected) and the colour itself (in a form understood
 * by Tk).
 */
static int GaiaUtilsGrfAddColour( ClientData clientData, Tcl_Interp *interp,
                                  int objc, Tcl_Obj *CONST objv[] )
{
    /* Check arguments, only allow two. */
    if ( objc != 3 ) {
        Tcl_WrongNumArgs( interp, 1, objv, "index colour" );
        return TCL_ERROR;
    }

    /* Extract the index. */
    int index = 0;
    if ( Tcl_GetIntFromObj( interp, objv[1], &index ) != TCL_OK ) {
        return TCL_ERROR;
    }

    /* Make sure that default colours are established and add new one. */
    astTk_InitColours();
    astTk_AddColour( index, Tcl_GetString( objv[2] ) );
    return TCL_OK;
}

/*  ========================================
 *  STC, MOC and AST region support commands
 *  ========================================
 */

/**
 * Create an AST MOC from a FITS file.
 *
 * Accepts one argument the name of a file containing the MOC.
 * The result is the address of the new object.
 */
static int GaiaUtilsFitsMoc( ClientData clientData, Tcl_Interp *interp,
                             int objc, Tcl_Obj *CONST objv[] )
{
    const char *filename = NULL;
    AstMoc * moc = NULL;

    /* Check arguments, need  1 the filename. */
    if ( objc != 2 ) {
        Tcl_WrongNumArgs( interp, 1, objv, "FITS-file" );
        return TCL_ERROR;
    }

    /* XXX allow the specification of the HDU? */
    filename = Tcl_GetString( objv[1] );
    StarFitsIO *fitsio = GaiaFITSOpen( filename, 1 );
    if ( fitsio == NULL ) {
        Tcl_SetResult( interp, "Failed to open MOC FITS file", TCL_VOLATILE );
        return TCL_ERROR;
    }

    /* Need to find a HDU with the MOC. */
    for ( int i = 1; i <= fitsio->getNumHDUs(); i++ ) {
        const char *type = fitsio->getHDUType();
        if ( strcmp( type, "binary" ) == 0 ) {

            /* Signature is one column with some mandatory keywords .*/
            long rows = 0;
            int cols = 0;
            int status = fitsio->getTableDims( rows, cols );
            if ( status == 0 && cols == 1 ) {

                /* Look for mandatory keywords. */
                int moclen = 0;
                fitsio->get( "NAXIS2", moclen );
                int mocorder = 0;
                fitsio->get( "MOCORDER", mocorder );
                char *pixtype = fitsio->get( "PIXTYPE" );

                if ( strcmp( pixtype, "HEALPIX" ) == 0 ) {
                    char *ordering = fitsio->get( "ORDERING" );
                    if ( strcmp( ordering, "NUNIQ" ) == 0 ) {
                        char *coordsys = fitsio->get( "COORDSYS" );
                        if ( strcmp( coordsys, "C" ) == 0 ) {

                            /* We're in business. Need to read the column with
                             * the appropriate data type. */
                            int nb = 0;
                            char *tform = fitsio->get( "TFORM1" );
                            void *data;
                            if ( strcmp( tform, "1J" ) == 0 ) {
                                data = (void *) malloc( nb * moclen );
                                fitsio->getTableColumn( 1, (long *)data,
                                                        moclen );
                            } else if ( strcmp( tform, "1K" ) == 0 ) {
                                nb = 8;
                                data = (void *) malloc( nb * moclen );
                                fitsio->getTableColumn( 1, (long *)data,
                                                        moclen );
                            }

                            if ( nb > 0 ) {
                                /* Create an empty Moc. */
                                moc = astMoc( " " );

                                /* Add the column data into it. */
                                astAddMocData( moc, AST__OR, 0, mocorder,
                                               moclen, nb, data );

                                /* Free resources. */
                                free( data );

                                /* Leave the HDU loop. */
                                break;
                            }
                        }
                    }
                }
            }
        }

        /* Next HDU. */
        fitsio->setHDU( i + 1 );
    }

    /* Export the new object as a long containing the address */
    if ( astOK ) {
        Tcl_SetObjResult( interp, Tcl_NewLongObj( (long) moc ) );
        return TCL_OK;
    }
    astClearStatus;
    Tcl_SetResult( interp, "Failed to create STC region", TCL_VOLATILE );
    return TCL_ERROR;
}


/**
 * Create an AST region from an STC-S description.
 *
 * There is one argument the STC-S description for the region to create.
 * The result is the address of the new object.
 */
static int GaiaUtilsStcRegion( ClientData clientData, Tcl_Interp *interp,
                               int objc, Tcl_Obj *CONST objv[] )
{
    /* Check arguments, only allow one. */
    if ( objc != 2 ) {
        Tcl_WrongNumArgs( interp, 1, objv, "STC-S-region-description" );
        return TCL_ERROR;
    }

    /*  Create the shape by reading from a channel. */
    AstStcsChan *chan = astStcsChan( channel_source, NULL, " " );
    SOURCEInfo.next = 0;
    SOURCEInfo.lines = (const char **) malloc( sizeof( char * ) );
    SOURCEInfo.lines[0] = Tcl_GetString( objv[1] );
    SOURCEInfo.nlines = 1;
    AstRegion *region = (AstRegion *) astRead( chan );

    chan = (AstStcsChan *) astAnnul( chan );
    free( SOURCEInfo.lines );

    /* Export the new object as a long containing the address */
    if ( astOK ) {
        Tcl_SetObjResult( interp, Tcl_NewLongObj( (long) region ) );
        return TCL_OK;
    }
    astClearStatus;
    Tcl_SetResult( interp, "Failed to create STC region", TCL_VOLATILE );
    return TCL_ERROR;
}

/**
 * Get the parameterisation of a region
 *
 * There is one argument the address of an AST region. Only certain
 * region types can be parameterised, ellipse, circle etc. The parameters
 * returned are a simple list of the available values in the order shown
 * in the SUN/211 documentation.
 *
 * The exception to the above is the polygon. That has no "Pars" function
 * but the vertices can be obtained anyway, so is handled here.
 */
static int GaiaUtilsAstRegionPars( ClientData clientData, Tcl_Interp *interp,
                                   int objc, Tcl_Obj *CONST objv[] )
{
    /* Check arguments, only allow one. */
    if ( objc != 2 ) {
        Tcl_WrongNumArgs( interp, 1, objv, "AST-region" );
        return TCL_ERROR;
    }

    /* Get the Region */
    long adr;
    if ( Tcl_GetLongFromObj( interp, objv[1], &adr ) != TCL_OK ) {
        return TCL_ERROR;
    }
    AstRegion *region = (AstRegion *) adr;

    /* If a supported region return the parameters. */
    if ( astIsACircle( region ) ) {
        double centre[2];
        double radius;
        double p1[2];
        astCirclePars( (AstCircle *)region, centre, &radius, p1 );
        if ( astOK ) {
            Tcl_ResetResult( interp );
            Tcl_Obj *resultObj = Tcl_GetObjResult( interp );

            Tcl_ListObjAppendElement( interp, resultObj,
                                      Tcl_NewDoubleObj( centre[0] ) );
            Tcl_ListObjAppendElement( interp, resultObj,
                                      Tcl_NewDoubleObj( centre[1] ) );
            Tcl_ListObjAppendElement( interp, resultObj,
                                      Tcl_NewDoubleObj( radius ) );
            Tcl_ListObjAppendElement( interp, resultObj,
                                      Tcl_NewDoubleObj( p1[0] ) );
            Tcl_ListObjAppendElement( interp, resultObj,
                                      Tcl_NewDoubleObj( p1[1] ) );
            return TCL_OK;
        }
        astClearStatus;
        Tcl_SetResult( interp, "Failed to get parameters for an AST circle",
                       TCL_VOLATILE );
        return TCL_ERROR;
    }
    else if ( astIsAEllipse( region ) ) {
        double centre[2];
        double a;
        double b;
        double angle;
        double p1[2];
        double p2[2];
        astEllipsePars( (AstEllipse *)region, centre, &a, &b, &angle, p1, p2 );
        if ( astOK ) {
            Tcl_ResetResult( interp );
            Tcl_Obj *resultObj = Tcl_GetObjResult( interp );

            Tcl_ListObjAppendElement( interp, resultObj,
                                      Tcl_NewDoubleObj( centre[0] ) );
            Tcl_ListObjAppendElement( interp, resultObj,
                                      Tcl_NewDoubleObj( centre[1] ) );
            Tcl_ListObjAppendElement( interp, resultObj,
                                      Tcl_NewDoubleObj( a ) );
            Tcl_ListObjAppendElement( interp, resultObj,
                                      Tcl_NewDoubleObj( b ) );
            Tcl_ListObjAppendElement( interp, resultObj,
                                      Tcl_NewDoubleObj( angle ) );
            Tcl_ListObjAppendElement( interp, resultObj,
                                      Tcl_NewDoubleObj( p1[0] ) );
            Tcl_ListObjAppendElement( interp, resultObj,
                                      Tcl_NewDoubleObj( p1[1] ) );
            Tcl_ListObjAppendElement( interp, resultObj,
                                      Tcl_NewDoubleObj( p2[0] ) );
            Tcl_ListObjAppendElement( interp, resultObj,
                                      Tcl_NewDoubleObj( p2[1] ) );
            return TCL_OK;
        }
        astClearStatus;
        Tcl_SetResult( interp, "Failed to get parameters for an AST ellipse",
                       TCL_VOLATILE );
        return TCL_ERROR;
    }
    else if ( astIsAPolygon( region ) ) {

        int npoint;
        double *points;
        astGetRegionPoints( region, 0, 0, &npoint, points );
        if ( astOK && npoint > 0 ) {
            points = new double[npoint*2];
            astGetRegionPoints( region, npoint, 2, &npoint, points );
            if ( astOK ) {
                Tcl_ResetResult( interp );
                Tcl_Obj *resultObj = Tcl_GetObjResult( interp );
                double *x = points;
                double *y = points + npoint;
                for ( int i = 0; i < npoint; i++ ) {
                    Tcl_ListObjAppendElement( interp, resultObj,
                                              Tcl_NewDoubleObj( x[i] ) );
                    Tcl_ListObjAppendElement( interp, resultObj,
                                              Tcl_NewDoubleObj( y[i] ) );
                }
                delete[] points;
                return TCL_OK;
            }
            delete[] points;
        }
        astClearStatus;
        Tcl_SetResult( interp, "Failed to get parameters for an AST polygon",
                       TCL_VOLATILE );
        return TCL_ERROR;
    }

    astClearStatus;
    Tcl_SetResult( interp, "Unsupported AST region, cannot parameterise",
                   TCL_VOLATILE );
    return TCL_ERROR;
}

/**
 * Return the type of a given region.
 */
static int GaiaUtilsRegionType( ClientData clientData, Tcl_Interp *interp,
                                int objc, Tcl_Obj *CONST objv[] )
{
    /* Check arguments, only allow one. */
    if ( objc != 2 ) {
        Tcl_WrongNumArgs( interp, 1, objv, "AST-region" );
        return TCL_ERROR;
    }

    /* Get the Region */
    long adr;
    if ( Tcl_GetLongFromObj( interp, objv[1], &adr ) != TCL_OK ) {
        return TCL_ERROR;
    }
    AstRegion *region = (AstRegion *) adr;

    /* Check and return the type. */
    const char *result = "unsupported";
    if ( astIsABox( region ) ) {
        result = "box";
    }
    else if ( astIsACircle( region  ) ) {
        result = "circle";
    }
    else if ( astIsACmpRegion( region ) ) {
        result = "cmpregion";
    }
    else if ( astIsAEllipse( region ) ) {
        result = "ellipse";
    }
    else if ( astIsAInterval( region ) ) {
        result = "interval";
    }
    else if ( astIsANullRegion( region ) ) {
        result = "nullregion";
    }
    else if ( astIsAPolygon( region ) ) {
        result = "polygon";
    }
    else if ( astIsAPrism( region ) ) {
        result = "prism";
    }

    Tcl_SetResult( interp, const_cast< char *>(result), TCL_VOLATILE );
    return TCL_OK;
}
