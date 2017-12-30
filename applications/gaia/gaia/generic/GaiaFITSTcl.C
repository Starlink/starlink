/*+
 *  Name:
 *     GaiaFITSTcl

 *  Purpose:
 *     Simple, that's without 2D bias, access to FITS from Tcl scripts.

 *  Language:
 *     C++

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
 *     Foundation, Inc., 51 Franklin Street,Fifth Floor, Boston, MA
 *     02110-1301, USA

 *  Authors:
 *     PWD: Peter W. Draper, JAC - University of Durham

 *  History:
 *     5-APR-2006 (PWD):
 *        Original version.
 *     {enter_changes_here}
 *-
 */

#if HAVE_CONFIG_H
#include "config.h"
#endif

#include <string>
#include <stdio.h>
#include <stdlib.h>
#include <strings.h>

#include <tcl.h>
extern "C" {
#include <ast.h>
}
#include <Mem.h>
#include "StarFitsIO.h"
#include "GaiaArray.h"
#include "gaiaUtils.h"
#include "GaiaFITS.h"

#define MAX_DIMS 7
#define FITSCARD 80

/* Struct for information associated with an identifier */
struct FITSinfo {
    StarFitsIO *handle;    /* Pointer to underlying object */
    AstFrameSet *wcs;      /* Full AST frameset for WCS */
    Mem *dataPtr;          /* Data section object */
    int dims[MAX_DIMS];    /* Dimensionality of FITS data */
    int mmap;              /* Whether data section is mmapped, or copied */
    int ndims;             /* Number of dimensions of FITS data */
    void *dataCopy;        /* Copy of data, if made (mmap == 0) */
};
typedef struct FITSinfo FITSinfo;

/* Local prototypes */
static int GaiaFITSTclBounds( ClientData clientData, Tcl_Interp *interp,
                              int objc, Tcl_Obj *CONST objv[] );
static int GaiaFITSTclCGet( ClientData clientData, Tcl_Interp *interp,
                            int objc, Tcl_Obj *CONST objv[] );
static int GaiaFITSTclClose( ClientData clientData, Tcl_Interp *interp,
                             int objc, Tcl_Obj *CONST objv[] );
static int GaiaFITSTclCoord( ClientData clientData, Tcl_Interp *interp,
                             int objc, Tcl_Obj *CONST objv[] );
static int GaiaFITSTclCreate( ClientData clientData, Tcl_Interp *interp,
                              int objc, Tcl_Obj *CONST objv[] );
static int GaiaFITSTclDims( ClientData clientData, Tcl_Interp *interp,
                            int objc, Tcl_Obj *CONST objv[] );
static int GaiaFITSTclFitsHeaders( ClientData clientData, Tcl_Interp *interp,
                                   int objc, Tcl_Obj *CONST objv[] );
static int GaiaFITSTclFitsRead( ClientData clientData, Tcl_Interp *interp,
                                int objc, Tcl_Obj *CONST objv[] );
static int GaiaFITSTclFitsWrite( ClientData clientData, Tcl_Interp *interp,
                                 int objc, Tcl_Obj *CONST objv[] );
static int GaiaFITSTclExists( ClientData clientData, Tcl_Interp *interp,
                              int objc, Tcl_Obj *CONST objv[] );
static int GaiaFITSTclExtensionExists( ClientData clientData,
                                       Tcl_Interp *interp,
                                       int objc, Tcl_Obj *CONST objv[] );
static int GaiaFITSTclGetProperty( ClientData clientData, Tcl_Interp *interp,
                                   int objc, Tcl_Obj *CONST objv[] );
static int GaiaFITSTclGtWcs( ClientData clientData, Tcl_Interp *interp,
                             int objc, Tcl_Obj *CONST objv[] );
static int GaiaFITSTclHdu( ClientData clientData, Tcl_Interp *interp,
                           int objc, Tcl_Obj *CONST objv[] );
static int GaiaFITSTclMap( ClientData clientData, Tcl_Interp *interp,
                           int objc, Tcl_Obj *CONST objv[] );
static int GaiaFITSTclOpen( ClientData clientData, Tcl_Interp *interp,
                            int objc, Tcl_Obj *CONST objv[] );
static int GaiaFITSTclParseCard( ClientData clientData, Tcl_Interp *interp,
                                 int objc, Tcl_Obj *CONST objv[] );
static int GaiaFITSTclUnMap( ClientData clientData, Tcl_Interp *interp,
                             int objc, Tcl_Obj *CONST objv[] );

static int GetFITSDimsOrBounds( Tcl_Interp *interp, int objc,
                                Tcl_Obj *CONST objv[], int bounds );
static int importFITSIdentifier( Tcl_Interp *interp, Tcl_Obj *obj,
                                 FITSinfo **info );
static long exportFITSHandle( StarFitsIO *handle );

/**
 * Register all the FITS access commands.
 */
int Fits_Init( Tcl_Interp *interp )
{
    Tcl_CreateObjCommand( interp, "fits::close", GaiaFITSTclClose,
                          (ClientData) NULL,
                          (Tcl_CmdDeleteProc *) NULL );

    Tcl_CreateObjCommand( interp, "fits::create", GaiaFITSTclCreate,
                          (ClientData) NULL,
                          (Tcl_CmdDeleteProc *) NULL );

    Tcl_CreateObjCommand( interp, "fits::exists", GaiaFITSTclExists,
                          (ClientData) NULL,
                          (Tcl_CmdDeleteProc *) NULL );

    Tcl_CreateObjCommand( interp, "fits::extensionexists",
                          GaiaFITSTclExtensionExists,
                          (ClientData) NULL,
                          (Tcl_CmdDeleteProc *) NULL );

    Tcl_CreateObjCommand( interp, "fits::fitsheaders", GaiaFITSTclFitsHeaders,
                          (ClientData) NULL,
                          (Tcl_CmdDeleteProc *) NULL );

    Tcl_CreateObjCommand( interp, "fits::fitsread", GaiaFITSTclFitsRead,
                          (ClientData) NULL,
                          (Tcl_CmdDeleteProc *) NULL );

    Tcl_CreateObjCommand( interp, "fits::fitswrite", GaiaFITSTclFitsWrite,
                          (ClientData) NULL,
                          (Tcl_CmdDeleteProc *) NULL );

    Tcl_CreateObjCommand( interp, "fits::getbounds", GaiaFITSTclBounds,
                          (ClientData) NULL,
                          (Tcl_CmdDeleteProc *) NULL );

    Tcl_CreateObjCommand( interp, "fits::getc", GaiaFITSTclCGet,
                          (ClientData) NULL,
                          (Tcl_CmdDeleteProc *) NULL );

    Tcl_CreateObjCommand( interp, "fits::getcoord", GaiaFITSTclCoord,
                          (ClientData) NULL,
                          (Tcl_CmdDeleteProc *) NULL );

    Tcl_CreateObjCommand( interp, "fits::getdims", GaiaFITSTclDims,
                          (ClientData) NULL,
                          (Tcl_CmdDeleteProc *) NULL );

    Tcl_CreateObjCommand( interp, "fits::getproperty", GaiaFITSTclGetProperty,
                          (ClientData) NULL,
                          (Tcl_CmdDeleteProc *) NULL );

    Tcl_CreateObjCommand( interp, "fits::getwcs", GaiaFITSTclGtWcs,
                          (ClientData) NULL,
                          (Tcl_CmdDeleteProc *) NULL );

    Tcl_CreateObjCommand( interp, "fits::hdu", GaiaFITSTclHdu,
                          (ClientData) NULL,
                          (Tcl_CmdDeleteProc *) NULL );

    Tcl_CreateObjCommand( interp, "fits::map", GaiaFITSTclMap,
                          (ClientData) NULL,
                          (Tcl_CmdDeleteProc *) NULL );

    Tcl_CreateObjCommand( interp, "fits::open", GaiaFITSTclOpen,
                          (ClientData) NULL,
                          (Tcl_CmdDeleteProc *) NULL );

    Tcl_CreateObjCommand( interp, "fits::parsecard", GaiaFITSTclParseCard,
                          (ClientData) NULL,
                          (Tcl_CmdDeleteProc *) NULL );

    Tcl_CreateObjCommand( interp, "fits::unmap", GaiaFITSTclUnMap,
                          (ClientData) NULL,
                          (Tcl_CmdDeleteProc *) NULL );

    return TCL_OK;
}

/**
 * Import a FITS identifier and return the info struct.
 */
static int importFITSIdentifier( Tcl_Interp *interp, Tcl_Obj *obj,
                                 FITSinfo **info )
{
    Tcl_Obj *resultObj;
    long adr;

    if ( Tcl_GetLongFromObj( interp, obj, &adr ) != TCL_OK ) {

        /* Replace result with our message */
        resultObj = Tcl_GetObjResult( interp );
        Tcl_SetStringObj( resultObj, Tcl_GetString( obj ), -1 );
        Tcl_AppendStringsToObj( resultObj, " : not a FITS identifier" ,
                                (char *)NULL );
        return TCL_ERROR;
    }

    /* Cast to a local FITSinfo struct */
    *info = (FITSinfo *) adr;
    return TCL_OK;
}

/**
 * Export a FITS handle and return an identifier.
 */
static long exportFITSHandle( StarFitsIO *handle )
{
    FITSinfo *info = (FITSinfo *) malloc( sizeof( FITSinfo ) );

    info->handle = handle;
    info->wcs = NULL;
    info->mmap = 1;
    info->dataCopy = NULL;
    info->ndims = 0;

    return (long) info;
}

/**
 * Open a FITS data unit. The result is an identifier.
 */
static int GaiaFITSTclOpen( ClientData clientData, Tcl_Interp *interp,
                            int objc, Tcl_Obj *CONST objv[] )
{
    Tcl_Obj *resultObj;
    char name[1024];
    char *spec;
    int hdu;
    StarFitsIO *handle;

    /* Check arguments, only allow one, the FITS specification. */
    if ( objc != 2 ) {
        Tcl_WrongNumArgs( interp, 1, objv, "fits_specification" );
        return TCL_ERROR;
    }

    /* Get the FITS specification. */
    spec = Tcl_GetString( objv[1] );

    /* Parse into filename and possible HDU number. */
    GaiaFITSParseName( spec, name, 1024, &hdu );

    /* And access it */
    resultObj = Tcl_GetObjResult( interp );
    handle = (StarFitsIO *) GaiaFITSOpen( name, hdu );
    if ( handle != NULL ) {
        Tcl_SetLongObj( resultObj, exportFITSHandle( handle ) );
        return TCL_OK;
    }
    Tcl_SetResult( interp, "Failed to open FITS file", TCL_VOLATILE );
    return TCL_ERROR;
}

/**
 * Close the FITS file.
 */
static int GaiaFITSTclClose( ClientData clientData, Tcl_Interp *interp,
                             int objc, Tcl_Obj *CONST objv[] )
{
    FITSinfo *info;

    /* Check arguments, only allow one, the FITS identifier */
    if ( objc != 2 ) {
        Tcl_WrongNumArgs( interp, 1, objv, "fits_identifier" );
        return TCL_ERROR;
    }

    /* Import the identifier */
    if ( importFITSIdentifier( interp, objv[1], &info ) != TCL_OK ) {
        return TCL_ERROR;
    }
    GaiaFITSClose( info->handle );
    free( (void *) info );

    return TCL_OK;
}

/**
 * Map the FITS data array in the native type, using file mapping as
 * requested. The result is a memory address (long int) of an ARRAYinfo
 * struct. Note the data is raw and no byte swapping will be applied.
 *
 * The access mode is currently ignored.
 */
static int GaiaFITSTclMap( ClientData clientData, Tcl_Interp *interp,
                           int objc, Tcl_Obj *CONST objv[] )
{
    ARRAYinfo *arrayInfo;
    FITSinfo *info;
    double bscale;
    double bzero;
    int blank;
    int haveblank;
    int type;
    size_t el;

    /* Check arguments, allow three, the FITS identifier, whether to use file
     * mapping and the access mode, the optional component value is ignored
     * and "DATA" is always used. */
    if ( objc != 4 && objc != 5 ) {
        Tcl_WrongNumArgs( interp, 1, objv,
                          "fits_identifier ?usemmap? access [\"DATA\"]" );
        return TCL_ERROR;
    }

    /* Import the identifier */
    if ( importFITSIdentifier( interp, objv[1], &info ) != TCL_OK ) {
        return TCL_ERROR;
    }

    /* Mmap mode. */
    if ( Tcl_GetBooleanFromObj( interp, objv[2], &info->mmap ) != TCL_OK ) {
        return TCL_ERROR;
    }
    type = gaiaArrayFITSType( info->handle->bitpix() );

    /* Get the data array Mem object */
    if ( GaiaFITSDataMem( info->handle, &info->dataPtr ) == TCL_OK ) {

        /* Need the size of the data area (not the mapped one). */
        if ( info->ndims == 0 ) {
            GaiaFITSGtWcs( info->handle, &info->wcs, info->dims,
                           &info->ndims );
        }

        /* Make a memory copy if necessary. */
        void *ptr = info->dataPtr->ptr();
        el = 1;
        for ( int i = 0; i < info->ndims; i++ ) {
            el *= info->dims[i];
        }
        if ( ! info->mmap ) {
            info->dataCopy = malloc( el * info->handle->pixelSize() );
            memcpy( info->dataCopy, ptr, el * info->handle->pixelSize() );
            ptr = info->dataCopy;
        }

        /* Get the BLANK value, if set */
        if ( GaiaFITSHGet( info->handle, "BLANK", &blank ) == TCL_OK ) {
            haveblank = 1;
        }
        else {
            haveblank = 0;
            blank = 0;
        }

        /* Similarly for BSCALE and BZERO */
        bscale = 1.0;
        bzero = 0.0;
        if ( GaiaFITSHGet( info->handle, "BSCALE", &bscale ) != TCL_OK ) {
            bscale = 1.0;
        }
        if ( GaiaFITSHGet( info->handle, "BZERO", &bzero ) != TCL_OK ) {
            bzero = 0.0;
        }

        /* Construct result */
        arrayInfo = gaiaArrayCreateInfo( ptr, type, el, 1, haveblank, blank,
                                         bscale, bzero, GAIA_ARRAY_NONE );
        Tcl_SetObjResult( interp, Tcl_NewLongObj( (long)arrayInfo ) );
        return TCL_OK;
    }

    Tcl_SetResult( interp, "Failed to map FITS data", TCL_VOLATILE );
    return TCL_ERROR;
}

/**
 * Unmap the data array, associated with a handle.
 */
static int GaiaFITSTclUnMap( ClientData clientData, Tcl_Interp *interp,
                             int objc, Tcl_Obj *CONST objv[] )
{
    FITSinfo *info;
    long adr;

    /* Check arguments, need, the FITS identifier, the "component" and
     * ARRAYinfo memory address, for FITS data the component is always DATA,
     * so any actual value is ignored (like this to match the API of the
     * equivalent NDF functions). */
    if ( objc != 4 ) {
        Tcl_WrongNumArgs( interp, 1, objv,
                          "fits_identifier component address" );
        return TCL_ERROR;
    }

    /* Import the identifier */
    if ( importFITSIdentifier( interp, objv[1], &info ) != TCL_OK ) {
        return TCL_ERROR;
    }

    /* Unmap */
    GaiaFITSUnmap( info->dataPtr );

    /* Free the copy, if made */
    if ( ! info->mmap ) {
        if ( info->dataCopy != NULL ) {
            free( info->dataCopy );
            info->dataCopy = NULL;
        }
    }

    /* Free the ARRAYinfo struct */
    if ( Tcl_GetLongFromObj( interp, objv[3], &adr ) == TCL_OK ) {
        gaiaArrayFreeInfo( (ARRAYinfo *)adr );
    }

    return TCL_OK;
}

/**
 * Return the address of the FITS WCS component. This is an AST FrameSet.
 */
static int GaiaFITSTclGtWcs( ClientData clientData, Tcl_Interp *interp,
                             int objc, Tcl_Obj *CONST objv[] )
{
    FITSinfo *info;
    int result;

    /* Check arguments, only allow one the fits identifier */
    if ( objc != 2 ) {
        Tcl_WrongNumArgs( interp, 1, objv, "fits_identifier" );
        return TCL_ERROR;
    }

    /* Import the identifier */
    if ( importFITSIdentifier( interp, objv[1], &info ) != TCL_OK ) {
        return TCL_ERROR;
    }

    /* Get the full WCS, if not already done. */
    result = TCL_OK;
    if ( info->wcs == NULL ) {
        result = GaiaFITSGtWcs( info->handle, &info->wcs, info->dims,
                                &info->ndims );
    }

    /* Export the WCS as a long containing the address */
    if ( result == TCL_OK ) {
        Tcl_SetObjResult( interp, Tcl_NewLongObj( (long) info->wcs ) );
    }
    else {
        Tcl_SetResult( interp, "Failed to read FITS WCS", TCL_VOLATILE );
    }
    return result;
}

/**
 * Return the value of a FITS header card.
 */
static int GaiaFITSTclCGet( ClientData clientData, Tcl_Interp *interp,
                            int objc, Tcl_Obj *CONST objv[] )
{
    FITSinfo *info;
    Tcl_Obj *resultObj;
    char value[FITSCARD];
    int result;

    /* Check arguments, need 2 the fits identifier and the header key */
    if ( objc != 3 ) {
        Tcl_WrongNumArgs( interp, 1, objv, "fits_identifier header_key" );
        return TCL_ERROR;
    }

    /* Import the identifier */
    result = importFITSIdentifier( interp, objv[1], &info );
    if ( result == TCL_OK ) {
        resultObj = Tcl_GetObjResult( interp );
        value[0] = '\0';

        /* We test for some special keywords that are used by the NDF
         * library. These are "units", "label" and "title". Transform these
         * into the obvious FITS equivalents. */
        char *keyword = Tcl_GetString( objv[2] );
        if ( strcasecmp( keyword, "units" ) == 0 ) {
            keyword = "BUNIT";
        }
        else if ( strcasecmp( keyword, "label" ) == 0 ) {
            keyword = "OBJECT";
        }
        else if ( strcasecmp( keyword, "title" ) == 0 ) {
            keyword = "OBJECT";
        }
        result = GaiaFITSHGet( info->handle, keyword, value, FITSCARD );
        if ( result == TCL_OK ) {
            Tcl_SetStringObj( resultObj, value, -1 );
        }
        else {
            Tcl_SetResult( interp, "Failed to read FITS header value",
                           TCL_VOLATILE );
        }
    }
    return result;
}

/**
 * Return if a "component" exists. FITS only supports "DATA" and
 * "EXTENSION", which are always present.
 */
static int GaiaFITSTclExists( ClientData clientData, Tcl_Interp *interp,
                              int objc, Tcl_Obj *CONST objv[] )
{
    FITSinfo *info;
    const char *component;
    int result;

    /* Check arguments, need 2 the fits identifier and the component */
    if ( objc != 3 ) {
        Tcl_WrongNumArgs( interp, 1, objv, "fits_identifier component" );
        return TCL_ERROR;
    }

    /* Import the identifier */
    result = importFITSIdentifier( interp, objv[1], &info );
    if ( result == TCL_OK ) {
        component = Tcl_GetString( objv[2] );

        /* Check for known extensions */
        if ( strcasecmp( "DATA", component )      == 0 ||
             strcasecmp( "EXTENSION", component ) == 0 ) {
            Tcl_SetObjResult( interp, Tcl_NewBooleanObj( 1 ) );
        }
        else {
            Tcl_SetObjResult( interp, Tcl_NewBooleanObj( 0 ) );
        }
    }
    return result;
}

/**
 * Get the dimensions of the FITS data. Returns a list of values, one for
 * each dimension.
 */
static int GaiaFITSTclDims( ClientData clientData, Tcl_Interp *interp,
                            int objc, Tcl_Obj *CONST objv[] )
{
    return GetFITSDimsOrBounds( interp, objc, objv, 0 );
}

/**
 * Get the bounds of the FITS data. Returns a list of lower and upper pixel
 * indices, which for FITS are 1 to the dimension size.
 */
static int GaiaFITSTclBounds( ClientData clientData, Tcl_Interp *interp,
                              int objc, Tcl_Obj *CONST objv[] )
{
    return GetFITSDimsOrBounds( interp, objc, objv, 1 );
}

/**
 * Get the dimensions of the FITS data. Either return the result as a list
 * of dimensions or bounds.
 */
static int GetFITSDimsOrBounds( Tcl_Interp *interp, int objc,
                                Tcl_Obj *CONST objv[], int bounds )
{
    FITSinfo *info;
    Tcl_Obj *resultObj;
    int i;
    int result;

    /* Check arguments, need the fits identifier and whether to trim redundant
     * axes. */
    if ( objc != 2 && objc != 3 ) {
        Tcl_WrongNumArgs( interp, 1, objv, "fits_identifier [?trunc?]" );
        return TCL_ERROR;
    }

    /* Import the FITS identifier */
    result = importFITSIdentifier( interp, objv[1], &info );
    if ( result == TCL_OK ) {
        resultObj = Tcl_GetObjResult( interp );

        /* Real dimensions are extracted from the FITS headers */
        if ( info->ndims == 0 ) {
            result = GaiaFITSGtWcs( info->handle, &info->wcs, info->dims,
                                    &info->ndims );
        }
        if ( result == TCL_OK ) {
            Tcl_ResetResult( interp );

            /* Need to truncate? */
            int trunc = 0;
            if ( objc == 3 ) {
                Tcl_GetBooleanFromObj( interp, objv[2], &trunc );
            }
            int ndim = info->ndims;
            if ( trunc ) {
                /* Remove any trailing dimensions of size 1, note always keep
                 * at least one. */
                for ( i = ndim - 1; i > 0; i-- ) {
                    if ( info->dims[i] != 1 ) {
                        ndim = i + 1;
                        break;
                    }
                }
            }
            for ( i = 0; i < ndim; i++ ) {
                if ( bounds ) {
                    Tcl_ListObjAppendElement( interp, resultObj,
                                              Tcl_NewIntObj( 1 ) );
                }
                Tcl_ListObjAppendElement( interp, resultObj,
                                          Tcl_NewIntObj( info->dims[i] ) );
            }
        }
        if ( result != TCL_OK ) {
            Tcl_SetResult( interp, "Failed to determine FITS dimensions",
                           TCL_VOLATILE );
        }
    }
    return result;
}

/**
 * Return the coordinate of a position along a given axis.
 *
 * The second argument is the FITS identifier, the third the index of the
 * axis, and the fourth a list of all the pixel indices needed to identify the
 * coordinate (so the list must have a number for each dimension).  The fifth
 * argument defines whether to format the result (using astFormat), otherwise
 * it is returned as a double. The two arguments are booleans used to switch
 * on the addition of label and units strings, either as simple trailing
 * values, or as a more readable string.
 */
static int GaiaFITSTclCoord( ClientData clientData, Tcl_Interp *interp,
                             int objc, Tcl_Obj *CONST objv[] )
{
    FITSinfo *info;
    Tcl_Obj **listObjv;
    Tcl_Obj *resultObj;
    char *coord;
    char *error_mess;
    double coords[MAX_DIMS];
    int axis;
    int format;
    int ncoords;
    int readable;
    int result;
    int trailed;

    /* Check arguments */
    if ( objc != 7 ) {
        Tcl_WrongNumArgs( interp, 1, objv, "fits_identifier axis \
                          {c1 c2 .. cn} ?format? ?trail_units? ?readable?" );
        return TCL_ERROR;
    }

    /* Import FITS identifier */
    result = importFITSIdentifier( interp, objv[1], &info );
    if ( result == TCL_OK ) {

        /* Next arguments are the axis that the coordinate is required for
         * followed by a list of all the pixel indices that specify the
         * position. */
        if ( Tcl_GetIntFromObj( interp, objv[2], &axis ) == TCL_OK ) {
            if ( Tcl_ListObjGetElements( interp, objv[3], &ncoords, &listObjv )
                 == TCL_OK ) {
                for ( int i = 0; i < ncoords; i++ ) {
                    if ( Tcl_GetDoubleFromObj( interp, listObjv[i],
                                               &coords[i] ) != TCL_OK ) {
                        return TCL_ERROR;
                    }

                    /* These are "pixel indices", so we need to convert to
                     * FITS grid values. */
                    coords[i] += 0.5;
                }

                /* Whether to format value. */
                format = 1;
                if ( Tcl_GetBooleanFromObj( interp, objv[4], &format ) != TCL_OK ) {
                    return TCL_ERROR;
                }

                /* Whether to include the label and units. */
                trailed = 0;
                if ( Tcl_GetBooleanFromObj( interp, objv[5], &trailed ) != TCL_OK ) {
                    return TCL_ERROR;
                }

                /* Whether to format inclusion of label and units. */
                readable = 0;
                if ( Tcl_GetBooleanFromObj( interp, objv[6], &readable ) != TCL_OK ) {
                    return TCL_ERROR;
                }

                resultObj = Tcl_GetObjResult( interp );
                if ( result != TCL_ERROR ) {
                    if ( info->wcs == NULL ) {
                        result = GaiaFITSGtWcs( info->handle, &info->wcs,
                                                info->dims, &info->ndims );
                    }
                    /* Do the transformation */
                    result = gaiaUtilsQueryCoord( info->wcs, axis, coords,
                                                  trailed, readable, format,
                                                  ncoords, &coord,
                                                  &error_mess );
                    if ( result == TCL_OK ) {
                        Tcl_SetStringObj( resultObj, coord, -1 );
                    }
                    else {
                        free( error_mess );
                        Tcl_SetResult( interp, "Failed to determine axis"
                                       " coordinate", TCL_VOLATILE );
                    }
                }
            }
            else {
                result = TCL_ERROR;
            }
        }
        else {
            result = TCL_ERROR;
        }
    }
    return result;
}

/**
 * Create a FITS file and write to the primary extension. The result is a FITS
 * file.
 *
 * Requires the name of the FITS file, a pointer to an ARRAYinfo data struct,
 * the dimensions (a list) of the array, a WCS, a name (OBJECT) and units for
 * the data (can be blank).
 */
static int GaiaFITSTclCreate( ClientData clientData, Tcl_Interp *interp,
                              int objc, Tcl_Obj *CONST objv[] )
{
    ARRAYinfo *info;
    AstFrameSet *wcs;
    Tcl_Obj **listObjv;
    char *name;
    const char *object;
    const char *units;
    int bitpix;
    int i;
    int ndims;
    long adr;
    long dims[MAX_DIMS];

    /* Check arguments. */
    if ( objc != 7 ) {
        Tcl_WrongNumArgs( interp, 1, objv, "fitsfile data_array dims "
                          "frameset object_name units" );
        return TCL_ERROR;
    }

    /* Get name */
    name = Tcl_GetString( objv[1] );

    /* Get the data, this is an ARRAYinfo struct */
    if ( Tcl_GetLongFromObj( interp, objv[2], &adr ) != TCL_OK ) {
        Tcl_AppendResult( interp, ": failed to read data pointer",
                          (char *) NULL );
        return TCL_ERROR;
    }
    info = (ARRAYinfo *) adr;

    /* Convert type into a bitpix. */
    bitpix = gaiaArrayFITSBitpix( info->type );

    /* Dimensionality */
    if ( Tcl_ListObjGetElements( interp, objv[3], &ndims, &listObjv )
         != TCL_OK ) {
        return TCL_ERROR;
    }
    for ( i = 0; i < ndims; i++ ) {
        if ( Tcl_GetLongFromObj( interp, listObjv[i], &dims[i] ) != TCL_OK ) {
            return TCL_ERROR;
        }
    }

    /* Get the WCS */
    wcs = NULL;
    if ( Tcl_GetLongFromObj( interp, objv[4], &adr ) != TCL_OK ) {
        return TCL_ERROR;
    }
    wcs = (AstFrameSet *) adr;

    /* Object name and units */
    object = Tcl_GetString( objv[5] );
    units = Tcl_GetString( objv[6] );

    return GaiaFITSCreate( name, info->ptr, wcs, bitpix, info->bscale,
                           info->bzero, info->blank, object, units,
                           ndims, dims );
}

/**
 * Return the FITS headers as a string. Each card is separated by a newline.
 */
static int GaiaFITSTclFitsHeaders( ClientData clientData, Tcl_Interp *interp,
                                   int objc, Tcl_Obj *CONST objv[] )
{
    FITSinfo *info;
    Mem mem;
    char *headerPtr;
    char *headers;
    char *ptr;
    int i;
    int headerSize;
    int ncards;
    int result;

    /* Check arguments, need the FITS identifier */
    if ( objc != 2 ) {
        Tcl_WrongNumArgs( interp, 1, objv, "fits_identifier" );
        return TCL_ERROR;
    }

    /* Import FITS identifier */
    result = importFITSIdentifier( interp, objv[1], &info );
    if ( result == TCL_OK ) {

        /*  Access the FITS headers */
        mem = info->handle->header();
        headerPtr = (char *) mem.ptr();
        headerSize = mem.size();

        /* Loop over all cards, append each one to the result */
        ncards = headerSize / 80;
        headers = (char *) ckalloc( ncards * 81 );
        ptr = headers;
        for ( i = 0; i < ncards; i++ ) {
            memcpy( ptr, headerPtr, 80 );
            ptr[80] = '\n';
            if ( strncmp( ptr, "END     ", 8 ) == 0 ) {
                break;
            }
            ptr += 81;
            headerPtr += 80;
        }
        ptr = 0;
        Tcl_SetResult( interp, headers, TCL_DYNAMIC );
    }
    return result;
}


/**
 * Return the value of a FITS keyword. If not found an empty string
 * is returned.
 */
static int GaiaFITSTclFitsRead( ClientData clientData, Tcl_Interp *interp,
                                int objc, Tcl_Obj *CONST objv[] )
{
    FITSinfo *info;
    const char *keyword;
    char value[81];
    int result;

    /* Check arguments, need the fits handle and the keyword */
    if ( objc != 3 ) {
        Tcl_WrongNumArgs( interp, 1, objv, "fits_identifier keyword" );
        return TCL_ERROR;
    }

    /* Import FITS identifier */
    result = importFITSIdentifier( interp, objv[1], &info );
    if ( result == TCL_OK ) {
        keyword = Tcl_GetString( objv[2] );
        result = GaiaFITSHGet( info->handle, keyword, value, 80 );
        value[80] = '\0';
        if ( result == TCL_OK ) {
            Tcl_SetResult( interp, value, TCL_VOLATILE );
        }
        else {
            Tcl_ResetResult( interp );
            Tcl_SetResult( interp, "", TCL_VOLATILE );
        }
    }
    return result;
}

/**
 * Write a FITS card, either as a keyword, value, comment and type or
 * as a whole card. The type should be "char" or "numeric". If numeric the
 * value is written without quotes.
 */
static int GaiaFITSTclFitsWrite( ClientData clientData, Tcl_Interp *interp,
                                 int objc, Tcl_Obj *CONST objv[] )
{
    FITSinfo *info;
    const char *comment;
    const char *keyword;
    const char *type;
    const char *value;
    int result;

    /* Check arguments, need the fits handle and keyword, value and comment,
    *  or just card */
    if ( objc != 3 && objc != 6 ) {
        Tcl_WrongNumArgs( interp, 1, objv,
           "fits_identifier [keyword value comment type | card]" );
        return TCL_ERROR;
    }

    /* Import FITS identifier */
    result = importFITSIdentifier( interp, objv[1], &info );
    if ( result == TCL_OK ) {

        /* Write kvc or card */
        if ( objc == 6 ) {
            keyword = Tcl_GetString( objv[2] );
            value = Tcl_GetString( objv[3] );
            comment = Tcl_GetString( objv[4] );
            type = Tcl_GetString( objv[5] );
            result = GaiaFITSHPut( info->handle, keyword, value, comment,
                                   type );
        }
        else {
            result = GaiaFITSHPutCard( info->handle, Tcl_GetString( objv[2]) );
        }
    }
    return result;
}

/**
 * Return if a named extension exists. FITS only supports "FITS",
 * which is always present.
 */
static int GaiaFITSTclExtensionExists( ClientData clientData,
                                       Tcl_Interp *interp,
                                       int objc, Tcl_Obj *CONST objv[] )
{
    FITSinfo *info;
    const char *component;
    int result;

    /* Check arguments, need 2 the fits identifier and the extension */
    if ( objc != 3 ) {
        Tcl_WrongNumArgs( interp, 1, objv, "fits_identifier FITS" );
        return TCL_ERROR;
    }

    /* Import the identifier */
    result = importFITSIdentifier( interp, objv[1], &info );
    if ( result == TCL_OK ) {
        component = Tcl_GetString( objv[2] );

        /* Check for known extensions */
        if ( strcasecmp( "FITS", component )      == 0 ) {
            Tcl_SetObjResult( interp, Tcl_NewBooleanObj( 1 ) );
        }
        else {
            Tcl_SetObjResult( interp, Tcl_NewBooleanObj( 0 ) );
        }
    }
    return result;
}

/**
 * Return the value of a named property in an extension. For a FITS file
 * the only supported extension is "FITS", so this call is equivalent to
 * ::fitsread.
 *
 * If the property doesn't exist then an empty string is returned.
 */
static int GaiaFITSTclGetProperty( ClientData clientData, Tcl_Interp *interp,
                                   int objc, Tcl_Obj *CONST objv[] )
{
    Tcl_Obj *newobjv[3];

    /* Check arguments, need the fits handle, extension and property name */
    if ( objc != 4 ) {
        Tcl_WrongNumArgs( interp, 1, objv,
                          "fits_identifier \"FITS\" keyword" );
        return TCL_ERROR;
    }

    if ( strcmp( "FITS", Tcl_GetString( objv[2] ) ) == 0 ) {
        newobjv[0] = objv[0];
        newobjv[1] = objv[1];
        newobjv[2] = objv[3];
        return GaiaFITSTclFitsRead( clientData, interp, 3, newobjv );
    }

    /* Unknown, so return an empty string */
    Tcl_SetResult( interp, "", TCL_VOLATILE );
    return TCL_OK;
}

/**
 * Parse a given FITS header card into it keyword, value and comment
 * parts. Returns the card as a list of values.
 */
static int GaiaFITSTclParseCard( ClientData clientData, Tcl_Interp *interp,
                                 int objc, Tcl_Obj *CONST objv[] )
{
    Tcl_Obj *resultObj;
    char *c;
    char *k;
    char *v;
    char comment[FITSCARD+1];
    char keyword[FITSCARD+1];
    char value[FITSCARD+1];
    const char *card;
    int status = 0;

    /* Check arguments, need the card */
    if ( objc != 2 ) {
        Tcl_WrongNumArgs( interp, 1, objv, "FITS_card" );
        return TCL_ERROR;
    }
    card = Tcl_GetString( objv[1] );

    /* Get keyword, just the characters to the first space or equals sign. */
    c = (char *) card;
    k = keyword;
    while ( *c && *c != '=' && *c != ' ' ) {
        *k = *c;
        c++;
        k++;
    }
    *k = '\0';

    /* Do parsing to get value and comment, handles quoting. */
    if ( fits_parse_value( (char *) card, value, comment, &status ) == 0 ) {

        /* Remove quoting from value, if present. */
        v = value;
        if ( *v == '\'' ) {
            v++;
            c = value + strlen( value );
            while ( *c && *c != '\'' ) c--;
            *c = '\0';
        }

        /*  Construct result */
        resultObj = Tcl_GetObjResult( interp );
        Tcl_ListObjAppendElement( interp, resultObj,
                                  Tcl_NewStringObj( keyword, -1 ) );
        Tcl_ListObjAppendElement( interp, resultObj,
                                  Tcl_NewStringObj( v, -1 ) );
        Tcl_ListObjAppendElement( interp, resultObj,
                                  Tcl_NewStringObj( comment, -1 ) );
        return TCL_OK;
    }
    Tcl_SetResult( interp, "Error reading FITS card", TCL_VOLATILE );
    return TCL_ERROR;
}


/**
 * Commands for querying the FITS HDUs of the current file.
 *
 * "list"             returns a list of all the HDU properties.
 * "listheadings"     returns a list of headings for the returned properties.
 * "get <n> filename" save the nth HDU to the given diskfile, in fact this
 *                    currently only saves compressed images. An optional
 *                    argument if given will be set as the OBJECT card.
 *
 * The headings are: HDU Type ExtName NAXIS NAXIS1 NAXIS2 NAXIS3 CRPIX1 CRPIX2,
 * as returned by the rtdimage command of the same name.
 */
static int GaiaFITSTclHdu( ClientData clientData, Tcl_Interp *interp,
                           int objc, Tcl_Obj *CONST objv[] )
{
    FITSinfo *info;
    int result;

    /* Check arguments, need the fits handle and the hdu command. */
    if ( objc < 3 || objc > 6 ) {
        Tcl_WrongNumArgs( interp, 1, objv, "fits_identifier "
                          "[list|listheadings|get hdu filename]" );
        return TCL_ERROR;
    }

    /* Import FITS identifier */
    result = importFITSIdentifier( interp, objv[1], &info );
    if ( result != TCL_OK ) {
        return TCL_ERROR;
    }

    //  Return current HDU number. Note error returns 1.
    if ( objc == 2 ) {
        int nhdu = info->handle->getHDUNum();
        Tcl_SetObjResult( interp, Tcl_NewIntObj( nhdu ) );
        return TCL_OK;
    }

    const char *action = Tcl_GetString( objv[2] );

    //  hdu listheadings.
    if ( strcmp( action, "listheadings" ) == 0 ) {
        Tcl_SetResult( interp, "HDU Type ExtName NAXIS NAXIS1 "
                       "NAXIS2 NAXIS3 CRPIX1 CRPIX2", TCL_VOLATILE );
        return TCL_OK;
    }

    //  hdu list.
    if ( strcmp( action, "list" ) == 0 ) {
        string list;
        if( GaiaFITSHduList( info->handle, list ) == TCL_OK ) {
            Tcl_SetResult( interp, (char *) list.c_str(), TCL_VOLATILE );
            return TCL_OK;
        }
        Tcl_SetResult( interp, "Error list FITS file extensions",
                       TCL_VOLATILE );
        return TCL_ERROR;
    }

    //  hdu get <n> filename [objectname]. Saves only compressed images...
    if ( strcmp( action, "get" ) == 0 ) {
        if ( objc != 5 && objc != 6 ) {
            Tcl_SetResult( interp,
                           "hdu get requires at least an hdu and filename",
                           TCL_VOLATILE );
            return TCL_ERROR;
        }
        int hdu;
        if ( Tcl_GetIntFromObj( interp, objv[3], &hdu ) != TCL_OK ) {
            return TCL_ERROR;
        }
        const char *filename = Tcl_GetString( objv[4] );
        const char *objectname = NULL;
        if ( objc == 6 ) {
            objectname = Tcl_GetString( objv[5] );
        }

        //  Switch to the required HDU.
        int oldhdu = info->handle->getHDUNum();
        if ( info->handle->setHDU( hdu ) == TCL_OK ) {
            if ( info->handle->isCompressedImage() ) {
                if ( info->handle->saveCompressedImage( filename, objectname )
                     == TCL_OK ) {
                    info->handle->setHDU( oldhdu );
                    return TCL_OK;
                }
            }
        }
        Tcl_SetResult( interp, "Failed to save HDU to disk", TCL_VOLATILE );
        return TCL_ERROR;
    }

    //  Should never arrive here.
    Tcl_SetResult( interp, "Unknown HDU command", TCL_VOLATILE );
    return TCL_ERROR;
}
