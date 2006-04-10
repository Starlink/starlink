/*+
 *   Name:
 *      GaiaFITSTcl
 *
 *   Purpose:
 *      Simple, that's without 2D bias, access to FITS from Tcl scripts.
 *
 *   Language:
 *      C++
 *
 *   Authors:
 *      PWD: Peter W. Draper, JAC - University of Durham
 *
 *   History:
 *      5-APR-2006 (PWD):
 *         Original version.
 *      {enter_changes_here}
 *-
 */

#include <stdio.h>
#include <stdlib.h>

#include <tcl.h>
extern "C" {
#include <ast.h>
}
#include <Mem.h>
#include "StarFitsIO.h"
#include "gaiaArray.h"
#include "gaiaUtils.h"
#include "GaiaFITS.h"

#define MAX_DIMS 7

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
static int GaiaFITSTclDims( ClientData clientData, Tcl_Interp *interp,
                           int objc, Tcl_Obj *CONST objv[] );
static int GaiaFITSTclGtWcs( ClientData clientData, Tcl_Interp *interp,
                             int objc, Tcl_Obj *CONST objv[] );
static int GaiaFITSTclMap( ClientData clientData, Tcl_Interp *interp,
                          int objc, Tcl_Obj *CONST objv[] );
static int GaiaFITSTclOpen( ClientData clientData, Tcl_Interp *interp,
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
    Tcl_CreateObjCommand( interp, "fits::getbounds", GaiaFITSTclBounds,
                          (ClientData) NULL,
                          (Tcl_CmdDeleteProc *) NULL );

    Tcl_CreateObjCommand( interp, "fits::close", GaiaFITSTclClose,
                          (ClientData) NULL,
                          (Tcl_CmdDeleteProc *) NULL );

    Tcl_CreateObjCommand( interp, "fits::getcoord", GaiaFITSTclCoord,
                          (ClientData) NULL,
                          (Tcl_CmdDeleteProc *) NULL );

    Tcl_CreateObjCommand( interp, "fits::getdims", GaiaFITSTclDims,
                          (ClientData) NULL,
                          (Tcl_CmdDeleteProc *) NULL );

    Tcl_CreateObjCommand( interp, "fits::getc", GaiaFITSTclCGet,
                          (ClientData) NULL,
                          (Tcl_CmdDeleteProc *) NULL );

    Tcl_CreateObjCommand( interp, "fits::getwcs", GaiaFITSTclGtWcs,
                          (ClientData) NULL,
                          (Tcl_CmdDeleteProc *) NULL );

    Tcl_CreateObjCommand( interp, "fits::map", GaiaFITSTclMap,
                          (ClientData) NULL,
                          (Tcl_CmdDeleteProc *) NULL );

    Tcl_CreateObjCommand( interp, "fits::open", GaiaFITSTclOpen,
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
    info->wcs = NULL;
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
 * requested. The result is a memory address (long int), the number of
 * elements mapped and the data type. Note the data is raw and no byte
 * swapping will be applied (FITS is bigendian).
 */
static int GaiaFITSTclMap( ClientData clientData, Tcl_Interp *interp,
                           int objc, Tcl_Obj *CONST objv[] )
{
    FITSinfo *info;
    Tcl_Obj *resultObj;
    char const *type;
    size_t el;

    /* Check arguments, allow two, the FITS identifier and whether to use file
     * mapping, this may not be very useful if the data requires byte
     * swapping. */
    if ( objc != 3 ) {
        Tcl_WrongNumArgs( interp, 1, objv, "fits_identifier ?usemmap?" );
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
    type = gaiaArrayTypeToHDS( gaiaArrayFITSType( info->handle->bitpix() ) );

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
        fprintf( stderr, "el = %d\n", el );
        if ( ! info->mmap ) {
            info->dataCopy = malloc( el * info->handle->pixelSize() );
            memcpy( info->dataCopy, ptr, el * info->handle->pixelSize() );
            ptr = info->dataCopy;
        }

        /* Construct result */
        resultObj = Tcl_GetObjResult( interp );
        Tcl_ListObjAppendElement( interp, resultObj,
                                  Tcl_NewLongObj( (long) ptr ) );
        Tcl_ListObjAppendElement( interp, resultObj,
                                  Tcl_NewIntObj( (int) el ) );
        Tcl_ListObjAppendElement( interp, resultObj,
                                  Tcl_NewStringObj( type , -1 ) );
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

    /* Check arguments, only allow one, the FITS identifier */
    if ( objc != 2 ) {
        Tcl_WrongNumArgs( interp, 1, objv, "fits_identifier" );
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
    return TCL_OK;
}

/**
 * Return the address of the FITS WCS component. This is an AST FrameSet.
 *
 * Can return a WCS for a specific axis using optional second and third
 * arguments. The third argument is an offset for the GRID domain and is used
 * when a section of a WCS is required (0 when not needed).
 */
static int GaiaFITSTclGtWcs( ClientData clientData, Tcl_Interp *interp,
                             int objc, Tcl_Obj *CONST objv[] )
{
    AstFrameSet *iwcs;
    FITSinfo *info;
    Tcl_Obj *resultObj;
    char *error_mess;
    int axis;
    int offset;
    int result;

    /* Check arguments, only allow one or three, the fits identifier and an
     * axis number plus offset */
    if ( objc != 2 && objc != 4 ) {
        Tcl_WrongNumArgs( interp, 1, objv, "fits_identifier [axis offset]" );
        return TCL_ERROR;
    }

    /* Import the identifier */
    if ( importFITSIdentifier( interp, objv[1], &info ) != TCL_OK ) {
        return TCL_ERROR;
    }
    resultObj = Tcl_GetObjResult( interp );

    /* Get the axis and offset */
    axis = -1;
    offset = 0;
    if ( objc == 4 ) {
        if ( Tcl_GetIntFromObj( interp, objv[2], &axis ) != TCL_OK ) {
            return TCL_ERROR;
        }
        if ( Tcl_GetIntFromObj( interp, objv[3], &offset ) != TCL_OK ) {
            return TCL_ERROR;
        }
    }

    /* Get the full WCS, if not already done. */
    if ( info->wcs == NULL ) {
        result = GaiaFITSGtWcs( info->handle, &info->wcs, info->dims,
                                &info->ndims );
    }
    iwcs = info->wcs;

    if ( axis != -1 && result == TCL_OK ) {
        /* WCS for one axis, plus grid offset */
        result = gaiaUtilsGtAxisWcs( info->wcs, axis, offset, &iwcs, 
                                     &error_mess );
    }

    /* Export the WCS as a long containing the address */
    if ( result == TCL_OK ) {
        Tcl_SetLongObj( resultObj, (long) iwcs );
    }
    else {
        free( error_mess );
        Tcl_SetResult( interp, "Failed to read WCS", TCL_VOLATILE );
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
    char value[80];
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
        result = GaiaFITSHGet( info->handle, Tcl_GetString( objv[2] ), value,
                               80 );
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
    int dims[MAX_DIMS];
    int i;
    int ndim;
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
 * it is returned as a double. The final is a boolean used to switch on the
 * addition of trailing label and units strings.
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
    int result;
    int trailed;

    /* Check arguments */
    if ( objc != 6 ) {
        Tcl_WrongNumArgs( interp, 1, objv, "fits_identifier axis \
                          {c1 c2 .. cn} ?trail_units? ?format?" );
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
                        Tcl_SetStringObj( resultObj,
                                          "is not a valid number", -1 );
                        result = TCL_ERROR;
                        break;
                    }
                }

                /* Whether to format value. */
                format = 1;
                if ( Tcl_GetBooleanFromObj( interp, objv[4], &format )
                     != TCL_OK ) {
                    result = TCL_ERROR;
                }

                /* Final element is whether to append the label and units. */
                trailed = 0;
                if ( Tcl_GetBooleanFromObj( interp, objv[5], &trailed )
                     != TCL_OK ) {
                    result = TCL_ERROR;
                }

                resultObj = Tcl_GetObjResult( interp );
                if ( result != TCL_ERROR ) {
                    if ( info->wcs == NULL ) {
                        result = GaiaFITSGtWcs( info->handle, &info->wcs,
                                                info->dims, &info->ndims );
                    }
                    /* Do the transformation */
                    result = gaiaUtilsQueryCoord( info->wcs, axis, coords,
                                                  trailed, format, ncoords,
                                                  &coord, &error_mess );
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
