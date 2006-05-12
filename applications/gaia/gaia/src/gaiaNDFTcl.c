/*+
 *   Name:
 *      gaiaNDFTcl
 
 *   Purpose:
 *      Simple, that's without 2D bias, access to NDFs from Tcl scripts.
 
 *   Language:
 *      C
 
 *   Authors:
 *      PWD: Peter W. Draper, JAC - University of Durham

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
 
 *   History:
 *      2-MAR-2006 (PWD):
 *         Original version.
 *      {enter_changes_here}
 *-
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include <tcl.h>
#include <ndf.h>
#include "gaiaNDF.h"
#include "gaiaHDS.h"
#include "gaiaArray.h"
#include "gaiaUtils.h"

/* Struct for information associated with an identifier */
struct NDFinfo {
    int ndfid;             /* The NDF identifier */
    AstFrameSet *wcs;      /* Full AST frameset for WCS */
};
typedef struct NDFinfo NDFinfo;

/* Local prototypes */
static int gaiaNDFTclBounds( ClientData clientData, Tcl_Interp *interp,
                             int objc, Tcl_Obj *CONST objv[] );
static int gaiaNDFTclCGet( ClientData clientData, Tcl_Interp *interp,
                           int objc, Tcl_Obj *CONST objv[] );
static int gaiaNDFTclClose( ClientData clientData, Tcl_Interp *interp,
                            int objc, Tcl_Obj *CONST objv[] );
static int gaiaNDFTclCoord( ClientData clientData, Tcl_Interp *interp,
                            int objc, Tcl_Obj *CONST objv[] );
static int gaiaNDFTclCreate( ClientData clientData, Tcl_Interp *interp,
                             int objc, Tcl_Obj *CONST objv[] );
static int gaiaNDFTclDims( ClientData clientData, Tcl_Interp *interp,
                           int objc, Tcl_Obj *CONST objv[] );
static int gaiaNDFTclGtWcs( ClientData clientData, Tcl_Interp *interp,
                            int objc, Tcl_Obj *CONST objv[] );
static int gaiaNDFTclMap( ClientData clientData, Tcl_Interp *interp,
                          int objc, Tcl_Obj *CONST objv[] );
static int gaiaNDFTclOpen( ClientData clientData, Tcl_Interp *interp,
                           int objc, Tcl_Obj *CONST objv[] );
static int gaiaNDFTclUnMap( ClientData clientData, Tcl_Interp *interp,
                            int objc, Tcl_Obj *CONST objv[] );

static int importNdfHandle( Tcl_Interp *interp, Tcl_Obj *obj, NDFinfo **info );
static long exportNdfHandle( int ndfid, AstFrameSet *wcs );
static int queryNdfCoord( AstFrameSet *frameSet, int axis, double *coords, 
                          int trailed, int formatted, int ncoords, 
                          char **coord, char **error_mess );

/**
 * Register all the NDF access commands.
 */
int Ndf_Init( Tcl_Interp *interp )
{
    Tcl_CreateObjCommand( interp, "ndf::close", gaiaNDFTclClose,
                          (ClientData) NULL,
                          (Tcl_CmdDeleteProc *) NULL );

    Tcl_CreateObjCommand( interp, "ndf::create", gaiaNDFTclCreate,
                          (ClientData) NULL,
                          (Tcl_CmdDeleteProc *) NULL );

    Tcl_CreateObjCommand( interp, "ndf::getbounds", gaiaNDFTclBounds,
                          (ClientData) NULL,
                          (Tcl_CmdDeleteProc *) NULL );

    Tcl_CreateObjCommand( interp, "ndf::getc", gaiaNDFTclCGet,
                          (ClientData) NULL,
                          (Tcl_CmdDeleteProc *) NULL );

    Tcl_CreateObjCommand( interp, "ndf::getcoord", gaiaNDFTclCoord,
                          (ClientData) NULL,
                          (Tcl_CmdDeleteProc *) NULL );

    Tcl_CreateObjCommand( interp, "ndf::getdims", gaiaNDFTclDims,
                          (ClientData) NULL,
                          (Tcl_CmdDeleteProc *) NULL );

    Tcl_CreateObjCommand( interp, "ndf::getwcs", gaiaNDFTclGtWcs,
                          (ClientData) NULL,
                          (Tcl_CmdDeleteProc *) NULL );

    Tcl_CreateObjCommand( interp, "ndf::map", gaiaNDFTclMap,
                          (ClientData) NULL,
                          (Tcl_CmdDeleteProc *) NULL );

    Tcl_CreateObjCommand( interp, "ndf::open", gaiaNDFTclOpen,
                          (ClientData) NULL,
                          (Tcl_CmdDeleteProc *) NULL );

    Tcl_CreateObjCommand( interp, "ndf::unmap", gaiaNDFTclUnMap,
                          (ClientData) NULL,
                          (Tcl_CmdDeleteProc *) NULL );

    return TCL_OK;
}

/*
 * Import an NDF back from a Tcl_Obj. Returns TCL_ERROR if fails.
 */
static int importNdfHandle( Tcl_Interp *interp, Tcl_Obj *obj, NDFinfo **info  )
{
    Tcl_Obj *resultObj;
    long adr;

    if ( Tcl_GetLongFromObj( interp, obj, &adr ) != TCL_OK ) {

        /* Replace result with our message */
        resultObj = Tcl_GetObjResult( interp );
        Tcl_SetStringObj( resultObj, Tcl_GetString( obj ), -1 );
        Tcl_AppendStringsToObj( resultObj, " : not an NDF identifier" ,
                                (char *)NULL );
        return TCL_ERROR;
    }

    /* Cast to a local NDFinfo struct */
    *info = (NDFinfo *) adr;
    return TCL_OK;
}

/**
 * Export an NDF identifier and return an NDFinfo struct.
 */
static long exportNdfHandle( int ndfid, AstFrameSet *wcs )
{
    NDFinfo *info = (NDFinfo *) malloc( sizeof( NDFinfo ) );

    info->ndfid = ndfid;
    info->wcs = wcs;

    return (long) info;
}

/**
 * Open an NDF. The result is the address of an NDFinfo struct.
 */
static int gaiaNDFTclOpen( ClientData clientData, Tcl_Interp *interp,
                           int objc, Tcl_Obj *CONST objv[] )
{
    Tcl_Obj *resultObj;
    char *error_mess;
    char *name;
    int ndfid;

    /* Check arguments, only allow one, the filename */
    if ( objc != 2 ) {
        Tcl_WrongNumArgs( interp, 1, objv, "ndf_name" );
        return TCL_ERROR;
    }

    /* Get name of NDF */
    name = Tcl_GetString( objv[1] );

    /* And open it */
    resultObj = Tcl_GetObjResult( interp );
    if ( gaiaNDFOpen( name, &ndfid, &error_mess ) == TCL_OK ) {
        Tcl_SetLongObj( resultObj, exportNdfHandle( ndfid, NULL ) );
        return TCL_OK;
    }
    Tcl_SetStringObj( resultObj, error_mess, -1 );
    free( error_mess );
    return TCL_ERROR;
}

/**
 * Create a 2D NDF. The result is a handle to an NDF opened for write
 * access. Requires the data type, upper and lower bounds (two sets) and an
 * optional WCS (pointer to an ASTFrameSet, set to 0 for default WCS).
 */
static int gaiaNDFTclCreate( ClientData clientData, Tcl_Interp *interp,
                             int objc, Tcl_Obj *CONST objv[] )
{
    AstFrameSet *wcs;
    Tcl_Obj *resultObj;
    char *error_mess;
    char *name;
    char *type;
    int indf;
    int lbnd[2];
    int ubnd[2];
    long adr;

    /* Check arguments, need the filename, width, height and data type. */
    if ( objc != 7 && objc != 8 ) {
        Tcl_WrongNumArgs( interp, 1, objv, "ndf_name lbnd1 ubnd1 lbnd2 ubnd2 "
                          "hds_type [frameset]" );
        return TCL_ERROR;
    }

    /* Get name of NDF */
    name = Tcl_GetString( objv[1] );

    /* Bounds */
    if ( Tcl_GetIntFromObj( interp, objv[2], &lbnd[0] ) != TCL_OK ||
         Tcl_GetIntFromObj( interp, objv[3], &ubnd[0] ) != TCL_OK ||
         Tcl_GetIntFromObj( interp, objv[4], &lbnd[1] ) != TCL_OK ||
         Tcl_GetIntFromObj( interp, objv[5], &ubnd[1] ) != TCL_OK ) {
        return TCL_ERROR;
    }

    /* HDS data type */
    type = Tcl_GetString( objv[6] );

    /* Check for the WCS */
    wcs = NULL;
    if ( objc == 8 ) {
        if ( Tcl_GetLongFromObj( interp, objv[7], &adr ) != TCL_OK ) {
            return TCL_ERROR;
        }
        wcs = (AstFrameSet *) adr;
    }

    /* And create it and return the NDF identifier. */
    resultObj = Tcl_GetObjResult( interp );
    if ( gaiaCreateNDF( name, 2, lbnd, ubnd, type, wcs, &indf, &error_mess) ) {
        Tcl_SetLongObj( resultObj, exportNdfHandle( indf, wcs ) );
        return TCL_OK;
    }
    Tcl_SetStringObj( resultObj, error_mess, -1 );
    free( error_mess );
    return TCL_ERROR;
}

/**
 * Close an NDF. Also frees the associated handle.
 */
static int gaiaNDFTclClose( ClientData clientData, Tcl_Interp *interp,
                            int objc, Tcl_Obj *CONST objv[] )
{
    NDFinfo *info;
    int result;

    /* Check arguments, only allow one, the NDF handle*/
    if ( objc != 2 ) {
        Tcl_WrongNumArgs( interp, 1, objv, "ndf_handle" );
        return TCL_ERROR;
    }

    /* Get the NDF */
    result = importNdfHandle( interp, objv[1], &info );
    if ( result == TCL_OK ) {
        /* Close NDF */
        result = gaiaNDFClose( &info->ndfid );
        
        /* Free the handle */
        free( info );
    }
    return result;
}

/**
 * Map the NDF data array in the NDF's data type, using file mapping as
 * requested. The result is a memory address (long int) of an ARRAYinfo
 * structure.
 */
static int gaiaNDFTclMap( ClientData clientData, Tcl_Interp *interp,
                          int objc, Tcl_Obj *CONST objv[] )
{
    ARRAYinfo *arrayInfo;
    NDFinfo *ndfInfo;
    char *access;
    char *error_mess;
    char *error_mess2;
    char ctype[NDF__SZTYP+1];
    int el;
    int oldmmap;
    int mmap;
    int result;
    int type;
    void *dataPtr;

    /* Check arguments, allow two, the ndf handle and whether to use file
     * mapping, if using file mapping then the access mode is used (READ,
     * UPDATE or WRITE)  */
    if ( objc != 4 ) {
        Tcl_WrongNumArgs( interp, 1, objv, "ndf_handle ?usemmap? access" );
        return TCL_ERROR;
    }

    /* Get the NDF */
    result = importNdfHandle( interp, objv[1], &ndfInfo );
    if ( result == TCL_OK ) {

        /* Mmap mode. */
        result = Tcl_GetBooleanFromObj( interp, objv[2], &mmap );
        if ( result == TCL_OK ) {
            result = gaiaNDFType( ndfInfo->ndfid, "DATA", ctype, NDF__SZTYP+1,
                                  &error_mess );
            type = gaiaArrayHDSType( ctype );
            if ( result == TCL_OK ) {
                /* Set mmap tuning, 0 for off, 1 for on, don't handle tuning
                   errors as these should be none fatal. */
                if ( gaiaHDSGTune( "MAP", &oldmmap, &error_mess2 ) != TCL_OK ){
                    free( error_mess2 );
                }
                if ( gaiaHDSTune( "MAP", mmap, &error_mess2 ) != TCL_OK ) {
                    free( error_mess2 );
                }

                /* Map data */
                access = Tcl_GetString( objv[3] );
                result = gaiaNDFMap( ndfInfo->ndfid, ctype, access, 
                                     "DATA", &dataPtr, &el, &error_mess );

                /* Restore default MAP mode */
                if ( gaiaHDSTune( "MAP", oldmmap, &error_mess2 ) != TCL_OK ) {
                    free( error_mess2 );
                }

                /* Construct result */
                if ( result == TCL_OK ) {
                    arrayInfo = gaiaArrayCreateInfo( dataPtr, type, el,
                                                     0, 0, 0, 1.0, 0.0, 0 );
                    Tcl_SetObjResult( interp,Tcl_NewLongObj((long)arrayInfo));
                }
                else {
                    Tcl_SetResult( interp, error_mess, TCL_VOLATILE );
                    free( error_mess );
                }
            }
            else {
                Tcl_SetResult( interp, error_mess, TCL_VOLATILE );
                free( error_mess );
            }
        }
    }
    return result;
}

/**
 * Unmap the NDF data array.
 */
static int gaiaNDFTclUnMap( ClientData clientData, Tcl_Interp *interp,
                            int objc, Tcl_Obj *CONST objv[] )
{
    NDFinfo *info;
    char *error_mess;
    int result;
    long adr;

    /* Check arguments, need two, the ndf handle and ARRAYinfo addresses */
    if ( objc != 3 ) {
        Tcl_WrongNumArgs( interp, 1, objv, "ndf_handle arrayinfo_address" );
        return TCL_ERROR;
    }

    /* Get the NDF */
    result = importNdfHandle( interp, objv[1], &info);
    if ( result == TCL_OK ) {

        /* Unmap */
        result = gaiaNDFUnmap( info->ndfid, "DATA", &error_mess );
        if ( result != TCL_OK ) {
            Tcl_SetResult( interp, error_mess, TCL_VOLATILE );
            free( error_mess );
        }
    }

    /* Free the ARRAYinfo struct */
    if ( Tcl_GetLongFromObj( interp, objv[2], &adr ) == TCL_OK ) {
        gaiaArrayFreeInfo( (ARRAYinfo *)adr );
    }
    return result;
}

/**
 * Return the address of the NDF WCS component. This is an AST FrameSet.
 */
static int gaiaNDFTclGtWcs( ClientData clientData, Tcl_Interp *interp,
                            int objc, Tcl_Obj *CONST objv[] )
{
    NDFinfo *info;
    Tcl_Obj *resultObj;
    char *error_mess;
    int result;

    /* Check arguments, only allow one the ndf handle */
    if ( objc != 2 ) {
        Tcl_WrongNumArgs( interp, 1, objv, "ndf_handle" );
        return TCL_ERROR;
    }

    /* Get the NDF */
    if( importNdfHandle( interp, objv[1], &info ) != TCL_OK ) {
        return TCL_ERROR;
    }
    resultObj = Tcl_GetObjResult( interp );

    /* Get WCS, this is cached as the NDF library returns a copy each time so
     * any changes will be lost */
    result = TCL_OK;
    if ( info->wcs == NULL ) {
        result = gaiaNDFGtWcs( info->ndfid, &info->wcs, &error_mess );
    }

    /* Export the WCS as a long containing the address */
    if ( result == TCL_OK ) {
        Tcl_SetLongObj( resultObj, (long) info->wcs );
    }
    else {
        Tcl_SetStringObj( resultObj, error_mess, -1 );
        free( error_mess );
    }
    return result;
}

/**
 * Return the value of an NDF character component.
 */
static int gaiaNDFTclCGet( ClientData clientData, Tcl_Interp *interp,
                           int objc, Tcl_Obj *CONST objv[] )
{
    NDFinfo *info;
    Tcl_Obj *resultObj;
    char *error_mess;
    char value[NDF__SZHIS+1];
    int result;

    /* Check arguments, need 2 the ndf handle and the component */
    if ( objc != 3 ) {
        Tcl_WrongNumArgs( interp, 1, objv, "ndf_handle component" );
        return TCL_ERROR;
    }

    /* Get the NDF */
    result = importNdfHandle( interp, objv[1], &info );
    if ( result == TCL_OK ) {
        resultObj = Tcl_GetObjResult( interp );
        value[0] = '\0';
        result = gaiaNDFCGet( info->ndfid, Tcl_GetString( objv[2] ), value,
                              NDF__SZHIS+1, &error_mess );
        if ( result == TCL_OK ) {
            Tcl_SetStringObj( resultObj, value, -1 );
        }
        else {
            Tcl_SetStringObj( resultObj, error_mess, -1 );
            free( error_mess );
        }
    }
    return result;
}

/**
 * Get the dimensions of an NDF. Returns a list of values, one for each NDF
 * dimension. Trailing redundant axes may be removed, if requested.
 */
static int gaiaNDFTclDims( ClientData clientData, Tcl_Interp *interp,
                           int objc, Tcl_Obj *CONST objv[] )
{
    NDFinfo *info;
    Tcl_Obj *resultObj;
    char *error_mess;
    int i;
    int dims[NDF__MXDIM];
    int ndim;
    int result;
    int trunc;
    
    /* Check arguments, need the ndf handle and whether to trim redundant
     * axes. */
    if ( objc != 2 && objc != 3 ) {
        Tcl_WrongNumArgs( interp, 1, objv, "ndf_handle [?trunc?]" );
        return TCL_ERROR;
    }

    /* Get the NDF */
    result = importNdfHandle( interp, objv[1], &info );
    if ( result == TCL_OK ) {
        resultObj = Tcl_GetObjResult( interp );
        result = gaiaNDFQueryDims( info->ndfid, NDF__MXDIM, dims, &ndim,
                                   &error_mess );
        if ( result == TCL_OK ) {

            /* Need to truncate? */
            trunc = 0;
            if ( objc == 3 ) {
                Tcl_GetBooleanFromObj( interp, objv[2], &trunc );
            }
            if ( trunc ) {
                /* Remove any trailing dimensions of size 1, note always keep
                 * at least one. */
                for ( i = ndim - 1; i > 0; i-- ) {
                    if ( dims[i] != 1 ) {
                        ndim = i + 1;
                        break;
                    }
                }
            }

            /* Add dimensions to result */
            for ( i = 0; i < ndim; i++ ) {
                Tcl_ListObjAppendElement( interp, resultObj,
                                          Tcl_NewIntObj( dims[i] ) );
            }
        }
        if ( result != TCL_OK ) {
            Tcl_SetStringObj( resultObj, error_mess, -1 );
            free( error_mess );
        }
    }
    return result;
}

/**
 * Get the bounds of an NDF. Returns a list of lower and upper pixel indices.
 * Trailing redundant axes may be removed, if requested.
 */
static int gaiaNDFTclBounds( ClientData clientData, Tcl_Interp *interp,
                             int objc, Tcl_Obj *CONST objv[] )
{
    NDFinfo *info;
    Tcl_Obj *resultObj;
    char *error_mess;
    int i;
    int lbnd[NDF__MXDIM];
    int ndim;
    int result;
    int trunc;
    int ubnd[NDF__MXDIM];

    /* Check arguments, need the ndf handle and whether to trim redundant
     * axes. */
    if ( objc != 2 && objc != 3 ) {
        Tcl_WrongNumArgs( interp, 1, objv, "ndf_handle [?trunc?]" );
        return TCL_ERROR;
    }

    /* Get the NDF */
    result = importNdfHandle( interp, objv[1], &info );
    if ( result == TCL_OK ) {
        resultObj = Tcl_GetObjResult( interp );
        result = gaiaNDFQueryBounds( info->ndfid, NDF__MXDIM, lbnd, ubnd, 
                                     &ndim, &error_mess );
        if ( result == TCL_OK ) {

            /* Need to truncate? */
            trunc = 0;
            if ( objc == 3 ) {
                Tcl_GetBooleanFromObj( interp, objv[2], &trunc );
            }
            if ( trunc ) {

                /* Remove any trailing dimensions of size 1, note always keep
                 * at least one. */
                for ( i = ndim - 1; i > 0; i-- ) {
                    if ( ( ubnd[i] - lbnd[i] ) != 0 ) {
                        ndim = i + 1;
                        break;
                    }
                }
            }

            for ( i = 0; i < ndim; i++ ) {
                Tcl_ListObjAppendElement( interp, resultObj,
                                          Tcl_NewIntObj( lbnd[i] ) );
                Tcl_ListObjAppendElement( interp, resultObj,
                                          Tcl_NewIntObj( ubnd[i] ) );
            }
        }
        if ( result != TCL_OK ) {
            Tcl_SetStringObj( resultObj, error_mess, -1 );
            free( error_mess );
        }
    }
    return result;
}

/**
 * Return the coordinate of a position along a given axis.
 *
 * The second argument is the NDF handle, the third the index of the axis,
 * and the fourth a list of all the pixel indices needed to identify the
 * coordinate (so the list must have a number for each dimension of the NDF if
 * that isn'r true then any extra dimensions will be given the coordinate
 * AST__BAD).  The fifth argument defines whether to format the result (using
 * astFormat), otherwise it is returned as a double. The final is a boolean
 * used to switch on the addition of trailing label and units strings.
 */
static int gaiaNDFTclCoord( ClientData clientData, Tcl_Interp *interp,
                            int objc, Tcl_Obj *CONST objv[] )
{
    NDFinfo *info;
    Tcl_Obj **listObjv;
    Tcl_Obj *resultObj;
    char *coord;
    char *error_mess;
    double coords[NDF__MXDIM];
    int axis;
    int format;
    int i;
    int ncoords;
    int result;
    int trailed;

    /* Check arguments */
    if ( objc != 6 ) {
        Tcl_WrongNumArgs( interp, 1, objv, "ndf_handle axis \
                          {c1 c2 .. cn} ?trail_units? ?format?" );
        return TCL_ERROR;
    }

    /* Import NDF  */
    result = importNdfHandle( interp, objv[1], &info );
    if ( result == TCL_OK ) {

        /* Next arguments are the axis that the coordinate is required for
         * followed by a list of all the pixel indices that specify the
         * position. */
        if ( Tcl_GetIntFromObj( interp, objv[2], &axis ) == TCL_OK ) {
            if ( Tcl_ListObjGetElements( interp, objv[3], &ncoords, &listObjv )
                 == TCL_OK ) {
                for ( i = 0; i < ncoords; i++ ) {
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
                        result = gaiaNDFGtWcs( info->ndfid, &info->wcs, 
                                               &error_mess );
                    }

                    /* Do the transformation */
                    result = queryNdfCoord( info->wcs, axis, coords,
                                            trailed, format, ncoords,
                                            &coord, &error_mess );

                    if ( result == TCL_OK ) {
                        Tcl_SetStringObj( resultObj, coord, -1 );
                    }
                    else {
                        Tcl_SetStringObj( resultObj, error_mess, -1 );
                        free( error_mess );
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
 * Query the equivalent world coordinate of a pixel coordinate along the given
 * axis. Selects the PIXEL domain from the frameSet (which must therefore be
 * from an NDF) and works with that as the BASE domain.
 */
static int queryNdfCoord( AstFrameSet *frameSet, int axis, double *coords, 
                          int trailed, int formatted, int ncoords, 
                          char **coord, char **error_mess )
{
    char *domain;
    int current;
    int base;
    int i;
    int nframes;
    int pixel;
    int result;

    /*  Find the PIXEL domain. */
    current = astGetI( frameSet, "Current" );
    base = astGetI( frameSet, "Base" );
    nframes = astGetI( frameSet, "Nframe" );
    pixel = 0;
    for ( i = 1; i <= nframes; i++ ) {
        astSetI( frameSet, "Current", i );
        domain = (char *)astGetC( frameSet, "Domain" );
        if ( strcasecmp( domain, "PIXEL" ) == 0 ) {
            pixel = i;
            break;
        }
    }
    astSetI( frameSet, "Current", current );
    if ( pixel == 0 ) {
        frameSet = (AstFrameSet *) astAnnul( frameSet );
        *error_mess = strdup( "Cannot locate the PIXEL domain" );
        *coord = NULL;
        return TCL_ERROR;
    }

    /*  This becomes the BASE frame. */
    astSetI( frameSet, "Base", pixel );

    /* Do the transformation */
    result = gaiaUtilsQueryCoord( frameSet, axis, coords, trailed, formatted,
                                  ncoords, coord, error_mess );

    /*  Restore the base frame. */
    astSetI( frameSet, "Base", base );

    if ( !astOK ) {
        astClearStatus;
    }
    return result;
}
