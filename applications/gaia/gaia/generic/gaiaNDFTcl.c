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

 *   History:
 *      2-MAR-2006 (PWD):
 *         Original version.
 *      {enter_changes_here}
 *-
 */

#if HAVE_CONFIG_H
#include "config.h"
#endif

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <float.h>
#include <limits.h>

#include <tcl.h>
#include <ndf.h>
#include <prm_par.h>
#include "gaiaNDF.h"
#include "gaiaHDS.h"
#include "GaiaArray.h"
#include "gaiaUtils.h"

/* Struct for information associated with an identifier */
struct NDFinfo {
    int ndfid;             /* The NDF identifier */
    AstFrameSet *wcs;      /* Full AST frameset for WCS */
    AstFitsChan *fitschan; /* All FITS headers in a channel, if used */
    int fitschanmod;       /* Set true then fitschan has been modified
                            * (will require saving) */
    int nhdu;              /* Number of HDUs associated with this NDF */
    int deepsearch;        /* Whether this was a deepsearch. */
    char *hdulist;         /* Tcl list of the properties of each HDU */
};
typedef struct NDFinfo NDFinfo;

/* Local prototypes */
static int gaiaNDFTclBounds( ClientData clientData, Tcl_Interp *interp,
                             int objc, Tcl_Obj *CONST objv[] );
static int gaiaNDFTclCGet( ClientData clientData, Tcl_Interp *interp,
                           int objc, Tcl_Obj *CONST objv[] );
static int gaiaNDFTclCPut( ClientData clientData, Tcl_Interp *interp,
                           int objc, Tcl_Obj *CONST objv[] );
static int gaiaNDFTclFitsHeaders( ClientData clientData, Tcl_Interp *interp,
                                  int objc, Tcl_Obj *CONST objv[] );
static int gaiaNDFTclFitsRead( ClientData clientData, Tcl_Interp *interp,
                               int objc, Tcl_Obj *CONST objv[] );
static int gaiaNDFTclFitsWrite( ClientData clientData, Tcl_Interp *interp,
                                int objc, Tcl_Obj *CONST objv[] );
static int gaiaNDFTclClose( ClientData clientData, Tcl_Interp *interp,
                            int objc, Tcl_Obj *CONST objv[] );
static int gaiaNDFTclCoord( ClientData clientData, Tcl_Interp *interp,
                            int objc, Tcl_Obj *CONST objv[] );
static int gaiaNDFTclCopy( ClientData clientData, Tcl_Interp *interp,
                           int objc, Tcl_Obj *CONST objv[] );
static int gaiaNDFTclCreate( ClientData clientData, Tcl_Interp *interp,
                             int objc, Tcl_Obj *CONST objv[] );
static int gaiaNDFTclDims( ClientData clientData, Tcl_Interp *interp,
                           int objc, Tcl_Obj *CONST objv[] );
static int gaiaNDFTclExists( ClientData clientData, Tcl_Interp *interp,
                             int objc, Tcl_Obj *CONST objv[] );
static int gaiaNDFTclExtensionExists( ClientData clientData,
                                      Tcl_Interp *interp,
                                      int objc, Tcl_Obj *CONST objv[] );
static int gaiaNDFTclGetDoubleProperty( ClientData clientData,
                                        Tcl_Interp *interp,
                                        int objc, Tcl_Obj *CONST objv[] );
static int gaiaNDFTclGetProperty( ClientData clientData, Tcl_Interp *interp,
                                  int objc, Tcl_Obj *CONST objv[] );
static int gaiaNDFTclGetPropertyDims( ClientData clientData,
                                      Tcl_Interp *interp,
                                      int objc, Tcl_Obj *CONST objv[] );
static int gaiaNDFTclGtWcs( ClientData clientData, Tcl_Interp *interp,
                            int objc, Tcl_Obj *CONST objv[] );
static int gaiaNDFTclMap( ClientData clientData, Tcl_Interp *interp,
                          int objc, Tcl_Obj *CONST objv[] );
static int gaiaNDFTclHdu( ClientData clientData, Tcl_Interp *interp,
                          int objc, Tcl_Obj *CONST objv[] );
static int gaiaNDFTclOpen( ClientData clientData, Tcl_Interp *interp,
                           int objc, Tcl_Obj *CONST objv[] );
static int gaiaNDFTclUnMap( ClientData clientData, Tcl_Interp *interp,
                            int objc, Tcl_Obj *CONST objv[] );

static int importNdfHandle( Tcl_Interp *interp, Tcl_Obj *obj, NDFinfo **info );
static long exportNdfHandle( int ndfid, AstFrameSet *wcs );
static int queryNdfCoord( AstFrameSet *frameSet, int axis, double *coords,
                          int trailed, int readable, int formatted,
                          int ncoords, char **coord, char **error_mess );
static void storeCard( AstFitsChan *channel, const char *keyword,
                       const char *value, const char *comment,
                       const char *type, int overwrite );

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

    Tcl_CreateObjCommand( interp, "ndf::copy", gaiaNDFTclCopy,
                          (ClientData) NULL,
                          (Tcl_CmdDeleteProc *) NULL );

    Tcl_CreateObjCommand( interp, "ndf::exists", gaiaNDFTclExists,
                          (ClientData) NULL,
                          (Tcl_CmdDeleteProc *) NULL );

    Tcl_CreateObjCommand( interp, "ndf::extensionexists",
                          gaiaNDFTclExtensionExists,
                          (ClientData) NULL,
                          (Tcl_CmdDeleteProc *) NULL );

    Tcl_CreateObjCommand( interp, "ndf::fitsheaders", gaiaNDFTclFitsHeaders,
                          (ClientData) NULL,
                          (Tcl_CmdDeleteProc *) NULL );

    Tcl_CreateObjCommand( interp, "ndf::fitsread", gaiaNDFTclFitsRead,
                          (ClientData) NULL,
                          (Tcl_CmdDeleteProc *) NULL );

    Tcl_CreateObjCommand( interp, "ndf::fitswrite", gaiaNDFTclFitsWrite,
                          (ClientData) NULL,
                          (Tcl_CmdDeleteProc *) NULL );

    Tcl_CreateObjCommand( interp, "ndf::getbounds", gaiaNDFTclBounds,
                          (ClientData) NULL,
                          (Tcl_CmdDeleteProc *) NULL );

    Tcl_CreateObjCommand( interp, "ndf::getc", gaiaNDFTclCGet,
                          (ClientData) NULL,
                          (Tcl_CmdDeleteProc *) NULL );

    Tcl_CreateObjCommand( interp, "ndf::getdoubleproperty",
                          gaiaNDFTclGetDoubleProperty,
                          (ClientData) NULL,
                          (Tcl_CmdDeleteProc *) NULL );

    Tcl_CreateObjCommand( interp, "ndf::getproperty", gaiaNDFTclGetProperty,
                          (ClientData) NULL,
                          (Tcl_CmdDeleteProc *) NULL );

    Tcl_CreateObjCommand( interp, "ndf::getpropertydims",
                          gaiaNDFTclGetPropertyDims,
                          (ClientData) NULL,
                          (Tcl_CmdDeleteProc *) NULL );

    Tcl_CreateObjCommand( interp, "ndf::hdu", gaiaNDFTclHdu,
                          (ClientData) NULL,
                          (Tcl_CmdDeleteProc *) NULL );

    Tcl_CreateObjCommand( interp, "ndf::putc", gaiaNDFTclCPut,
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
    info->fitschan = NULL;
    info->fitschanmod = 0;
    info->nhdu = -1;
    info->hdulist = NULL;

    return (long) info;
}

/**
 * Open an NDF. The result is the address of an NDFinfo struct.
 */
static int gaiaNDFTclOpen( ClientData clientData, Tcl_Interp *interp,
                           int objc, Tcl_Obj *CONST objv[] )
{
    Tcl_Obj *resultObj;
    char *error_mess1 = NULL;
    char *error_mess2 = NULL;
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
    if ( gaiaNDFOpen( name, &ndfid, &error_mess1 ) == TCL_OK ) {
        Tcl_SetLongObj( resultObj, exportNdfHandle( ndfid, NULL ) );
        return TCL_OK;
    }

    /* Failed, but we don't give up just yet. GAIA allows the automatic
     * opening of NDFs that are immediate children instead (.HDU_1 etc.).
     * So look for NDFs at that level. If found we pick the first one.
     */
    if ( gaiaNDFFindChild( name, &ndfid, &error_mess2 ) == TCL_OK ) {
        free( error_mess1 );
        Tcl_SetLongObj( resultObj, exportNdfHandle( ndfid, NULL ) );
        return TCL_OK;
    }

    /* Really give up, but use the first message. */
    if ( error_mess2 != NULL ) free( error_mess2 );
    Tcl_SetStringObj( resultObj, error_mess1, -1 );
    free( error_mess1 );
    return TCL_ERROR;
}

/**
 * Create an NDF. The result is a handle to an NDF opened for write
 * access. Requires the data type, upper and lower bounds (two sets) and an
 * optional WCS (pointer to an ASTFrameSet, set to 0 for default WCS).
 * The dimensionality is determined by the number of lower bounds, which
 * should match the number of upper bounds and that of the base frame of the
 * given WCS.
 */
static int gaiaNDFTclCreate( ClientData clientData, Tcl_Interp *interp,
                             int objc, Tcl_Obj *CONST objv[] )
{
    AstFrameSet *wcs;
    Tcl_Obj **listObjv;
    Tcl_Obj *resultObj;
    char *error_mess;
    char *name;
    char *type;
    int i;
    int indf;
    int lbnd[NDF__MXDIM];
    int n;
    int ndims;
    int ubnd[NDF__MXDIM];
    long adr;

    /* Check arguments, need the filename bounds and data type. */
    if ( objc != 5 && objc != 6 ) {
        Tcl_WrongNumArgs( interp, 1, objv, "ndf_name lbnd_list ubnd_list "
                          "hds_type [frameset]" );
        return TCL_ERROR;
    }

    /* Get name of NDF */
    name = Tcl_GetString( objv[1] );

    /* Lower bounds, also dimensionality */
    if ( Tcl_ListObjGetElements( interp, objv[2], &ndims, &listObjv )
         != TCL_OK ) {
        return TCL_ERROR;
    }
    for ( i = 0; i < ndims; i++ ) {
        if ( Tcl_GetIntFromObj( interp, listObjv[i], &lbnd[i] ) != TCL_OK ) {
            return TCL_ERROR;
        }
    }

    /* Upper bounds. */
    if ( Tcl_ListObjGetElements( interp, objv[3], &n, &listObjv )
         != TCL_OK ) {
        return TCL_ERROR;
    }
    if ( n != ndims ) {
        Tcl_SetResult( interp,
                       "Lower and upper bounds of requested NDF do not match",
                       TCL_VOLATILE );
        return TCL_ERROR;
    }
    for ( i = 0; i < ndims; i++ ) {
        if ( Tcl_GetIntFromObj( interp, listObjv[i], &ubnd[i] ) != TCL_OK ) {
            return TCL_ERROR;
        }
    }

    /* HDS data type */
    type = Tcl_GetString( objv[4] );

    /* Check for the WCS */
    wcs = NULL;
    if ( objc == 6 ) {
        if ( Tcl_GetLongFromObj( interp, objv[5], &adr ) != TCL_OK ) {
            return TCL_ERROR;
        }
        wcs = (AstFrameSet *) adr;
    }

    /* And create it and return the NDF identifier. */
    resultObj = Tcl_GetObjResult( interp );
    if ( gaiaCreateNDF( name, ndims, lbnd, ubnd, type, wcs, &indf,
                        &error_mess ) ) {
        Tcl_SetLongObj( resultObj, exportNdfHandle( indf, wcs ) );
        return TCL_OK;
    }
    Tcl_SetStringObj( resultObj, error_mess, -1 );
    free( error_mess );
    return TCL_ERROR;
}

/**
 * Copy parts of an NDF to create a new NDF. The result is a handle to an NDF
 * opened for write access. Requires the existing NDF and a list of components
 * to copy. The size and type of the new NDF are set to those given and a WCS
 * that matches the dimensionality must be given.
 *
 * If an axis value is given it is assumed that this is an extraction to a
 * single axis (nD to 1D) and that any AXIS component for axis "axis", should
 * be also be extracted and copied to the first dimension.
 */
static int gaiaNDFTclCopy( ClientData clientData, Tcl_Interp *interp,
                           int objc, Tcl_Obj *CONST objv[] )
{
    AstFrameSet *wcs;
    NDFinfo *info;
    Tcl_Obj **listObjv;
    Tcl_Obj *resultObj;
    char *error_mess;
    char *name;
    char *type;
    const char *clist;
    int axis = -1;
    int i;
    int lbnd[NDF__MXDIM];
    int n;
    int ndims;
    int ondf;
    int result;
    int ubnd[NDF__MXDIM];
    long adr;

    /* Check arguments. */
    if ( objc != 8 && objc != 9 ) {
        Tcl_WrongNumArgs( interp, 1, objv, "ndf_name ndf_handle "
                          "clist lbnds ubnds type frameset [axis]" );
        return TCL_ERROR;
    }

    /* Get name of output NDF */
    name = Tcl_GetString( objv[1] );

    /* Get the input NDF identifier */
    result = importNdfHandle( interp, objv[2], &info );
    if ( result == TCL_OK ) {

        /* Component list */
        clist = Tcl_GetString( objv[3] );

        /* Lower bounds, also dimensionality */
        if ( Tcl_ListObjGetElements( interp, objv[4], &ndims, &listObjv )
             != TCL_OK ) {
            return TCL_ERROR;
        }
        for ( i = 0; i < ndims; i++ ) {
            if ( Tcl_GetIntFromObj( interp, listObjv[i], &lbnd[i] )
                 != TCL_OK ) {
                return TCL_ERROR;
            }
        }

        /* Upper bounds. */
        if ( Tcl_ListObjGetElements( interp, objv[5], &n, &listObjv )
             != TCL_OK ) {
            return TCL_ERROR;
        }
        if ( n != ndims ) {
            Tcl_SetResult( interp, "Lower and upper bounds do not match",
                           TCL_VOLATILE );
            return TCL_ERROR;
        }
        for ( i = 0; i < ndims; i++ ) {
            if ( Tcl_GetIntFromObj( interp, listObjv[i], &ubnd[i] )
                 != TCL_OK ) {
                return TCL_ERROR;
            }
        }

        /* HDS data type (DATA and QUALITY components) */
        type = Tcl_GetString( objv[6] );

        /* WCS */
        wcs = NULL;
        if ( Tcl_GetLongFromObj( interp, objv[7], &adr ) != TCL_OK ) {
            return TCL_ERROR;
        }
        wcs = (AstFrameSet *) adr;

        /* If an axis is given we may need to copy the AXIS structure. */
        if ( objc == 9 ) {
            if ( Tcl_GetIntFromObj( interp, objv[8], &axis ) != TCL_OK ) {
                return TCL_ERROR;
            }
        }

        /* And do the creation and copy */
        resultObj = Tcl_GetObjResult( interp );
        if ( gaiaCopyNDF( name, info->ndfid, clist, ndims, lbnd, ubnd, type,
                          wcs, axis, &ondf, &error_mess ) ) {
            Tcl_SetLongObj( resultObj, exportNdfHandle( ondf, wcs ) );
            return TCL_OK;
        }
        Tcl_SetStringObj( resultObj, error_mess, -1 );
        free( error_mess );
    }
    return TCL_ERROR;
}


/**
 * Close an NDF. Also frees the associated handle and writes modified FITS
 * headers, if possible.
 */
static int gaiaNDFTclClose( ClientData clientData, Tcl_Interp *interp,
                            int objc, Tcl_Obj *CONST objv[] )
{
    NDFinfo *info;
    int result;
    char *error_mess;

    /* Check arguments, only allow one, the NDF handle*/
    if ( objc != 2 ) {
        Tcl_WrongNumArgs( interp, 1, objv, "ndf_handle" );
        return TCL_ERROR;
    }

    /* Get the NDF */
    result = importNdfHandle( interp, objv[1], &info );
    if ( result == TCL_OK ) {

        /* Check for FITS headers */
        if ( info->fitschan != NULL ) {
            if ( info->fitschanmod ) {
                if ( gaiaNDFCanWrite( info->ndfid ) ) {
                    result = gaiaNDFWriteFitsChan( info->ndfid, info->fitschan,
                                                   &error_mess );
                    if ( result != TCL_OK ) {
                        Tcl_SetResult( interp, error_mess, TCL_VOLATILE );
                        free( error_mess );
                    }
                }
            }

            /* Free the channel */
            info->fitschan = (AstFitsChan *) astAnnul( info->fitschan );
            info->fitschan = NULL;
            info->fitschanmod = 0;
        }

        /* Close NDF */
        result = gaiaNDFClose( &info->ndfid );

        /* Free the handle */
        if ( info->hdulist != NULL ) {
            free( info->hdulist );
        }
        free( info );
        info = NULL;
    }
    return result;
}

/**
 * Map an array component of the NDF in its native data type, using file
 * mapping as requested. The result is a memory address (long int) of an
 * ARRAYinfo structure.
 */
static int gaiaNDFTclMap( ClientData clientData, Tcl_Interp *interp,
                          int objc, Tcl_Obj *CONST objv[] )
{
    ARRAYinfo *arrayInfo;
    NDFinfo *ndfInfo;
    const char *access;
    const char *component;
    const char *fullComponent;
    char *error_mess;
    char *error_mess2;
    char ctype[NDF__SZTYP+1];
    int el;
    int oldmmap;
    int mmap;
    int result;
    int type;
    void *dataPtr;

    /* Check arguments, allow four, the ndf handle and whether to use file
     * mapping, if using file mapping then the access mode is used (READ,
     * UPDATE or WRITE). The final argument is the data component to map.  */
    if ( objc != 5 ) {
        Tcl_WrongNumArgs( interp, 1, objv,
                          "ndf_handle ?usemmap? access component" );
        return TCL_ERROR;
    }

    /* Get the NDF */
    result = importNdfHandle( interp, objv[1], &ndfInfo );
    if ( result == TCL_OK ) {

        /* Mmap mode. */
        result = Tcl_GetBooleanFromObj( interp, objv[2], &mmap );

        /* Component, need underlying component name in the case of ERROR */
        component = Tcl_GetString( objv[4] );
        if ( strcmp( component, "ERROR" ) == 0 ) {
            fullComponent = "VARIANCE";
        }
        else {
            fullComponent = component;
        }

        if ( result == TCL_OK ) {
            result = gaiaNDFType( ndfInfo->ndfid, fullComponent, ctype,
                                  NDF__SZTYP+1, &error_mess );
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
                                     component, &dataPtr, &el, &error_mess );

                /* Restore default MAP mode */
                if ( gaiaHDSTune( "MAP", oldmmap, &error_mess2 ) != TCL_OK ) {
                    free( error_mess2 );
                }

                /* Construct result */
                if ( result == TCL_OK ) {
                    arrayInfo = gaiaArrayCreateInfo( dataPtr, type, el,
                                                     0, 0, 0, 1.0, 0.0,
                                                     GAIA_ARRAY_NONE );
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
 * Unmap the NDF component.
 */
static int gaiaNDFTclUnMap( ClientData clientData, Tcl_Interp *interp,
                            int objc, Tcl_Obj *CONST objv[] )
{
    NDFinfo *info;
    char *error_mess;
    int result;
    long adr;

    /* Check arguments, the ndf handle, component to unmap and ARRAYinfo
       addresses */
    if ( objc != 4 ) {
        Tcl_WrongNumArgs( interp, 1, objv,
                          "ndf_handle component arrayinfo_address" );
        return TCL_ERROR;
    }

    /* Get the NDF */
    result = importNdfHandle( interp, objv[1], &info);
    if ( result == TCL_OK ) {

        /* Unmap */
        result = gaiaNDFUnmap( info->ndfid, Tcl_GetString( objv[2] ),
                               &error_mess );
        if ( result != TCL_OK ) {
            Tcl_SetResult( interp, error_mess, TCL_VOLATILE );
            free( error_mess );
        }
    }

    /* Free the ARRAYinfo struct */
    if ( Tcl_GetLongFromObj( interp, objv[3], &adr ) == TCL_OK ) {
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

    /* Get WCS, this is cached as the NDF library returns a copy each time so
     * any changes will be lost */
    result = TCL_OK;
    if ( info->wcs == NULL ) {
        result = gaiaNDFGtWcs( info->ndfid, &info->wcs, &error_mess );
    }

    /* Export the WCS as a long containing the address */
    if ( result == TCL_OK ) {
        Tcl_SetObjResult( interp, Tcl_NewLongObj( (long) info->wcs ) );
    }
    else {
        Tcl_SetResult( interp, error_mess, TCL_VOLATILE );
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
 * Set the value of an NDF character component.
 */
static int gaiaNDFTclCPut( ClientData clientData, Tcl_Interp *interp,
                           int objc, Tcl_Obj *CONST objv[] )
{
    NDFinfo *info;
    Tcl_Obj *resultObj;
    char *error_mess;
    int result;

    /* Check arguments, need the ndf handle, the component and the value */
    if ( objc != 4 ) {
        Tcl_WrongNumArgs( interp, 1, objv, "ndf_handle component  value" );
        return TCL_ERROR;
    }

    /* Get the NDF */
    result = importNdfHandle( interp, objv[1], &info );
    if ( result == TCL_OK ) {
        resultObj = Tcl_GetObjResult( interp );
        result = gaiaNDFCPut( info->ndfid, Tcl_GetString( objv[2] ),
                              Tcl_GetString( objv[3] ), &error_mess );
        if ( result != TCL_OK ) {
            Tcl_SetStringObj( resultObj, error_mess, -1 );
            free( error_mess );
        }
    }
    return result;
}

/**
 * Return if an NDF component exists.
 */
static int gaiaNDFTclExists( ClientData clientData, Tcl_Interp *interp,
                             int objc, Tcl_Obj *CONST objv[] )
{
    NDFinfo *info;
    Tcl_Obj *resultObj;
    const char *component;
    char *error_mess;
    int exists;
    int result;

    /* Check arguments, need 2 the ndf handle and the component */
    if ( objc != 3 ) {
        Tcl_WrongNumArgs( interp, 1, objv, "ndf_handle component" );
        return TCL_ERROR;
    }

    /* Get the NDF */
    result = importNdfHandle( interp, objv[1], &info );
    if ( result == TCL_OK ) {

        /*  ERROR component is really VARIANCE */
        component = Tcl_GetString( objv[2] );
        if ( strcmp( component, "ERROR" ) == 0 ) {
            component = "VARIANCE";
        }

        result = gaiaNDFExists( info->ndfid, Tcl_GetString( objv[2] ),
                                &exists, &error_mess );
        resultObj = Tcl_GetObjResult( interp );
        if ( result != TCL_OK ) {
            Tcl_SetStringObj( resultObj, error_mess, -1 );
            free( error_mess );
        }
        else {
            Tcl_SetBooleanObj( resultObj, exists );
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
 * that isn't true then any extra dimensions will be given the coordinate
 * AST__BAD).  The fifth argument defines whether to format the result (using
 * astFormat), otherwise it is returned as a double. The final two arguments
 * are booleans to switch on the addition of trailing label and units strings
 * and whether to make the whole result more readable (need trail plus this
 * for readable).
 */
static int gaiaNDFTclCoord( ClientData clientData, Tcl_Interp *interp,
                            int objc, Tcl_Obj *CONST objv[] )
{
    NDFinfo *info;
    Tcl_Obj **listObjv;
    char *coord;
    char *error_mess;
    double coords[NDF__MXDIM];
    int axis;
    int format;
    int i;
    int ncoords;
    int readable;
    int result;
    int trailed;

    /* Check arguments */
    if ( objc != 7 ) {
        Tcl_WrongNumArgs( interp, 1, objv, "ndf_handle axis \
                          {c1 c2 .. cn} ?format? ?trail? ?readable?" );
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
                        Tcl_SetResult( interp, "not a valid number",
                                       TCL_VOLATILE );
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

                /* Whether to append the label and units. */
                trailed = 0;
                if ( Tcl_GetBooleanFromObj( interp, objv[5], &trailed )
                     != TCL_OK ) {
                    result = TCL_ERROR;
                }

                /* Whether to make more readable. */
                readable = 0;
                if ( Tcl_GetBooleanFromObj( interp, objv[6], &readable )
                     != TCL_OK ) {
                    result = TCL_ERROR;
                }

                if ( result != TCL_ERROR ) {
                    if ( info->wcs == NULL ) {
                        result = gaiaNDFGtWcs( info->ndfid, &info->wcs,
                                               &error_mess );
                    }

                    /* Do the transformation */
                    result = queryNdfCoord( info->wcs, axis, coords,
                                            trailed, readable, format, ncoords,
                                            &coord, &error_mess );

                    if ( result == TCL_OK ) {
                        Tcl_SetResult( interp, coord, TCL_VOLATILE );
                    }
                    else {
                        Tcl_SetResult( interp, error_mess, TCL_VOLATILE );
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
                          int trailed, int readable, int formatted,
                          int ncoords, char **coord, char **error_mess )
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
    result = gaiaUtilsQueryCoord( frameSet, axis, coords, trailed,
                                  readable, formatted, ncoords, coord,
                                  error_mess );

    /*  Restore the base frame. */
    astSetI( frameSet, "Base", base );

    if ( !astOK ) {
        astClearStatus;
    }
    return result;
}

/**
 * Return the FITS headers as a string. Each card is separated by a newline.
 */
static int gaiaNDFTclFitsHeaders( ClientData clientData, Tcl_Interp *interp,
                                  int objc, Tcl_Obj *CONST objv[] )
{
    NDFinfo *info;
    char *error_mess;
    char *headers;
    char *ptr;
    int i;
    int ncards;
    int result;

    /* Check arguments, need the ndf handle */
    if ( objc != 2 ) {
        Tcl_WrongNumArgs( interp, 1, objv, "ndf_handle" );
        return TCL_ERROR;
    }

    /* Get the NDF */
    result = importNdfHandle( interp, objv[1], &info );
    if ( result == TCL_OK ) {

        /* Get the FITS headers as an AST FITS channel */
        if ( info->fitschan == NULL ) {
            result = gaiaNDFGetFitsChan( info->ndfid, &info->fitschan,
                                         &error_mess );
        }
        if ( result != TCL_OK ) {
            Tcl_SetResult( interp, error_mess, TCL_VOLATILE );
            free( error_mess );
        }
        else {
            /* Loop over all cards, append each one to the result */
            ncards = astGetI( info->fitschan, "Ncard" );
            headers = (char *) ckalloc( ncards * 81 );
            ptr = headers;
            astClear( info->fitschan, "Card" );
            for ( i = 0; i < ncards; i++ ) {
                astFindFits( info->fitschan, "%f", ptr, 1 );
                ptr[80] = '\n';
                ptr += 81;
            }
            ptr = '\0';
            Tcl_SetResult( interp, headers, TCL_DYNAMIC );
        }
        if ( !astOK ) astClearStatus;
    }
    return result;
}


/**
 * Return the value of a FITS keyword. If not found then an empty string is
 * returned.
 */
static int gaiaNDFTclFitsRead( ClientData clientData, Tcl_Interp *interp,
                               int objc, Tcl_Obj *CONST objv[] )
{
    NDFinfo *info;
    Tcl_Obj *resultObj;
    char *error_mess;
    char *start;
    char card[81];
    char value[81];
    char *ptr;
    const char *keyword;
    int result;

    /* Check arguments, need the ndf handle and the keyword */
    if ( objc != 3 ) {
        Tcl_WrongNumArgs( interp, 1, objv, "ndf_handle keyword" );
        return TCL_ERROR;
    }

    /* Get the NDF */
    result = importNdfHandle( interp, objv[1], &info );
    if ( result == TCL_OK ) {

        /* Get the FITS headers as an AST FITS channel */
        if ( info->fitschan == NULL ) {
            result = gaiaNDFGetFitsChan( info->ndfid, &info->fitschan,
                                         &error_mess );
        }
        resultObj = Tcl_GetObjResult( interp );
        if ( result != TCL_OK ) {
            Tcl_SetStringObj( resultObj, error_mess, -1 );
            free( error_mess );
        }
        else {
            keyword = Tcl_GetString( objv[2] );

            /* Look for existing card */
            astClear( info->fitschan, "Card" );
            if ( astFindFits( info->fitschan, keyword, card, 0 ) != 0 ) {
                card[80] = '\0';

                /* Extract value */
                start = strstr( card, "=" );
                if ( start != NULL ) {
                    ptr = value;
                    start++;
                    while ( *start && *start != '/' ) {
                        *ptr++ = *start++;
                    }
                    *ptr = '\0';
                    ptr = value;
                }
                else {
                    ptr = card;
                }
                Tcl_SetStringObj( resultObj, ptr, -1);
            }
            else {
                /* Return an empty string when the card is not found */
                Tcl_SetResult( interp, "", TCL_VOLATILE );
            }
        }
    }
    return result;
}

/**
 * Write a FITS card, either as a keyword, value, comment set or
 * as a whole card. If using a keyword the type of value supplied
 * can be "char", "numeric". If numeric the value is written without quotes.
 */
static int gaiaNDFTclFitsWrite( ClientData clientData, Tcl_Interp *interp,
                                int objc, Tcl_Obj *CONST objv[] )
{
    NDFinfo *info;
    Tcl_Obj *resultObj;
    char *error_mess;
    const char *comment;
    const char *keyword;
    const char *value;
    const char *type;
    int result;


    /* Check arguments, need the ndf handle, the keyword, value, comment and
     * type, or a card. */
    if ( objc != 3 && objc != 6 ) {
        Tcl_WrongNumArgs( interp, 1, objv,
                          "ndf_handle [keyword value comment type | card]" );
        return TCL_ERROR;
    }

    /* Get the NDF */
    result = importNdfHandle( interp, objv[1], &info );
    if ( result == TCL_OK ) {

        /* Get the FITS headers as an AST FITS channel */
        if ( info->fitschan == NULL ) {
            result = gaiaNDFGetFitsChan( info->ndfid, &info->fitschan,
                                         &error_mess );
        }
        resultObj = Tcl_GetObjResult( interp );
        if ( result != TCL_OK ) {
            Tcl_SetStringObj( resultObj, error_mess, -1 );
            free( error_mess );
        }
        else {
            if ( objc == 6 ) {
                keyword = Tcl_GetString( objv[2] );
                value = Tcl_GetString( objv[3] );
                comment = Tcl_GetString( objv[4] );
                type = Tcl_GetString( objv[5] );

                /* Look for existing card */
                astClear( info->fitschan, "Card" );
                astFindFits( info->fitschan, keyword, NULL, 0 );

                /* And store value */
                storeCard( info->fitschan, keyword, value, comment, type, 1 );
            }
            else {
                /* These go at the end */
                astClear( info->fitschan, "Card" );
                astSetI( info->fitschan, "Card",
                         astGetI( info->fitschan, "Ncard" ) + 1 );
                astPutFits( info->fitschan, Tcl_GetString( objv[2] ), 0 );
            }
            info->fitschanmod = 1;
        }
    }
    return result;
}

/**
 * Write a keyword value comment set into a FITS channel. The type
 * string should start with one of "c" or "n", for character or numeric.
 */
static void storeCard( AstFitsChan *fitschan, const char *keyword,
                       const char *value, const char *comment,
                       const char *type, int overwrite )
{
    char card[81];
    if ( type[0] == 'n' ) {
        /* Numeric value, no quotes */
        if ( strlen( value ) > 20 ) {
            sprintf( card, "%-8.8s= %s / %s", keyword, value, comment );
        }
        else {
            sprintf( card, "%-8.8s= %20.20s / %s", keyword, value, comment );
        }
    }
    else {
        /* Char */
        if ( strlen( value ) > 18 ) {
            sprintf( card, "%-8.8s= '%s' / %s", keyword, value, comment );
        }
        else {
            sprintf( card, "%-8.8s= '%18.18s' / %s", keyword, value, comment );
        }
    }
    astPutFits( fitschan, card, overwrite );
}

/**
 * Return if a named extension exists.
 */
static int gaiaNDFTclExtensionExists( ClientData clientData,
                                      Tcl_Interp *interp,
                                      int objc, Tcl_Obj *CONST objv[] )
{
    NDFinfo *info;
    int exists;
    int result;

    /* Check arguments, need the ndf handle and extension */
    if ( objc != 3 ) {
        Tcl_WrongNumArgs( interp, 1, objv, "ndf_handle extension" );
        return TCL_ERROR;
    }

    result = importNdfHandle( interp, objv[1], &info );
    if ( result == TCL_OK ) {
        exists = gaiaNDFExtensionExists( info->ndfid,
                                         Tcl_GetString( objv[2] ) );
        Tcl_SetObjResult( interp, Tcl_NewBooleanObj( exists ) );

    }
    return result;
}

/**
 * Return the value of a property. This is a primitive in an extension of the
 * NDF. So for instance "ACSIS" "TSYS(10,10)" will return the value of the
 * primitive in the ACSIS extension, if it exists. Calls to the "FITS"
 * extension will be passed to the ::fitsread procedure.
 *
 * If the value doesn't exist an empty string is returned.
 */
static int gaiaNDFTclGetProperty( ClientData clientData, Tcl_Interp *interp,
                                  int objc, Tcl_Obj *CONST objv[] )
{
    NDFinfo *info;
    char *error_mess;
    char value[132];
    const char *extension;
    const char *name;
    int result;

    /* Check arguments, need the ndf handle, extension and the property name */
    if ( objc != 4 ) {
        Tcl_WrongNumArgs( interp, 1, objv, "ndf_handle extension name" );
        return TCL_ERROR;
    }

    /* Name of extension */
    extension = Tcl_GetString( objv[2] );

    /* If the extension is "FITS", just pass this request on */
    if ( strcmp( "FITS", extension ) == 0 ) {
        Tcl_Obj *newobjv[3];
        newobjv[0] = objv[0];
        newobjv[1] = objv[1];
        newobjv[2] = objv[3];
        return gaiaNDFTclFitsRead( clientData, interp, 3, newobjv );
    }

    /* Get the NDF */
    result = importNdfHandle( interp, objv[1], &info );
    if ( result == TCL_OK ) {

        /* Check we have an extension */
        if ( gaiaNDFExtensionExists( info->ndfid, extension ) ) {

            /* Name of the component */
            name = Tcl_GetString( objv[3] );

            /* Value */
            result = gaiaNDFGetProperty( info->ndfid, extension, name, value,
                                         132, &error_mess );
            if ( result == TCL_OK ) {
                Tcl_SetResult( interp, value, TCL_VOLATILE );
            }
            else {
                Tcl_SetResult( interp, error_mess, TCL_VOLATILE );
                free( error_mess );
            }
        }
        else {
            /* No extension, so no value */
            Tcl_SetResult( interp, "", TCL_VOLATILE );
        }
    }
    return result;
}

/**
 * Return the value of a double precision property. Access etc. same
 * as getproperty. Don't use this with FITS, the return semantics differ
 * and there's no difference.
 *
 * If the value doesn't exist the value "BAD" is returned, which can
 * also mean the value itself was set to VAL__BADD.
 */
static int gaiaNDFTclGetDoubleProperty( ClientData clientData,
                                        Tcl_Interp *interp,
                                        int objc, Tcl_Obj *CONST objv[] )
{
    NDFinfo *info;
    char *error_mess;
    const char *extension;
    const char *name;
    double value;
    int result;

    /* Check arguments, need the ndf handle, extension and the property name */
    if ( objc != 4 ) {
        Tcl_WrongNumArgs( interp, 1, objv, "ndf_handle extension name" );
        return TCL_ERROR;
    }

    /* Name of extension */
    extension = Tcl_GetString( objv[2] );

    /* If the extension is "FITS", just pass this request on, result is
     * a string, but with full precision anyway. */
    if ( strcmp( "FITS", extension ) == 0 ) {
        Tcl_Obj *newobjv[3];
        newobjv[0] = objv[0];
        newobjv[1] = objv[1];
        newobjv[2] = objv[3];
        return gaiaNDFTclFitsRead( clientData, interp, 3, newobjv );
    }

    /* Get the NDF */
    result = importNdfHandle( interp, objv[1], &info );
    if ( result == TCL_OK ) {

        /* Check we have an extension */
        if ( gaiaNDFExtensionExists( info->ndfid, extension ) ) {

            /* Name of the component */
            name = Tcl_GetString( objv[3] );

            /* Value */
            result = gaiaNDFGetDoubleProperty( info->ndfid, extension, name,
                                               &value, &error_mess );

            /* Check against failures, note that conversion of VAL__BADR to
             * VAL__BADD doesn't happen! */
            if ( result == TCL_OK && value != VAL__BADD &&
                 (float) value != VAL__BADR ) {
                Tcl_SetObjResult( interp, Tcl_NewDoubleObj( value ) );
            }
            else {
                Tcl_SetResult( interp, "BAD", TCL_VOLATILE );
                if ( result != TCL_OK ) {
                    free( error_mess );

                    /* The BAD return is also the signal of an error, so
                     * clear the status return */
                    result = TCL_OK;
                }
            }
        }
        else {
            /* No extension, so no value */
            Tcl_SetResult( interp, "BAD", TCL_VOLATILE );
        }
    }
    return result;
}

/**
 * Return the dimensionality of a named component stored in an extension.
 * If the extension or component do not exist then an error will occur.
 */
static int gaiaNDFTclGetPropertyDims( ClientData clientData,
                                      Tcl_Interp *interp,
                                      int objc, Tcl_Obj *CONST objv[] )
{
    NDFinfo *info;
    Tcl_Obj *resultObj;
    char *error_mess;
    const char *extension;
    const char *name;
    hdsdim dims[NDF__MXDIM];
    int ndim;
    int i;
    int result;

    /* Check arguments, need the ndf handle, extension and the component
     * name. */
    if ( objc != 4 ) {
        Tcl_WrongNumArgs( interp, 1, objv,
                          "ndf_handle extension component_name" );
        return TCL_ERROR;
    }

    /* Name of extension. */
    extension = Tcl_GetString( objv[2] );

    /* Get the NDF */
    result = importNdfHandle( interp, objv[1], &info );
    if ( result == TCL_OK ) {

        /* Name of the component */
        name = Tcl_GetString( objv[3] );

        /* Get the size. If this fails then the dimensionality is returned
         * as zero */
        result = gaiaNDFGetPropertyDims( info->ndfid, extension, name,
                                         dims, &ndim, &error_mess );

        if ( result == TCL_OK && ndim > 0 ) {
            for ( i = 0; i < ndim; i++ ) {
                if ( ( dims[i] < INT_MIN ) || ( dims[i] > INT_MAX ) ) {
                    Tcl_SetResult( interp,
                                   "Get extension component dimension is too large",
                                   TCL_VOLATILE );
                    result = TCL_ERROR;
                    break;
                }
            }

            if ( result == TCL_OK ) {
                resultObj = Tcl_GetObjResult( interp );
                for ( i = 0; i < ndim; i++ ) {
                    Tcl_ListObjAppendElement( interp, resultObj,
                                              Tcl_NewIntObj( (int) dims[i] ) );
                }
            }
        }
        else {
            if ( ndim == 0 && result == TCL_OK ) {
                Tcl_SetResult( interp,
                               "Failed to get extension component dimensions",
                               TCL_VOLATILE );
                result = TCL_ERROR;
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
 * Commands for querying the HDUs associated with the current NDF.
 * Symbolically HDUs are NDFs at the same level as the current NDF
 * (maps to the FITS HDU concept).
 *
 * "list"               returns a list of all the HDU properties.
 * "listheadings"       returns a list of headings for the returned properties.
 * "get <n> filename"   noop.
 *
 * The headings are: HDU Type ExtName NAXIS NAXIS1 NAXIS2 NAXIS3, note we
 * do not also list the presence of other displayable components in the NDF
 * (variance & quality) and Type is always "NDF". A deeper search for children
 * in the NDFs extensions can also be included when querying for a list.
 */
static int gaiaNDFTclHdu( ClientData clientData, Tcl_Interp *interp,
                          int objc, Tcl_Obj *CONST objv[] )
{
    NDFinfo *info;
    const char *action;
    int deepsearch = 1;
    int result;

    /* Check arguments, need the NDF handle, the hdu command and whether to
     * look for NDFs in the extension. */
    if ( objc < 3 || objc > 5 ) {
        Tcl_WrongNumArgs( interp, 1, objv,
                          "ndf_handle [list|listheadings|get] [deepsearch]" );
        return TCL_ERROR;
    }

    /* Import the NDF */
    result = importNdfHandle( interp, objv[1], &info );
    if ( result != TCL_OK ) {
        return TCL_ERROR;
    }

    action = Tcl_GetString( objv[2] );

    /* hdu listheadings. */
    if ( strcmp( action, "listheadings" ) == 0 ) {
        Tcl_SetResult( interp, "HDU Type ExtName NAXIS NAXIS1 NAXIS2 NAXIS3",
                       TCL_VOLATILE );
        return TCL_OK;
    }

    /* hdu list. */
    if ( strcmp( action, "list" ) == 0 ) {
        if ( objc == 4 ) {
            Tcl_GetIntFromObj( interp, objv[3], &deepsearch );
        }

        /* If this NDF hasn't been checked for "HDUs" then do that now.
         * Also need to do this again if deepsearch changes as that produces a
         * different listing*/
        if ( info->nhdu == -1 || ( deepsearch != info->deepsearch ) ) {
            if ( info->hdulist != NULL ) {
                free( info->hdulist );
            }
            info->deepsearch = deepsearch;
            gaiaNDFSiblingSearch( info->ndfid, deepsearch, &info->nhdu,
                                  &info->hdulist );
        }
        Tcl_SetResult( interp, info->hdulist, TCL_VOLATILE );
        return TCL_OK;
    }

    /*  hdu get <n> filename. Does nothing, just satifies the interface. */
    if ( strcmp( action, "get" ) == 0 ) {
        return TCL_OK;
    }

    /*  Should never arrive here. */
    Tcl_SetResult( interp, "Unknown HDU command", TCL_VOLATILE );
    return TCL_ERROR;
}
