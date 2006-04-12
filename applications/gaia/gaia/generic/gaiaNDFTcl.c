/*+
 *   Name:
 *      gaiaNDFTcl
 *
 *   Purpose:
 *      Simple, that's without 2D bias, access to NDFs from Tcl scripts.
 *
 *   Language:
 *      C
 *
 *   Authors:
 *      PWD: Peter W. Draper, JAC - University of Durham
 *
 *   History:
 *      2-MAR-2006 (PWD):
 *         Original version.
 *      {enter_changes_here}
 *-
 */

#include <stdio.h>
#include <stdlib.h>

#include <tcl.h>
#include <ndf.h>
#include "gaiaNDF.h"
#include "gaiaHDS.h"
#include "gaiaArray.h"

/* Local prototypes */
static int gaiaNDFTclBounds( ClientData clientData, Tcl_Interp *interp,
                             int objc, Tcl_Obj *CONST objv[] );
static int gaiaNDFTclCGet( ClientData clientData, Tcl_Interp *interp,
                           int objc, Tcl_Obj *CONST objv[] );
static int gaiaNDFTclClose( ClientData clientData, Tcl_Interp *interp,
                            int objc, Tcl_Obj *CONST objv[] );
static int gaiaNDFTclCoord( ClientData clientData, Tcl_Interp *interp,
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

static int importNdfIdentifier( Tcl_Interp *interp, Tcl_Obj *obj, int *indf );

/**
 * Register all the NDF access commands.
 */
int Ndf_Init( Tcl_Interp *interp )
{
    Tcl_CreateObjCommand( interp, "ndf::close", gaiaNDFTclClose,
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
 * Import an NDF identifier from a Tcl_Obj. Returns TCL_ERROR if fails.
 */
static int importNdfIdentifier( Tcl_Interp *interp, Tcl_Obj *obj, int *indf )
{
    Tcl_Obj *resultObj;

    if ( Tcl_GetIntFromObj( interp, obj, indf ) != TCL_OK ) {

        /* Replace result with our message */
        resultObj = Tcl_GetObjResult( interp );
        Tcl_SetStringObj( resultObj, Tcl_GetString( obj ), -1 );
        Tcl_AppendStringsToObj( resultObj, " : not an NDF identifier" , 
                                (char *)NULL );
        return TCL_ERROR;
    }
    return TCL_OK;
}

/**
 * Open an NDF. The result is an NDF identifier (integer)
 */
static int gaiaNDFTclOpen( ClientData clientData, Tcl_Interp *interp,
                        int objc, Tcl_Obj *CONST objv[] )
{
    char *error_mess;
    char *name;
    int indf;
    Tcl_Obj *resultObj;

    /* Check arguments, only allow one, the filename */
    if ( objc != 2 ) {
        Tcl_WrongNumArgs( interp, 1, objv, "ndf_name" );
        return TCL_ERROR;
    }

    /* Get name of NDF */
    name = Tcl_GetString( objv[1] );

    /* And open it */
    resultObj = Tcl_GetObjResult( interp );
    if ( gaiaNDFOpen( name, &indf, &error_mess ) == TCL_OK ) {
        Tcl_SetIntObj( resultObj, indf );
        return TCL_OK;
    }
    Tcl_SetStringObj( resultObj, error_mess, -1 );
    free( error_mess );
    return TCL_ERROR;
}

/**
 * Close an NDF.
 */
static int gaiaNDFTclClose( ClientData clientData, Tcl_Interp *interp,
                         int objc, Tcl_Obj *CONST objv[] )
{
    int indf;
    int result;

    /* Check arguments, only allow one, the ndf identifier */
    if ( objc != 2 ) {
        Tcl_WrongNumArgs( interp, 1, objv, "ndf_identifier" );
        return TCL_ERROR;
    }

    /* Get the identifier */
    result = importNdfIdentifier( interp, objv[1], &indf );
    if ( result == TCL_OK ) {
        /* Close NDF */
        result = gaiaNDFClose( &indf );
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
    char *error_mess;
    char *error_mess2;
    char ctype[NDF__SZTYP+1];
    int el;
    int indf;
    int oldmmap;
    int mmap;
    int result;
    int type;
    void *dataPtr;

    /* Check arguments, allow two, the ndf identifier and whether to use file
     * mapping  */
    if ( objc != 3 ) {
        Tcl_WrongNumArgs( interp, 1, objv, "ndf_identifier ?usemmap?" );
        return TCL_ERROR;
    }

    /* Get the identifier */
    result = importNdfIdentifier( interp, objv[1], &indf );
    if ( result == TCL_OK ) {

        /* Mmap mode. */
        result = Tcl_GetBooleanFromObj( interp, objv[2], &mmap );
        if ( result == TCL_OK ) {
            result = gaiaNDFType( indf, "DATA", ctype, NDF__SZTYP+1,
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
                result = gaiaNDFMap( indf, ctype, "DATA", &dataPtr, &el, 
                                     &error_mess  );

                /* Restore default MAP mode */
                if ( gaiaHDSTune( "MAP", oldmmap, &error_mess2 ) != TCL_OK ) {
                    free( error_mess2 );
                }

                /* Construct result */
                if ( result == TCL_OK ) {
                    arrayInfo = gaiaArrayCreateInfo( dataPtr, type, el, 
                                                     0, 0, 0 );
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
    char *error_mess;
    int indf;
    int result;
    long adr;

    /* Check arguments, need two, the ndf identifier and ARRAYinfo address */
    if ( objc != 3 ) {
        Tcl_WrongNumArgs( interp, 1, objv, "ndf_identifier address" );
        return TCL_ERROR;
    }

    /* Get the identifier */
    result = importNdfIdentifier( interp, objv[1], &indf );
    if ( result == TCL_OK ) {

        /* Unmap */
        result = gaiaNDFUnmap( indf, "DATA", &error_mess );
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
 *
 * Can return a WCS for a specific axis using optional second and third
 * arguments. The third argument is an offset for the GRID domain and is used
 * when a section of a WCS is required (0 when not needed).
 */
static int gaiaNDFTclGtWcs( ClientData clientData, Tcl_Interp *interp,
                         int objc, Tcl_Obj *CONST objv[] )
{
    AstFrameSet *iwcs;
    Tcl_Obj *resultObj;
    char *error_mess;
    int axis;
    int indf;
    int offset;
    int result;

    /* Check arguments, only allow one or three, the ndf identifier and an
     * axis number plus offset */
    if ( objc != 2 && objc != 4 ) {
        Tcl_WrongNumArgs( interp, 1, objv, "ndf_identifier [axis offset]" );
        return TCL_ERROR;
    }

    /* Get the identifier */
    if( importNdfIdentifier( interp, objv[1], &indf ) != TCL_OK ) {
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

    if ( axis == -1 ) {
        /* Full WCS */
        result = gaiaNDFGtWcs( indf, &iwcs, &error_mess );
    }
    else {
        /* WCS for one axis, plus grid offset */
        result = gaiaNDFGtAxisWcs( indf, axis, offset, &iwcs, &error_mess );
    }

    /* Export the WCS as a long containing the address */
    if ( result == TCL_OK ) {
        Tcl_SetLongObj( resultObj, (long) iwcs );
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
    Tcl_Obj *resultObj;
    char *error_mess;
    char value[NDF__SZHIS+1];
    int indf;
    int result;

    /* Check arguments, need 2 the ndf identifier and the component */
    if ( objc != 3 ) {
        Tcl_WrongNumArgs( interp, 1, objv, "ndf_identifier component" );
        return TCL_ERROR;
    }

    /* Get the identifier */
    result = importNdfIdentifier( interp, objv[1], &indf );
    if ( result == TCL_OK ) {
        resultObj = Tcl_GetObjResult( interp );
        value[0] = '\0';
        result = gaiaNDFCGet( indf, Tcl_GetString( objv[2] ), value, 
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
    Tcl_Obj *resultObj;
    char *error_mess;
    int i;
    int indf;
    int dims[NDF__MXDIM];
    int ndim;
    int result;
    int trunc;

    /* Check arguments, need the ndf identifier and whether to trim redundant
     * axes. */
    if ( objc != 2 && objc != 3 ) {
        Tcl_WrongNumArgs( interp, 1, objv, "ndf_identifier [?trunc?]" );
        return TCL_ERROR;
    }

    /* Get the NDF identifier */
    result = importNdfIdentifier( interp, objv[1], &indf );
    if ( result == TCL_OK ) {
        resultObj = Tcl_GetObjResult( interp );
        result = gaiaNDFQueryDims( indf, NDF__MXDIM, dims, &ndim,
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
 */
static int gaiaNDFTclBounds( ClientData clientData, Tcl_Interp *interp,
                          int objc, Tcl_Obj *CONST objv[] )
{
    Tcl_Obj *resultObj;
    char *error_mess;
    int i;
    int indf;
    int lbnd[NDF__MXDIM];
    int ndim;
    int result;
    int ubnd[NDF__MXDIM];

    /* Check arguments, need the ndf identifier */
    if ( objc != 2 ) {
        Tcl_WrongNumArgs( interp, 1, objv, "ndf_identifier" );
        return TCL_ERROR;
    }

    /* Get the NDF identifier */
    result = importNdfIdentifier( interp, objv[1], &indf );
    if ( result == TCL_OK ) {
        resultObj = Tcl_GetObjResult( interp );
        result = gaiaNDFQueryBounds( indf, NDF__MXDIM, lbnd, ubnd, &ndim,
                                     &error_mess );
        if ( result == TCL_OK ) {
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
 * The second argument is the NDF identifier, the third the index of the axis,
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
    Tcl_Obj **listObjv;
    Tcl_Obj *resultObj;
    char *coord;
    char *error_mess;
    double coords[NDF__MXDIM];
    int axis;
    int format;
    int i;
    int indf;
    int ncoords;
    int result;
    int trailed;

    /* Check arguments */
    if ( objc != 6 ) {
        Tcl_WrongNumArgs( interp, 1, objv, "ndf_identifier axis \
                          {c1 c2 .. cn} ?trail_units? ?format?" );
        return TCL_ERROR;
    }

    /* Import NDF identifier */
    result = importNdfIdentifier( interp, objv[1], &indf );
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
                    result = gaiaNDFQueryCoord( indf, axis, coords,
                                                trailed, format,
                                                ncoords, &coord,
                                                &error_mess );
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
