/*+
 *   Name:
 *      gaiaArrayTcl
 *
 *   Purpose:
 *      Perform operations on data cubes.
 *
 *   Language:
 *      C
 *
 *   Authors:
 *      PWD: Peter W. Draper, JAC - University of Durham
 *
 *   History:
 *      24-MAR-2006 (PWD):
 *         Original version.
 *      {enter_changes_here}
 *-
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include <tcl.h>
#include <gaiaArray.h>

/* Local prototypes */
static int gaiaArraySpectrum( ClientData clientData, Tcl_Interp *interp,
                              int objc, Tcl_Obj *CONST objv[] );
static int gaiaArrayImage( ClientData clientData, Tcl_Interp *interp,
                           int objc, Tcl_Obj *CONST objv[] );
static int gaiaArrayRelease( ClientData clientData, Tcl_Interp *interp,
                             int objc, Tcl_Obj *CONST objv[] );
static int gaiaArrayInfo( ClientData clientData, Tcl_Interp *interp,
                          int objc, Tcl_Obj *CONST objv[] );

/**
 * Register all the array commands.
 */
int Array_Init( Tcl_Interp *interp )
{
    Tcl_CreateObjCommand( interp, "array::release", gaiaArrayRelease,
                          (ClientData) NULL,
                          (Tcl_CmdDeleteProc *) NULL );

    Tcl_CreateObjCommand( interp, "array::getspectrum", gaiaArraySpectrum,
                          (ClientData) NULL,
                          (Tcl_CmdDeleteProc *) NULL );

    Tcl_CreateObjCommand( interp, "array::getimage", gaiaArrayImage,
                          (ClientData) NULL,
                          (Tcl_CmdDeleteProc *) NULL );

    Tcl_CreateObjCommand( interp, "array::getinfo", gaiaArrayInfo,
                          (ClientData) NULL,
                          (Tcl_CmdDeleteProc *) NULL );

    return TCL_OK;
}

/**
 * Extract a line of data from a cube.
 *
 * The cube must be available as a memory address to an ARRAYinfo structure,
 * the arguments are:
 *
 *   1)     the memory address (a long)
 *   2&3&4) the cube dimensions (three arguments)
 *   5)     the axis that the spectrum lies along, 1, 2 or 3.
 *   6&7)   the lower and upper bounds along axis line (-1's for all), grid
 *          indices.
 *   8&9)   grid indices of the other two dimensions (increasing order), these
 *          define the "position" of the line
 *   10)    a boolean indicating whether the extracted data should be
 *          registered with CNF so that it can be released by CNF 
 *          (NDF's will require this).
 *
 * The result is the memory address of an ARRAYinfo struct.
 */
static int gaiaArraySpectrum( ClientData clientData, Tcl_Interp *interp,
                              int objc, Tcl_Obj *CONST objv[] )
{
    ARRAYinfo *arrayInfo;
    int arange[2];
    int axis;
    int cnfMalloc;
    int dims[3];
    int index1;
    int index2;
    int nel = 0;
    long adr;
    void *outPtr;

    /* Check arguments */
    if ( objc != 11 ) {
        Tcl_WrongNumArgs( interp, 1, objv, "address dim1 dim2 dim3 "
                          "axis axis_lower axis_upper index1 index2 "
                          "?cnf_register?" );
        return TCL_ERROR;
    }

    /* Get cube memory address */
    if ( Tcl_GetLongFromObj( interp, objv[1], &adr ) != TCL_OK ) {
        Tcl_AppendResult( interp, ": failed to read data pointer",
                          (char *) NULL );
        return TCL_ERROR;
    }
    arrayInfo = (ARRAYinfo *) adr;

    /* Cube dimensions */
    if ( ( Tcl_GetIntFromObj( interp, objv[2], &dims[0] ) != TCL_OK ) ||
         ( Tcl_GetIntFromObj( interp, objv[3], &dims[1] ) != TCL_OK ) ||
         ( Tcl_GetIntFromObj( interp, objv[4], &dims[2] ) != TCL_OK ) ) {
        Tcl_AppendResult( interp, ": failed to read cube dimensions",
                          (char *) NULL );
        return TCL_ERROR;
    }

    /* Axis of spectrum */
    if ( Tcl_GetIntFromObj( interp, objv[5], &axis ) != TCL_OK ) {
        Tcl_AppendResult( interp, ": failed to read spectral axis",
                          (char *) NULL );
        return TCL_ERROR;
    }

    /* Correct to array indices (these are 0 based). */
    axis--;

    /* Range of axis */
    if ( ( Tcl_GetIntFromObj( interp, objv[6], &arange[0] ) != TCL_OK ) ||
         ( Tcl_GetIntFromObj( interp, objv[7], &arange[1] ) != TCL_OK ) ) {
        Tcl_AppendResult( interp, ": failed to read axis ranges",
                          (char *) NULL );
        return TCL_ERROR;
    }

    /* Correct to array indices from grid ones and set to either end if values
     * are -1 */
    if ( arange[0] == -1 ) {
        arange[0] = 0;
    }
    else {
        arange[0]--;
    }
    if ( arange[1] == -1 ) {
        arange[1] = dims[axis];
    }
    else {
        arange[1]--;
    }

    /* Indices of spectrum */
    if ( ( Tcl_GetIntFromObj( interp, objv[8], &index1 ) != TCL_OK ) ||
         ( Tcl_GetIntFromObj( interp, objv[9], &index2 ) != TCL_OK ) ) {
        Tcl_AppendResult( interp, ": failed to read spectral indices",
                          (char *) NULL );
        return TCL_ERROR;
    }

    /* Correct to zero-based array indices from grid indices */
    index1--;
    index2--;

    /* CNF registered memory */
    if ( Tcl_GetBooleanFromObj( interp, objv[10], &cnfMalloc ) != TCL_OK ) {
        Tcl_AppendResult( interp, ": failed to read spectral axis",
                          (char *) NULL );
        return TCL_ERROR;
    }

    /* Extraction */
    gaiaArraySpectrumFromCube( arrayInfo, dims, axis, arange,
                               index1, index2, cnfMalloc, &outPtr, &nel );

    /* Export results as an ARRAYinfo struct */
    arrayInfo = gaiaArrayCreateInfo( outPtr, arrayInfo->type, nel, 0, 0, 0 );
    Tcl_SetObjResult( interp, Tcl_NewLongObj( (long) arrayInfo ) );

    return TCL_OK;
}

/**
 * Extract an image plane from a data cube.
 *
 * The cube must be available as an ARRAYinfo structure, the arguments are:
 *
 *   1)     the memory address (a long) of an ARRAYinfo struct.
 *   2&3&4) the cube dimensions (three arguments)
 *   5)     the axis that image lies perpendicular to.
 *   6)     the grid index of the image plane along the axis.
 *   7)     a boolean indicating whether the extracted data should be
 *          registered with CNF so that it can be released by CNF
 *          (NDF's will require this).
 *
 * The result is an ARRAYinfo struct.
 */
static int gaiaArrayImage( ClientData clientData, Tcl_Interp *interp,
                           int objc, Tcl_Obj *CONST objv[] )
{
    ARRAYinfo *cubeArrayInfo = NULL;
    ARRAYinfo *imageArrayInfo = NULL;
    int axis;
    int cnfMalloc;
    int dims[3];
    int index;
    long cubeadr;

    /* Check arguments */
    if ( objc != 8 ) {
        Tcl_WrongNumArgs( interp, 1, objv, "cube_address dim1 dim2 dim3 "
                          "axis index cnf_register" );
        return TCL_ERROR;
    }

    /* Get cube memory address */
    if ( Tcl_GetLongFromObj( interp, objv[1], &cubeadr ) != TCL_OK ) {
        Tcl_AppendResult( interp, ": failed to read data pointer",
                          (char *) NULL );
        return TCL_ERROR;
    }
    cubeArrayInfo = (ARRAYinfo *)cubeadr;

    /* Cube dimensions */
    if ( ( Tcl_GetIntFromObj( interp, objv[2], &dims[0] ) != TCL_OK ) ||
         ( Tcl_GetIntFromObj( interp, objv[3], &dims[1] ) != TCL_OK ) ||
         ( Tcl_GetIntFromObj( interp, objv[4], &dims[2] ) != TCL_OK ) ) {
        Tcl_AppendResult( interp, ": failed to read cube dimensions",
                          (char *) NULL );
        return TCL_ERROR;
    }

    /* Anti-axis of image plane */
    if ( Tcl_GetIntFromObj( interp, objv[5], &axis ) != TCL_OK ) {
        Tcl_AppendResult( interp, ": failed to read image axis",
                          (char *) NULL );
        return TCL_ERROR;
    }

    /* Correct to array indices (these are 0 based). */
    axis--;

    /* Index of image plane */
    if ( Tcl_GetIntFromObj( interp, objv[6], &index ) != TCL_OK ) {
        Tcl_AppendResult( interp, ": failed to read image index",
                          (char *) NULL );
        return TCL_ERROR;
    }

    /* Correct to array indices */
    index--;

    /* CNF registered memory. */
    if ( Tcl_GetBooleanFromObj( interp, objv[7], &cnfMalloc ) != TCL_OK ) {
        Tcl_AppendResult( interp, ": failed to read spectral axis",
                          (char *) NULL );
        return TCL_ERROR;
    }

    /* Extraction */
    gaiaArrayImageFromCube( cubeArrayInfo, dims, axis, index, 
                            &imageArrayInfo, cnfMalloc );

    /* Export result */
    Tcl_SetObjResult( interp, Tcl_NewLongObj( (long) imageArrayInfo ) );
    return TCL_OK;
}

/**
 * Release previously allocated memory. The arguments are a memory address
 * of an ARRAYinfo structure and whether this memory was registered with CNF.
 */
static int gaiaArrayRelease( ClientData clientData, Tcl_Interp *interp,
                             int objc, Tcl_Obj *CONST objv[] )
{
    int cnfMalloc;
    long adr;

    /* Check arguments */
    if ( objc != 3 ) {
        Tcl_WrongNumArgs( interp, 1, objv, "address cnf_free" );
        return TCL_ERROR;
    }

    /* Get memory address */
    if ( Tcl_GetLongFromObj( interp, objv[1], &adr ) != TCL_OK ) {
        Tcl_AppendResult( interp, ": failed to read data pointer",
                          (char *) NULL );
        return TCL_ERROR;
    }

    /* CNF registered memory */
    if ( Tcl_GetBooleanFromObj( interp, objv[2], &cnfMalloc ) != TCL_OK ) {
        Tcl_AppendResult( interp, ": failed to read spectral axis",
                          (char *) NULL );
        return TCL_ERROR;
    }

    /* Free memory */
    gaiaArrayFree( (ARRAYinfo *)adr, cnfMalloc );

    return TCL_OK;
}

/**
 * Return basic information about an ARRAYinfo. These are:
 * 
 *   address of memory
 *   HDS type 
 *   number of elements
 */
static int gaiaArrayInfo( ClientData clientData, Tcl_Interp *interp,
                          int objc, Tcl_Obj *CONST objv[] )
{
    ARRAYinfo *info;
    Tcl_Obj *resultObj;
    Tcl_Obj *type;
    long adr;

    /* Check arguments */
    if ( objc != 2 ) {
        Tcl_WrongNumArgs( interp, 1, objv, "address" );
        return TCL_ERROR;
    }

    /* Get memory address */
    if ( Tcl_GetLongFromObj( interp, objv[1], &adr ) != TCL_OK ) {
        Tcl_AppendResult( interp, ": failed to read data pointer",
                          (char *) NULL );
        return TCL_ERROR;
    }
    info = (ARRAYinfo *) adr;

    resultObj = Tcl_GetObjResult( interp );
    Tcl_ListObjAppendElement( interp, resultObj,
                              Tcl_NewLongObj( (long) info->ptr ) );
    Tcl_ListObjAppendElement( interp, resultObj, Tcl_NewIntObj( info->el ) );
    type = Tcl_NewStringObj( gaiaArrayTypeToHDS( info->type ), -1 );
    Tcl_ListObjAppendElement(interp, resultObj, type );
                             
    return TCL_OK;
}
