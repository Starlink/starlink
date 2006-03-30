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

#include <tcl.h>
#include <gaiaArray.h>

/* Local prototypes */
static int gaiaArraySpectrum( ClientData clientData, Tcl_Interp *interp,
                              int objc, Tcl_Obj *CONST objv[] );
static int gaiaArrayRelease( ClientData clientData, Tcl_Interp *interp,
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
    return TCL_OK;
}

/**
 * Extract a line of data from a cube.
 *
 * The cube must be available as a memory address, the arguments are:
 *
 *   1)     the memory address (a long)
 *   2)     the cube type (HDS string)
 *   3&4&5) the cube dimensions (three arguments)
 *   6)     the axis that the spectrum lies along, 1, 2 or 3.
 *   7&8)   the lower and upper bounds along axis line (-1's for all), grid
 *          indices.
 *   9&10)  grid indices of the other two dimensions (increasing order), these
 *          define the "position" of the line
 *   11)    a boolean indicating whether the extracted data should be
 *          registered with CNF so that it can be released by CNF (NDF's will
 *          require this).
 *
 * The result is the memory address of the extracted line and the number of
 * elements extracted.
 */
static int gaiaArraySpectrum( ClientData clientData, Tcl_Interp *interp,
                              int objc, Tcl_Obj *CONST objv[] )
{
    Tcl_Obj *resultObj;
    int arange[2];
    int axis;
    int cnfMalloc;
    int dims[3];
    int index1;
    int index2;
    int nel;
    int type;
    long adr;
    void *outPtr;

    /* Check arguments */
    if ( objc != 12 ) {
        Tcl_WrongNumArgs( interp, 1, objv, "address data_type "
                          "dim1 dim2 dim3 axis axis_lower axis_upper "
                          "index1 index2 cnf_register" );
        return TCL_ERROR;
    }

    /* Get cube memory address */
    if ( Tcl_GetLongFromObj( interp, objv[1], &adr ) != TCL_OK ) {
        Tcl_AppendResult( interp, ": failed to read data pointer",
                          (char *) NULL );
        return TCL_ERROR;
    }

    /* Data type */
    type = gaiaArrayHDSType( Tcl_GetString( objv[2] ) );

    /* Cube dimensions */
    if ( ( Tcl_GetIntFromObj( interp, objv[3], &dims[0] ) != TCL_OK ) ||
         ( Tcl_GetIntFromObj( interp, objv[4], &dims[1] ) != TCL_OK ) ||
         ( Tcl_GetIntFromObj( interp, objv[5], &dims[2] ) != TCL_OK ) ) {
        Tcl_AppendResult( interp, ": failed to read cube dimensions",
                          (char *) NULL );
        return TCL_ERROR;
    }

    /* Axis of spectrum */
    if ( Tcl_GetIntFromObj( interp, objv[6], &axis ) != TCL_OK ) {
        Tcl_AppendResult( interp, ": failed to read spectral axis",
                          (char *) NULL );
        return TCL_ERROR;
    }

    /* Correct to array indices (these are 0 based). */
    axis--;

    /* Range of axis */
    if ( ( Tcl_GetIntFromObj( interp, objv[7], &arange[0] ) != TCL_OK ) ||
         ( Tcl_GetIntFromObj( interp, objv[8], &arange[1] ) != TCL_OK ) ) {
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
    if ( ( Tcl_GetIntFromObj( interp, objv[9], &index1 ) != TCL_OK ) ||
         ( Tcl_GetIntFromObj( interp, objv[10], &index2 ) != TCL_OK ) ) {
        Tcl_AppendResult( interp, ": failed to read spectral indices",
                          (char *) NULL );
        return TCL_ERROR;
    }
    
    /* Correct to zero-based array indices from grid indices */
    index1--;
    index2--;

    /* CNF registered memory */
    if ( Tcl_GetBooleanFromObj( interp, objv[11], &cnfMalloc ) != TCL_OK ) {
        Tcl_AppendResult( interp, ": failed to read spectral axis",
                          (char *) NULL );
        return TCL_ERROR;
    }

    /* Extraction */

    gaiaArraySpectrumFromCube( (void *) adr, type, dims, axis, arange, 
                               index1, index2, cnfMalloc, &outPtr, &nel );

    /* Export address as result and number of elements extracted */
    resultObj = Tcl_GetObjResult( interp );
    Tcl_ListObjAppendElement( interp, resultObj,
                              Tcl_NewLongObj( (long) outPtr ) );
    Tcl_ListObjAppendElement( interp, resultObj, Tcl_NewIntObj( nel ) );

    return TCL_OK;
}

/**
 * Release previously allocated memory. The arguments are a memory address and
 * whether this memory was registered with CNF.
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
    gaiaArrayFree( (void *)adr, cnfMalloc );
    
    return TCL_OK;
}
