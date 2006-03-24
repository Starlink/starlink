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

/**
 * Register all the array commands.
 */
int Array_Init( Tcl_Interp *interp )
{
    Tcl_CreateObjCommand( interp, "array::getspectrum", gaiaArraySpectrum,
                          (ClientData) NULL,
                          (Tcl_CmdDeleteProc *) NULL );
    return TCL_OK;
}

/**
 * Extract a line of data from a cube. 
 * 
 * The cube must be available as a memory address, the arguments are the
 * memory address (a long), followed by the cube type (HDS string), the
 * dimensions (three arguments), the axis that the spectrum lies along, the
 * indices of the remaining two dimensions (increasing order), and a boolean
 * indicating whether the extracted data should be registered with CNF so that
 * it can be released by CNF (NDF's will require this).
 * 
 * The result is the memory address of the extracted line.
 */
static int gaiaArraySpectrum( ClientData clientData, Tcl_Interp *interp,
                              int objc, Tcl_Obj *CONST objv[] )
{
    Tcl_Obj *resultObj;
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
    if ( objc != 10 ) {
        Tcl_WrongNumArgs( interp, objc, objv, "address data_type "
                          "dim1 dim2 dim3 axis index1 index2 cnf_reg" );
        return TCL_ERROR;
    }

    /* Get cube memory address */
    if ( Tcl_GetLongFromObj( interp, objv[1], &adr ) != TCL_OK ) {
        Tcl_AppendResult( interp, "Failed to read data pointer", 
                          (char *) NULL );
        return TCL_ERROR;
    }

    /* Data type */
    type = gaiaArrayHDSType( Tcl_GetString( objv[2] ) );

    /* Cube dimensions */
    if ( ( Tcl_GetIntFromObj( interp, objv[3], &dims[0] ) != TCL_OK ) ||
         ( Tcl_GetIntFromObj( interp, objv[4], &dims[1] ) != TCL_OK ) ||
         ( Tcl_GetIntFromObj( interp, objv[5], &dims[2] ) != TCL_OK ) ) {
        Tcl_AppendResult( interp, "Failed to read cube dimensions", 
                          (char *) NULL );
        return TCL_ERROR;
    }
    
    /* Axis of spectrum */
    if ( Tcl_GetIntFromObj( interp, objv[6], &axis ) != TCL_OK ) {
        Tcl_AppendResult( interp, "Failed to read spectral axis", 
                          (char *) NULL );
        return TCL_ERROR;
    }

    /* Indices of spectrum */
    if ( ( Tcl_GetIntFromObj( interp, objv[7], &index1 ) != TCL_OK ) ||
         ( Tcl_GetIntFromObj( interp, objv[8], &index2 ) != TCL_OK ) ) {
        Tcl_AppendResult( interp, "Failed to read spectral indices", 
                          (char *) NULL );
        return TCL_ERROR;
    }

    /* CNF registered memory */
    if ( Tcl_GetBooleanFromObj( interp, objv[9], &cnfMalloc ) != TCL_OK ) {
        Tcl_AppendResult( interp, "Failed to read spectral axis", 
                          (char *) NULL );
        return TCL_ERROR;
    }

    /* Extraction */
    
    gaiaArraySpectrumFromCube( (void *) adr, type, dims, axis, index1, index2,
                               cnfMalloc, &outPtr, &nel );
    
    /* Export address as result and number of elements extracted */
    resultObj = Tcl_GetObjResult( interp );
    Tcl_ListObjAppendElement( interp, resultObj, 
                              Tcl_NewLongObj( (long) outPtr ) );
    Tcl_ListObjAppendElement( interp, resultObj, Tcl_NewIntObj( nel ) );

    return TCL_OK;
}
