/*+
 *  Name:
       gaiaArrayTcl
 *
 *  Purpose:
 *     Perform operations on array data (mainly data cubes).

 *  Language:
 *     C

 *  Authors:
 *     PWD: Peter W. Draper, JAC - University of Durham

 *  Copyright:
 *     Copyright (C) 2006 Particle Physics & Astronomy Research Council.
 *     Copyright (C) 2009 Science and Technology Facilities Council.
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

 *  History:
 *     24-MAR-2006 (PWD):
 *        Original version.
 *     {enter_changes_here}
 *-
 */

#if HAVE_CONFIG_H
#include "config.h"
#endif

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include <tcl.h>
#include <GaiaArray.h>

/* Local prototypes */
static int gaiaArrayCopy( ClientData clientData, Tcl_Interp *interp,
                          int objc, Tcl_Obj *CONST objv[] );
static int gaiaArrayCreate( ClientData clientData, Tcl_Interp *interp,
                            int objc, Tcl_Obj *CONST objv[] );
static int gaiaArrayGetMinMax( ClientData clientData, Tcl_Interp *interp,
                               int objc, Tcl_Obj *CONST objv[] );
static int gaiaArrayImage( ClientData clientData, Tcl_Interp *interp,
                           int objc, Tcl_Obj *CONST objv[] );
static int gaiaArrayInfo( ClientData clientData, Tcl_Interp *interp,
                          int objc, Tcl_Obj *CONST objv[] );
static int gaiaArrayMask( ClientData clientData, Tcl_Interp *interp,
                          int objc, Tcl_Obj *CONST objv[] );
static int gaiaArrayRegionSpectrum( ClientData clientData, Tcl_Interp *interp,
                                    int objc, Tcl_Obj *CONST objv[] );
static int gaiaArrayRelease( ClientData clientData, Tcl_Interp *interp,
                             int objc, Tcl_Obj *CONST objv[] );
static int gaiaArraySpectrum( ClientData clientData, Tcl_Interp *interp,
                              int objc, Tcl_Obj *CONST objv[] );
static int gaiaArrayWrap( ClientData clientData, Tcl_Interp *interp,
                          int objc, Tcl_Obj *CONST objv[] );

/**
 * Register all the array commands.
 */
int Array_Init( Tcl_Interp *interp )
{
    Tcl_CreateObjCommand( interp, "array::copy", gaiaArrayCopy,
                          (ClientData) NULL,
                          (Tcl_CmdDeleteProc *) NULL );

    Tcl_CreateObjCommand( interp, "array::create", gaiaArrayCreate,
                          (ClientData) NULL,
                          (Tcl_CmdDeleteProc *) NULL );

    Tcl_CreateObjCommand( interp, "array::getregionspectrum",
                          gaiaArrayRegionSpectrum,
                          (ClientData) NULL,
                          (Tcl_CmdDeleteProc *) NULL );

    Tcl_CreateObjCommand( interp, "array::getspectrum", gaiaArraySpectrum,
                          (ClientData) NULL,
                          (Tcl_CmdDeleteProc *) NULL );

    Tcl_CreateObjCommand( interp, "array::getimage", gaiaArrayImage,
                          (ClientData) NULL,
                          (Tcl_CmdDeleteProc *) NULL );

    Tcl_CreateObjCommand( interp, "array::getminmax", gaiaArrayGetMinMax,
                          (ClientData) NULL,
                          (Tcl_CmdDeleteProc *) NULL );

    Tcl_CreateObjCommand( interp, "array::getinfo", gaiaArrayInfo,
                          (ClientData) NULL,
                          (Tcl_CmdDeleteProc *) NULL );

    Tcl_CreateObjCommand( interp, "array::maskdata", gaiaArrayMask,
                          (ClientData) NULL,
                          (Tcl_CmdDeleteProc *) NULL );

    Tcl_CreateObjCommand( interp, "array::release", gaiaArrayRelease,
                          (ClientData) NULL,
                          (Tcl_CmdDeleteProc *) NULL );

    Tcl_CreateObjCommand( interp, "array::wrap", gaiaArrayWrap,
                          (ClientData) NULL,
                          (Tcl_CmdDeleteProc *) NULL );

    return TCL_OK;
}

/**
 * Extract a "region" spectrum from a data cube. The region is a 2D ARD
 * description in the image plane. Each region is combined into point of the
 * resultant spectrum.
 *
 * The cube must be available as a memory address to an ARRAYinfo structure,
 * the arguments are:
 *
 *   1)     the memory address (a long)
 *   2&3&4) the cube dimensions (three arguments)
 *   5)     the axis defining the spectral direction 1, 2 or 3.
 *   6&7)   the lower and upper bounds along axis line (-1's for all), grid
 *          indices.
 *   8)     the ARD description, with grid coordinates.
 *   9)     the combination method, only mean and median are supported.
 *   10)    a boolean indicating whether the extracted data should be
 *          registered with CNF so that it can be released by CNF
 *          (NDFs will require this).
 *
 * The result is the memory address of an ARRAYinfo struct.
 */
static int gaiaArrayRegionSpectrum( ClientData clientData, Tcl_Interp *interp,
                                    int objc, Tcl_Obj *CONST objv[] )
{
    ARRAYinfo *arrayInfo;
    char *description;
    int method;
    int arange[2];
    int axis;
    int cnfMalloc;
    int dims[3];
    int nel = 0;
    int outtype;
    long adr;
    void *outPtr;

    /* Check arguments */
    if ( objc != 11 ) {
        Tcl_WrongNumArgs( interp, 1, objv, "address dim1 dim2 dim3 "
                          "axis axis_lower axis_upper ard_description "
                          "combination_method ?cnf_register?" );
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

    /* ARD description */
    description = Tcl_GetString( objv[8] );

    /* Combination method */
    if ( strcmp( Tcl_GetString( objv[9] ), "mean" ) == 0 ) {
        method = GAIA_ARRAY_MEAN;
    }
    else {
        method = GAIA_ARRAY_MEDIAN;
    }

    /* CNF registered memory */
    if ( Tcl_GetBooleanFromObj( interp, objv[10], &cnfMalloc ) != TCL_OK ) {
        Tcl_AppendResult( interp, ": failed to read spectral axis",
                          (char *) NULL );
        return TCL_ERROR;
    }
    if ( cnfMalloc ) {
        cnfMalloc = GAIA_ARRAY_CNFMALLOC;
    }
    else {
        cnfMalloc = GAIA_ARRAY_MALLOC;
    }

    /* Extraction */
    gaiaArrayRegionSpectrumFromCube( arrayInfo, dims, axis, arange,
                                     description, method, cnfMalloc,
                                     &outPtr, &nel, &outtype );

    /* Export results as an ARRAYinfo struct */
    arrayInfo = gaiaArrayCreateInfo( outPtr, outtype, nel, 0, 0, 0, 1.0, 0.0,
                                     cnfMalloc );
    Tcl_SetObjResult( interp, Tcl_NewLongObj( (long) arrayInfo ) );

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
 *          (NDFs will require this).
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
    int outtype;
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
    if ( cnfMalloc ) {
        cnfMalloc = GAIA_ARRAY_CNFMALLOC;
    }
    else {
        cnfMalloc = GAIA_ARRAY_MALLOC;
    }

    /* Extraction */
    gaiaArraySpectrumFromCube( arrayInfo, dims, axis, arange,
                               index1, index2, cnfMalloc, &outPtr, &nel,
                               &outtype );

    /* Export results as an ARRAYinfo struct */
    arrayInfo = gaiaArrayCreateInfo( outPtr, outtype, nel, 0, 0, 0, 1.0, 0.0,
                                     cnfMalloc );
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
    if ( cnfMalloc ) {
        cnfMalloc = GAIA_ARRAY_CNFMALLOC;
    }
    else {
        cnfMalloc = GAIA_ARRAY_MALLOC;
    }

    /* Extraction */
    gaiaArrayImageFromCube( cubeArrayInfo, dims, axis, index,
                            &imageArrayInfo, cnfMalloc );

    /* Export result */
    Tcl_SetObjResult( interp, Tcl_NewLongObj( (long) imageArrayInfo ) );
    return TCL_OK;
}

/**
 * Create an array.
 *
 * The arguments are:
 *
 *   data type as a FITS bitpix
 *   the number of elements in the array.
 *
 * The result is an ARRAYinfo struct.
 */
static int gaiaArrayCreate( ClientData clientData, Tcl_Interp *interp,
                           int objc, Tcl_Obj *CONST objv[] )
{
    ARRAYinfo *arrayInfo;
    int bitpix;
    int type;
    long el;
    size_t nel;
    void *ptr;

    /* Check arguments */
    if ( objc != 3 ) {
        Tcl_WrongNumArgs( interp, 1, objv, "data_type nel" );
        return TCL_ERROR;
    }

    /* Get data type. */
    if ( Tcl_GetIntFromObj( interp, objv[1], &bitpix ) != TCL_OK ) {
        Tcl_AppendResult( interp, ": failed to read bitpix",
                          (char *) NULL );
        return TCL_ERROR;
    }
    type = gaiaArrayFITSType( bitpix );
    if ( type == HDS_UNKNOWN ) {
        Tcl_SetResult( interp, "Unknown data type", TCL_VOLATILE );
        return TCL_ERROR;
    }

    /* Number of elements */
    if ( Tcl_GetLongFromObj( interp, objv[2], &el ) != TCL_OK ) {
        Tcl_AppendResult( interp, ": failed to read number of elements",
                          (char *) NULL );
        return TCL_ERROR;
    }

    /* Get memory. */
    nel = el * gaiaArraySizeOf( type );
    gaiaAllocateMemory( GAIA_ARRAY_NEW, nel, &ptr );

    /* Create the info struct. */
    arrayInfo = gaiaArrayCreateInfo( ptr, type, el, 0, 1, 0, 1.0, 0.0,
                                     GAIA_ARRAY_NEW );

    /* Export */
    Tcl_SetObjResult( interp, Tcl_NewLongObj( (long) arrayInfo ) );
    return TCL_OK;
}

/**
 * Create an array by wrapping some given memory.
 *
 * The arguments are:
 *
 *   the address of the memory to wrap
 *   data type as a FITS bitpix
 *   the number of elements in the array.
 *
 * The result is an ARRAYinfo struct. Note that the memory will never be
 * freed, the caller must arrange that.
 */
static int gaiaArrayWrap( ClientData clientData, Tcl_Interp *interp,
                          int objc, Tcl_Obj *CONST objv[] )
{
    ARRAYinfo *arrayInfo;
    int bitpix;
    int type;
    long adr;
    long el;
    void *ptr;

    /* Check arguments */
    if ( objc != 4 ) {
        Tcl_WrongNumArgs( interp, 1, objv, "address data_type nel" );
        return TCL_ERROR;
    }

    /* Get address of the data array. */
    if ( Tcl_GetLongFromObj( interp, objv[1], &adr ) != TCL_OK ) {
        Tcl_AppendResult( interp, ": failed to read memory address",
                          (char *) NULL );
        return TCL_ERROR;
    }
    ptr = (void *)adr;

    /* Get data type. */
    if ( Tcl_GetIntFromObj( interp, objv[2], &bitpix ) != TCL_OK ) {
        Tcl_AppendResult( interp, ": failed to read bitpix",
                          (char *) NULL );
        return TCL_ERROR;
    }
    type = gaiaArrayFITSType( bitpix );
    if ( type == HDS_UNKNOWN ) {
        Tcl_SetResult( interp, "Unknown data type", TCL_VOLATILE );
        return TCL_ERROR;
    }

    /* Number of elements */
    if ( Tcl_GetLongFromObj( interp, objv[3], &el ) != TCL_OK ) {
        Tcl_AppendResult( interp, ": failed to read number of elements",
                          (char *) NULL );
        return TCL_ERROR;
    }

    /* Create the info struct. */
    arrayInfo = gaiaArrayCreateInfo( ptr, type, el, 1, 0, 0, 1.0, 0.0,
                                     GAIA_ARRAY_NONE );

    /* Export */
    Tcl_SetObjResult( interp, Tcl_NewLongObj( (long) arrayInfo ) );
    return TCL_OK;
}

/**
 * Release previously allocated memory. The arguments are a memory address
 * of an ARRAYinfo structure.
 */
static int gaiaArrayRelease( ClientData clientData, Tcl_Interp *interp,
                             int objc, Tcl_Obj *CONST objv[] )
{
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

    /* Free memory */
    if ( adr != 0 ) {
        gaiaArrayFree( (ARRAYinfo *)adr );
    }
    return TCL_OK;
}

/**
 * Return basic information about an ARRAYinfo. These are:
 *
 *   address of memory
 *   basic HDS type
 *   number of elements
 *   full HDS type (used when scaling a variant or mapping byte data).
 */
static int gaiaArrayInfo( ClientData clientData, Tcl_Interp *interp,
                          int objc, Tcl_Obj *CONST objv[] )
{
    ARRAYinfo *info;
    Tcl_Obj *resultObj;
    Tcl_Obj *basictype;
    Tcl_Obj *fulltype;
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
    if ( adr == 0L ) {
        Tcl_SetResult( interp, "NULL pointer supplied", TCL_VOLATILE );
        return TCL_ERROR;
    }
    info = (ARRAYinfo *) adr;

    resultObj = Tcl_GetObjResult( interp );

    Tcl_ListObjAppendElement( interp, resultObj,
                              Tcl_NewLongObj( (long) info->ptr ) );
    Tcl_ListObjAppendElement( interp, resultObj, Tcl_NewLongObj( info->el ) );

    basictype = Tcl_NewStringObj( gaiaArrayTypeToHDS( info->type ), -1 );
    Tcl_ListObjAppendElement(interp, resultObj, basictype );

    fulltype = Tcl_NewStringObj( gaiaArrayFullTypeToHDS( info->type,
                                                         info->isfits,
                                                         info->bscale,
                                                         info->bzero ), -1 );
    Tcl_ListObjAppendElement(interp, resultObj, fulltype );

    return TCL_OK;
}

/**
 * Copy the data of an ARRAYinfo struct to another ARRAYinfo struct.
 *
 * The data types and sizes must match.
 */
static int gaiaArrayCopy( ClientData clientData, Tcl_Interp *interp,
                          int objc, Tcl_Obj *CONST objv[] )
{
    ARRAYinfo *fromInfo;
    ARRAYinfo *toInfo;
    long adr;
    size_t nel;

    /* Check arguments */
    if ( objc != 3 ) {
        Tcl_WrongNumArgs( interp, 1, objv, "from_array to_array" );
        return TCL_ERROR;
    }

    /* Get memory addresses */
    if ( Tcl_GetLongFromObj( interp, objv[1], &adr ) != TCL_OK ) {
        Tcl_AppendResult( interp, ": failed to read data pointer",
                          (char *) NULL );
        return TCL_ERROR;
    }
    fromInfo = (ARRAYinfo *) adr;

    if ( Tcl_GetLongFromObj( interp, objv[2], &adr ) != TCL_OK ) {
        Tcl_AppendResult( interp, ": failed to read data pointer",
                          (char *) NULL );
        return TCL_ERROR;
    }
    toInfo = (ARRAYinfo *) adr;

    /* Basic check of dimensionality and type */
    if ( fromInfo->type != toInfo->type ) {
        Tcl_SetResult( interp, "Cannot copy arrays when types are different",
                       TCL_VOLATILE );
        return TCL_ERROR;
    }
    if ( fromInfo->el != toInfo->el ) {
        Tcl_SetResult( interp, "Cannot copy arrays with differing sizes",
                       TCL_VOLATILE );
        return TCL_ERROR;
    }

    /* Do the copy */
    nel = gaiaArraySizeOf( fromInfo->type );
    memcpy( toInfo->ptr, fromInfo->ptr, nel * fromInfo->el );
    return TCL_OK;
}

/**
 * Apply an integer mask to an ARRAYinfo struct copying the result
 * into a new array. If given various values from the mask can be
 * selected for application, otherwise all mask values, except BAD
 * are applied.
 *
 * The result is a the memory address of a array struct.
 */
static int gaiaArrayMask( ClientData clientData, Tcl_Interp *interp,
                          int objc, Tcl_Obj *CONST objv[] )
{
    ARRAYinfo *dataInfo;
    ARRAYinfo *maskInfo;
    ARRAYinfo *outInfo;
    Tcl_Obj **listObjv;
    int *values = NULL;
    int i;
    int nvalues = 0;
    long adr;
    void *outPtr = NULL;

    /*  Check arguments */
    if ( objc != 3 && objc != 4 ) {
        Tcl_WrongNumArgs( interp, 1, objv, "data mask {v1 v2 ...}" );
        return TCL_ERROR;
    }

    /*  Get memory addresses */
    if ( Tcl_GetLongFromObj( interp, objv[1], &adr ) != TCL_OK ) {
        Tcl_AppendResult( interp, ": failed to read data pointer",
                          (char *) NULL );
        return TCL_ERROR;
    }
    dataInfo = (ARRAYinfo *) adr;

    if ( Tcl_GetLongFromObj( interp, objv[2], &adr ) != TCL_OK ) {
        Tcl_AppendResult( interp, ": failed to read data pointer",
                          (char *) NULL );
        return TCL_ERROR;
    }
    maskInfo = (ARRAYinfo *) adr;

    /*  Basic check of dimensionality */
    if ( dataInfo->el != maskInfo->el ) {
        Tcl_SetResult( interp, "Cannot mask arrays with differing sizes",
                       TCL_VOLATILE );
        return TCL_ERROR;
    }

    /*  Mask must be integer. */
    if ( maskInfo->type != HDS_INTEGER ) {
        Tcl_SetResult( interp, "Cannot use non integer mask", TCL_VOLATILE );
        return TCL_ERROR;
    }

    /*  Get the selected values, if given. */
    if ( objc == 4 ) {
        if ( Tcl_ListObjGetElements( interp, objv[3], &nvalues, &listObjv )
             != TCL_OK ) {
            return TCL_ERROR;
        }
        values = (int *) malloc( nvalues * sizeof( int ) );
        for ( i = 0; i < nvalues; i++ ) {
            if ( Tcl_GetIntFromObj( interp, listObjv[i], &values[i] )
                 != TCL_OK ) {
                free( values );
                return TCL_ERROR;
            }
        }
    }

    /*  Do the masking, BAD data means outside of the mask, i.e. all
     *  masked data, unless values is set to contain some mask integers. */
    gaiaArrayMaskData( dataInfo, maskInfo, values, nvalues,
                       GAIA_ARRAY_CNFMALLOC, &outPtr );

    /*  Export the result. */
    outInfo = gaiaArrayCreateInfo( outPtr, dataInfo->type, dataInfo->el,
                                   0, 0, 0, 1.0, 0.0, GAIA_ARRAY_CNFMALLOC );
    Tcl_SetObjResult( interp, Tcl_NewLongObj( (long) outInfo ) );

    if ( values != NULL ) {
        free( values );
    }
    return TCL_OK;
}

/**
 * Get the minimum and maximum value in an array.
 */
static int gaiaArrayGetMinMax( ClientData clientData, Tcl_Interp *interp,
                               int objc, Tcl_Obj *CONST objv[] )
{
    ARRAYinfo *info;
    Tcl_Obj *resultObj;
    double max;
    double min;
    long adr;

    /* Check arguments */
    if ( objc != 2 ) {
        Tcl_WrongNumArgs( interp, 1, objv, "array" );
        return TCL_ERROR;
    }

    /* Get memory address */
    if ( Tcl_GetLongFromObj( interp, objv[1], &adr ) != TCL_OK ) {
        Tcl_AppendResult( interp, ": failed to read data pointer",
                          (char *) NULL );
        return TCL_ERROR;
    }
    info = (ARRAYinfo *) adr;
    if ( info ) {

        /* Get the min and max. */
        gaiaArrayMinMax( info, &min, &max );

        /* And report. */
        resultObj = Tcl_GetObjResult( interp );
        Tcl_ListObjAppendElement( interp, resultObj, Tcl_NewDoubleObj( min ) );
        Tcl_ListObjAppendElement( interp, resultObj, Tcl_NewDoubleObj( max ) );

        return TCL_OK;
    }

    Tcl_SetResult( interp, "Cannot determine min and max of an empty array",
                   TCL_VOLATILE );
    return TCL_ERROR;
}
