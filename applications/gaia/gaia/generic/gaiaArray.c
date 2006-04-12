/*+
 *   Name:
 *      gaiaArray

 *   Purpose:
 *      Utility routines for handling arrays.

 *   Language:
 *      C

 *  Copyright:
 *     Copyright (C) 2006 Particle Physics and Astronomy Research Council

 *   Authors:
 *      PWD: Peter W. Draper, Starlink - University of Durham

 *   History:
 *      23-MAR-2006 (PWD):
 *         Original version.
 *      {enter_changes_here}
 *-
 */
#include <stdio.h>
#include <sys/types.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>

#include <gaiaArray.h>
#include <prm_par.h>
#include <cnf.h>
#include "byteswap.h"

#define MIN(a,b) ( (a) < (b) ? (a) : (b) )
#define MAX(a,b) ( (a) > (b) ? (a) : (b) )

/* The type "long" may have 64 bits, need to know this for byte-swapping
 * macros */
#if SIZEOF_LONG == 8
#define FITS_ULONG unsigned long
#define FITS_UINT unsigned int
#else
#define FITS_ULONG unsigned long long
#define FITS_UINT unsigned long
#endif

/* Local types as HDS type strings, static for simple export */
static char *hdstypes[] = {
    "_UBYTE", "_BYTE", "_UWORD", "_WORD", "_INTEGER", "_REAL", "_DOUBLE"
};

/* Local prototypes */
static void gaiaArrayDataNormalise( void *inPtr, int type, int nel, 
                                    int isfits, int haveblank, int inBlank );

/**
 * Create an ARRAYinfo structure for a data array. If blank isn't set for your
 * data just set haveblank to 0. Note that NaN will be used for FITS floating
 * point values, so you should set haveblank 1 for those.
 */
ARRAYinfo *gaiaArrayCreateInfo( void *ptr, int type, long el, int isfits,
                                int haveblank, int blank )
{
    ARRAYinfo *info = (ARRAYinfo *) malloc( sizeof( ARRAYinfo ) );
    info->ptr = ptr;
    info->type = type;
    info->el = el;
    info->isfits = isfits;
    info->haveblank = haveblank;
    info->blank = blank;

    return info;
}

/**
 * Free an ARRAYinfo structure.
 */
void gaiaArrayFreeInfo( ARRAYinfo *info )
{
    if ( info != NULL ) {
        free( info );
    }
}

/**
 * Convert an HDS data type into a local enum value. Returns HDS_UNKNOWN if
 * the type cannot be understood (should be _BYTE, _UBYTE, _WORD, _UWORD,
 * _INTEGER, _REAL or _DOUBLE, case-insentive allowed and truncation to
 * uniqueness).
 */
int gaiaArrayHDSType( char *typePtr )
{
    int type = HDS_UNKNOWN;

    if ( typePtr[0] == '_' ) {
        if ( typePtr[1] == 'u' || typePtr[1] == 'U' ) {
            if ( typePtr[2] == 'b' || typePtr[2] == 'B' ) {
                type = HDS_UBYTE;
            }
            else if ( typePtr[2] == 'w' || typePtr[2] == 'W' ) {
                type = HDS_UWORD;
            }
        }
        else if ( typePtr[1] == 'b' || typePtr[1] == 'B' ) {
            type = HDS_BYTE;
        }
        else if ( typePtr[1] == 'w' || typePtr[1] == 'W' ) {
            type = HDS_WORD;
        }
        else if ( typePtr[1] == 'i' || typePtr[1] == 'I' ) {
            type = HDS_INTEGER;
        }
        else if ( typePtr[1] == 'r' || typePtr[1] == 'R' ) {
            type = HDS_REAL;
        }
        else if ( typePtr[1] == 'd' || typePtr[1] == 'D' ) {
            type = HDS_DOUBLE;
        }
    }
    return type;
}

/**
 * Convert a FITS bitpix into a local enum value. Returns HDS_UNKNOWN if
 * the type cannot be understood (should be _BYTE, _UBYTE, _WORD, _UWORD,
 * _INTEGER, _REAL or _DOUBLE).
 */
int gaiaArrayFITSType( int bitpix )
{
    int type = HDS_UNKNOWN;

    switch (bitpix)
    {
    case -8:
        /* This is not standard */
        type = HDS_BYTE;
        break;
    case 8:
        type = HDS_UBYTE;
        break;
    case -16:
        type = HDS_UWORD;
        break;
    case 16:
        type = HDS_WORD;
        break;
    case -32:
        type = HDS_REAL;
        break;
    case 32:
        type = HDS_INTEGER;
        break;
    case -64:
        type = HDS_DOUBLE;
        break;
    case 64:
        /* This is not supported, should be HDS_LONG... */
        type = HDS_UNKNOWN;
        break;
    }
    return type;
}

/**
 * Convert a local enum type into an HDS string type. Returns _UBYTE if type
 * is not known.
 */
char const *gaiaArrayTypeToHDS( int type )
{
    if ( type >= 0 && type < 7 ) {
        return hdstypes[type];
    }
    return "_UBYTE";
}

/**
 *  Return the sizeof() of a known type.
 */
static size_t gaiaArraySizeOf( int type )
{
    switch ( type )
    {
        case HDS_DOUBLE :
            return sizeof( double );

        case HDS_REAL :
            return sizeof( float );

        case HDS_INTEGER :
            return sizeof( int );

        case HDS_WORD :
            return sizeof( short );

        case HDS_UWORD :
            return sizeof( unsigned short );

        case HDS_BYTE :
            return sizeof( char );

    }
    /* Default, HDS_UBYTE */
    return sizeof( unsigned char );
}

/**
 *  Convert an array from a supported type into double precision and return
 *  the result in a simple pre-allocated array. Any HDS BAD values to be
 *  replaced with the given value. Assumes data are in native representation,
 *  that's machine byte order and have HDS bad values.
 */
void gaiaArrayToDouble( ARRAYinfo *info, double badValue, double *outPtr )
{
    void *inPtr = info->ptr;
    int nel = info->el;
    int type = info->type;

    /* Define loop as macro to save typing and maintenance */
#define CONVERT_AND_COPY(type,badFlag)    \
{                                         \
    double *dataPtr = outPtr;             \
    int i;                                \
    type *fromPtr = (type *) inPtr;       \
    type value;                           \
    for ( i = 0; i < nel; i++ ) {         \
        value = *fromPtr++;               \
        if ( value == badFlag ) {         \
            *dataPtr++ = badValue;        \
        }                                 \
        else {                            \
            *dataPtr++ = (type) value;    \
        }                                 \
    }                                     \
}
    switch ( type )
    {
        case HDS_DOUBLE:
            CONVERT_AND_COPY(double,VAL__BADD)
        break;

        case HDS_REAL:
            CONVERT_AND_COPY(float,VAL__BADR)
        break;

        case HDS_INTEGER:
            CONVERT_AND_COPY(int,VAL__BADI)
        break;

        case HDS_WORD:
            CONVERT_AND_COPY(short,VAL__BADW)
        break;

        case HDS_UWORD:
            CONVERT_AND_COPY(unsigned short,VAL__BADUW)
        break;

        case HDS_BYTE:
            CONVERT_AND_COPY(char,VAL__BADB)
        break;

        case HDS_UBYTE:
            CONVERT_AND_COPY(unsigned char,VAL__BADUB)
        break;
    }
#undef CONVERT_AND_COPY
}

/**
 *  Name:
 *     gaiaArrayImageFromCube
 *
 *  Purpose:
 *     Given an array of 3 significant dimensions, in a supported data type,
 *     extract a 2D image section and return the data in that section in a
 *     simple array. The dataType should be one of the enumerations HDS_
 *     defined in gaiaArray.h (these correspond to the HDS data types). No
 *     changes to the underlying data representation are made (no byte
 *     swapping or bad value transformations).
 *
 *  Arguments:
 *     info
 *         Pointer to the cube ARRAYinfo structure.
 *     dims[3]
 *         The dimensions of the cube.
 *     axis
 *         The axis that will be lost. One of 0, 1, 2. Losing the last axis is
 *         fastest.
 *     index
 *         The index of the plane that will be extracted (along axis "axis").
 *     cnfmalloc
 *         Whether to use cnfMalloc to allocate the image data. Otherwise
 *         malloc will be used.
 *     outPtr
 *         a pointer to a pointer that will point at the extracted image on
 *         exit. This memory is allocated using malloc or cnfMalloc as
 *         determined by the final argument. Freeing it is the responsibility
 *         of the caller.
 *     nel
 *         number of elements extracted (number of type elements in outPtr).
 */
void gaiaArrayImageFromCube( ARRAYinfo *info, int dims[3], int axis,
                             int index, int cnfmalloc, void **outPtr,
                             int *nel )
{
    void *inPtr = info->ptr;
    int type = info->type;

    if ( axis == 2 ) {

        /* If we're losing the last dimension, then this is just a memcpy. */
        char *ptr;
        size_t length;
        size_t offset;

        *nel = (size_t) dims[0] * (size_t) dims[1];
        length = (*nel) * gaiaArraySizeOf( type );

        if ( cnfmalloc == 1 ) {
            *outPtr = cnfMalloc( length );
        }
        else {
            *outPtr = malloc( length );
        }

        /* Get the offset into cube of first pixel (in bytes). */
        offset = length * (size_t) index;
        ptr = ((char *) inPtr) + offset;

        /* And copy the memory */
        memcpy( *outPtr, ptr, length );
    }
    else {

        /* Noncontiguous memory, so need to pick it out pixel by pixel */
        int axis1;
        int axis2;
        int i;
        int indices[3];
        int j;
        int k;
        int l;
        int offset;
        int strides[3];
        size_t length;

        /* Pick out axes we're keeping and set the index of the image to pick
         * out along axis "axis" */
        indices[axis] = index;
        if ( axis == 0 ) {
            axis1 = 1;
            axis2 = 2;
        }
        else {
            axis1 = 0;
            axis2 = 2;
        }

        /* Allocate the memory */
        *nel = (size_t) dims[axis1] * (size_t) dims[axis2];
        length = (*nel) * gaiaArraySizeOf( type );;

        if ( cnfmalloc == 1 ) {
            *outPtr = cnfMalloc( length );
        }
        else {
            *outPtr = malloc( length );
        }

        /* Get the strides for stepping around dimensions */
        gaiaArrayGetStrides( 3, dims, strides );

        /* Copy the image, use type switch to keep pointer arithmetic simple.
         * Use a macro to keep repeated code under control */
#define EXTRACT_AND_COPY(type)                    \
{                                                 \
    type *fromPtr = (type *) inPtr;               \
    type *toPtr = (type *) *outPtr;               \
    k = 0;                                        \
    for ( i = 0; i < dims[axis2]; i++ ) {         \
        indices[axis2] = i;                       \
        for ( j = 0; j < dims[axis1]; j++ ) {     \
            indices[axis1] = j;                   \
            offset = 0;                           \
            for ( l = 0; l < 3; l++ ) {           \
                offset += strides[l] * indices[l];\
            }                                     \
            toPtr[k++] = fromPtr[offset];         \
        }                                         \
    }                                             \
}
        switch ( type )
        {
            case HDS_DOUBLE:
                EXTRACT_AND_COPY(double)
            break;

            case HDS_REAL:
                EXTRACT_AND_COPY(float)
            break;

            case HDS_INTEGER:
                EXTRACT_AND_COPY(int)
            break;

            case HDS_WORD:
                EXTRACT_AND_COPY(short)
            break;

            case HDS_UWORD:
                EXTRACT_AND_COPY(unsigned short)
            break;

            case HDS_BYTE:
                EXTRACT_AND_COPY(char)
            break;

            case HDS_UBYTE:
                EXTRACT_AND_COPY(unsigned char)
            break;
        }
    }
#undef EXTRACT_AND_COPY
}

/**
 *  Name:
 *     gaiaArraySpectrumFromCube
 *
 *  Purpose:
 *     Given an array of 3 significant dimensions, in a supported data type,
 *     extract a 1D spectral section and return the data in that section in a
 *     simple array. The returned data will be byte-swapped and bad-value
 *     transformed if necessary.
 *
 *  Arguments:
 *     info
 *         Pointer to the cube structure.
 *     dims[3]
 *         The dimensions of the cube.
 *     axis
 *         The axis that will be extracted. One of 0, 1, 2. Extracting from
 *         the first axis is fastest.
 *     arange[2]
 *         A range (lower and upper indices) along axis for the extraction,
 *         NULL if whole of axis is to be returned.
 *     index1
 *     index2
 *         The indices of the spectrum to extract (these are along the two
 *         axes which are not "axis").
 *     cnfmalloc
 *         Whether to use cnfMalloc to allocate the image data. Otherwise
 *         malloc will be used.
 *     outPtr
 *         a pointer to a pointer that will point at the extracted spectrum on
 *         exit. This memory is allocated using malloc or cnfMalloc as
 *         determined by the cnfMalloc argument. Freeing it is the
 *         responsibility of the caller, or you can call gaiaArrayFree.
 *     nel
 *         the number of elements extracted
 *
 *  Notes:
 *     If the spectrum isn't within the array bounds then an empty spectrum
 *     (full of zeros) will be returned.
 */
void gaiaArraySpectrumFromCube( ARRAYinfo *info, int dims[3], int axis,
                                int arange[2], int index1, int index2,
                                int cnfmalloc, void **outPtr, int *nel )
{
    int lower;
    int outside = 0;
    int upper;
    size_t length;
    void *inPtr = info->ptr;
    int type = info->type;

    /* Allocate memory for spectrum. Only need "arange" spanning values when
     * that is set. */
    if ( arange == NULL ) {
        lower = 0;
        upper = dims[axis];
        *nel = (size_t) dims[axis];
    }
    else {
        lower = MAX( 0,          MIN( arange[0], arange[1] ) );
        upper = MIN( dims[axis], MAX( arange[1], arange[0] ) ) + 1;
        *nel = upper - lower;
    }
    length = (*nel) * gaiaArraySizeOf( type );
    if ( cnfmalloc == 1 ) {
        *outPtr = cnfMalloc( length );
    }
    else {
        *outPtr = malloc( length );
    }

    /* Check bounds. */
    if ( index1 < 0 || index2 < 0 ) {
        outside = 1;
    }
    else {
        if ( axis == 0 ) {
            if ( index1 >= dims[1] || index2 >= dims[2] ) {
                outside = 1;
            }
        }
        else if ( axis == 1 ) {
            if ( index1 >= dims[0] || index2 >= dims[2] ) {
                outside = 1;
            }
        }
        else if ( axis == 2 ) {
            if ( index1 >= dims[0] || index2 >= dims[1] ) {
                outside = 1;
            }
        }
    }

    if ( outside ) {

        /* Out of bounds, so return empty spectrum */
        memset( *outPtr, 0, length );
    }
    else if ( axis == 0 ) {

        /* If we're extracting from the first dimension then this is just a
         * memcpy of a contiguous piece of memory. */
        char *ptr;
        int strides[3];
        size_t offset;

        /* Get the offset into cube of first pixel on the line. */
        gaiaArrayGetStrides( 3, dims, strides );
        offset = (size_t) strides[1] * index1 + (size_t) strides[2] * index2;

        /* Correct for arange offset. */
        offset += lower;

        /* Set the address of first pixel, remember to include sizeof(type). */
        ptr = ((char *) inPtr) + ( offset * gaiaArraySizeOf( type ) );

        /* And copy the memory */
        memcpy( *outPtr, ptr, length );
    }
    else {

        /* Non-contiguous memory, so need to pick it out pixel by pixel */
        int i;
        int indices[3];
        int j;
        int k;
        int offset;
        int strides[3];

        /* Get the strides for stepping around cube with these dimensions. */
        gaiaArrayGetStrides( 3, dims, strides );

        /* Set up indices to select the spectral line. */
        indices[0] = index1;
        if ( axis == 1 ) {
            indices[1] = 0;
            indices[2] = index2;
        }
        else {
            indices[1] = index2;
            indices[2] = 0;
        }

        /* Copy the spectrum, use type switch to keep pointer arithmetic
         * simple and a macro to define repeated code. */
#define EXTRACT_AND_COPY(type)                      \
{                                                   \
    type *fromPtr = (type *) inPtr;                 \
    type *toPtr = (type *) *outPtr;                 \
    for ( k = 0, i = lower; i < upper; k++, i++ ) { \
        offset = 0;                                 \
        indices[axis] = i;                          \
        for ( j = 0; j < 3; j++ ) {                 \
            offset += strides[j] * indices[j];      \
        }                                           \
        toPtr[k] = fromPtr[offset];                 \
    }                                               \
}
        switch ( type )
        {
            case HDS_DOUBLE:
                EXTRACT_AND_COPY(double)
            break;
            case HDS_REAL:
                EXTRACT_AND_COPY(float)
            break;
            case HDS_INTEGER:
                EXTRACT_AND_COPY(int)
            break;
            case HDS_WORD:
                EXTRACT_AND_COPY(short)
            break;
            case HDS_UWORD:
                EXTRACT_AND_COPY(unsigned short)
            break;
            case HDS_BYTE:
                EXTRACT_AND_COPY(char)
            break;
            case HDS_UBYTE:
                EXTRACT_AND_COPY(unsigned char)
            break;
        }
    }
#undef EXTRACT_AND_COPY

    /* Normalise the data to remove byte-swapping and unrecognised 
     * BAD values */
    gaiaArrayDataNormalise( *outPtr, type, *nel, info->isfits, 
                            info->haveblank, info->blank );
}

/**
 * Return a set of column major (Fortran/FITS/NDF) order strides for
 * stepping around a vectorised array of the given dimensionality
 *
 * Once returned you can access the array element data(i,j,k) element, as
 * in the following code segment:
 *
 *    int indices[3];
 *    indices[0] = i;
 *    indices[1] = j;
 *    indices[2] = k;
 *    int offset = 0;
 *    for ( int l = 0; l < 3; l++ ) {
 *       offset += strides[l] * indices[l];
 *    }
 *    value = data[offset];
 */
void gaiaArrayGetStrides( int ndims, int dims[], int strides[] )
{
    int i;
    int count = 1;
    for ( i = 0; i < ndims; i++ ) {
        strides[i] = count;
        count *= dims[i];
    }
}

/**
 * Free data previously allocated by these routines and associated with an 
 * ARRAYinfo structure.
 */
void gaiaArrayFree( ARRAYinfo *info, int cnfMalloc )
{
    if ( info->ptr != NULL ) {
        if ( cnfMalloc ) {
            cnfFree( info->ptr );
        }
        else {
            free( info->ptr );
        }
        info->ptr = NULL;
    }
    else {
        fprintf( stderr, "gaiaArray: attempt to double free %p\n", info );
    }
}

/**
 *  Normalise an array if this machine does not have native FITS ordering and
 *  the data is in FITS format. Also transform FITS bad values into HDS ones.
 */
void gaiaArrayNormalise( ARRAYinfo *info )
{
    gaiaArrayDataNormalise( info->ptr, info->type, info->el, 
                            info->isfits, info->haveblank, info->blank );
}
 
/**
 *  Normalise an array if this machine does not have native FITS ordering and
 *  the data is in FITS format. Also transform FITS bad values into HDS ones.
 */
static void gaiaArrayDataNormalise( void *inPtr, int type, int nel, 
                                    int isfits, int haveblank, int inBlank )
{
    int i;
    
    /*  Only applies to FITS data */
    if ( ! isfits ) return;

    /* Byte swap first, note we do this using macros that require integers,
     * so cannot merge with bad checking. */
#if BIGENDIAN
    /* Nothing to do */
#else
    switch ( type )
    {
        case HDS_DOUBLE: {
            double *ptr = (double *)inPtr;
            for ( i = 0; i < nel; i++ ) {
                ptr[i] = SWAP_DOUBLE( ptr[i] );
            }
        }
        break;

        case HDS_REAL: {
            float *ptr = (float *)inPtr;
            for ( i = 0; i < nel; i++ ) {
                ptr[i] = SWAP_FLOAT( ptr[i] );
            }
        }
        break;

        case HDS_INTEGER: {
            int *ptr = (int *)inPtr;
            for ( i = 0; i < nel; i++ ) {
                ptr[i] = SWAP_INT( ptr[i] );
            }
        }
        break;

        case HDS_WORD: {
            short *ptr = (short *)inPtr;
            for ( i = 0; i < nel; i++ ) {
                ptr[i] = SWAP_SHORT( ptr[i] );
            }
        }
        break;

        case HDS_UWORD: {
            unsigned short *ptr = (unsigned short *)inPtr;
            unsigned short value;
            for ( i = 0; i < nel; i++ ) {
                value = ptr[i];
                ptr[i] = SWAP16( value );
            }
        }
        break;
    }
#endif

    if ( isfits ) return;

    /* BAD value transformation. */
    switch ( type )
    {
        case HDS_DOUBLE: {
            double *ptr = (double *)inPtr;
            for ( i = 0; i < nel; i++ ) {
                if ( isnan( ptr[i] ) ) {
                    ptr[i] = VAL__BADD;
                }
            }
        }
        break;

        case HDS_REAL: {
            float *ptr = (float *)inPtr;
            for ( i = 0; i < nel; i++ ) {
                if ( isnan( ptr[i] ) ) {
                    ptr[i] = VAL__BADR;
                }
            }
        }
        break;

        case HDS_INTEGER: {
            if ( haveblank ) {
                int *ptr = (int *)inPtr;
                for ( i = 0; i < nel; i++ ) {
                    if ( ptr[i] == inBlank ) {
                        ptr[i] = VAL__BADI;
                    }
                }
            }
        }
        break;

        case HDS_WORD: {
            if ( haveblank ) {
                short blank = (short) inBlank;
                short *ptr = (short *)inPtr;
                for ( i = 0; i < nel; i++ ) {
                    if ( ptr[i] == blank ) {
                        ptr[i] = VAL__BADW;
                    }
                }
            }
        }
        break;

        case HDS_UWORD: {
            if ( haveblank ) {
                unsigned short blank = (unsigned short) inBlank;
                unsigned short *ptr = (unsigned short *)inPtr;
                for ( i = 0; i < nel; i++ ) {
                    if ( ptr[i] == blank ) {
                        ptr[i] = VAL__BADUW;
                    }
                }
            }
        }
        break;

        case HDS_BYTE: {
            if ( haveblank ) {
                char blank = (char) inBlank;
                char *ptr = (char *)inPtr;
                for ( i = 0; i < nel; i++ ) {
                    if ( ptr[i] == blank ) {
                        ptr[i] = VAL__BADB;
                    }
                }
            }
        }
        break;

        case HDS_UBYTE: {
            if ( haveblank ) {
                unsigned char blank = (unsigned char) inBlank;
                unsigned char *ptr = (unsigned char *)inPtr;
                for ( i = 0; i < nel; i++ ) {
                    if ( ptr[i] == blank ) {
                        ptr[i] = VAL__BADUB;
                    }
                }
            }
        }
        break;
    }
}
