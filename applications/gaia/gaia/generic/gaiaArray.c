/*+
 *   Name:
 *      gaiaArray
 *
 *   Purpose:
 *      Utility routines for handling arrays.
 *
 *   Language:
 *      C
 *
 * Notes:
 *
 *  Copyright:
 *     Copyright (C) 2006 Particle Physics and Astronomy Research Council
 *
 *   Authors:
 *      PWD: Peter W. Draper, Starlink - University of Durham
 *
 *   History:
 *      23-MAR-2006 (PWD):
 *         Original version.
 *      {enter_changes_here}
 *-
 */

#include <gaiaArray.h>
#include <prm_par.h>


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
 *  Convert an array from a HDS type into double precision. Any BAD values to
 *  be replaced with the given value.
 */
void gaiaArrayToDouble( void *inPtr, int nel, int type, double badValue, 
                        double *outPtr )
{
    double *dataPtr = outPtr;
    int i;

    switch ( type )
    {
        case HDS_DOUBLE : {
            double *fromPtr = (double *) inPtr;
            double value;
            for ( i = 0; i < nel; i++ ) {
                value = *fromPtr++;
                if ( value == VAL__BADD ) {
                    *dataPtr++ = badValue;
                }
                else {
                    *dataPtr++ = (double) value;
                }
            }
        }
        break;

        case HDS_REAL : {
            float *fromPtr = (float *) inPtr;
            float value;
            for ( i = 0; i < nel; i++ ) {
                value = *fromPtr++;
                if ( value == VAL__BADR ) {
                    *dataPtr++ = badValue;
                }
                else {
                    *dataPtr++ = (double) value;
                }
            }
        }
        break;
        
        case HDS_INTEGER : {
            int *fromPtr = (int *) inPtr;
            int value;
            for ( i = 0; i < nel; i++ ) {
                value = *fromPtr++;
                if ( value == VAL__BADI ) {
                    *dataPtr++ = badValue;
                }
                else {
                    *dataPtr++ = (double) value;
                }
            }
        }
        break;

        case HDS_WORD : {
            short *fromPtr = (short *) inPtr;
            short value;
            for ( i = 0; i < nel; i++ ) {
                value = *fromPtr++;
                if ( value == VAL__BADW ) {
                    *dataPtr++ = badValue;
                }
                else {
                    *dataPtr++ = (double) value;
                }
            }
        }
        break;

        case HDS_UWORD : {
            unsigned short *fromPtr = (unsigned short *) inPtr;
            unsigned short value;
            for ( i = 0; i < nel; i++ ) {
                value = *fromPtr++;
                if ( value == VAL__BADUW ) {
                    *dataPtr++ = badValue;
                }
                else {
                    *dataPtr++ = (double) value;
                }
            }
        }
        break;

        case HDS_BYTE : {
            char *fromPtr = (char *) inPtr;
            char value;
            for ( i = 0; i < nel; i++ ) {
                value = *fromPtr++;
                if ( value == VAL__BADB ) {
                    *dataPtr++ = badValue;
                }
                else {
                    *dataPtr++ = (double) value;
                }
            }
        }
        break;

        case HDS_UBYTE : {
            unsigned char *fromPtr = (unsigned char *) inPtr;
            unsigned char value;
            for ( i = 0; i < nel; i++ ) {
                value = *fromPtr++;
                if ( value == VAL__BADUB ) {
                    *dataPtr++ = badValue;
                }
                else {
                    *dataPtr++ = (double) value;
                }
            }
        }
        break;
    }
}

/**
 *  Name:
 *     arraySection
 *
 *  Purpose:
 *     Given an array, in a supported data type, extract a section in the NDF
 *     sense and return the data in that section. The dataType should be one
 *     of the enumerations HDS_ defined in gaiaArray.h (these correspond to
 *     the HDS data types). 
 * 
 *     The current size of the array is defined as dims[ndim], and the
 *     section that is required is lbnd[ndim] ubnd[ndim]. These are defined as
 *     "ints", since that's all you can get through the NDF interface.
 */
void gaiaArraySection( void *dataPtr, char dataType, int ndim, int dims[],
                       int lbnd[], int ubnd[], void **sectionPtr )
{



}

