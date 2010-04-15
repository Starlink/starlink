/*
*+
*  Name:
*     pdaD1mach

*  Purpose:
*     C implementation of the D1MACH function

*  Language:
*     C

*  Description:
*     This routine replaces the Fortran version of the D1MACH function
*     so that portability is improved by having a single standard
*     implementation. The various values returned are:
*
*     D1MACH( 1) = B**(EMIN-1), THE SMALLEST POSITIVE MAGNITUDE.
*     D1MACH( 2) = B**EMAX*(1 - B**(-T)), THE LARGEST MAGNITUDE.
*     D1MACH( 3) = B**(-T), THE SMALLEST RELATIVE SPACING.
*     D1MACH( 4) = B**(1-T), THE LARGEST RELATIVE SPACING.
*     D1MACH( 5) = LOG10(B)

*  Authors:
*     UNKNOWN: See PDA_D1MACH Fortran source.
*     PWD: Peter W. Draper
*     {enter_new_authors_here}

*  History:
*     20-APR-2007 (PWD):
*        Original version, based on version in pda_d1mach.f
*     {enter_further_changes_here}

*-
*/

/* Header files. */
/* ============= */
#include <stdio.h>
#include <float.h>
#include <math.h>

double pdaD1mach( int i )
{
    switch( i ) {
       case 1: return DBL_MIN;
       case 2: return DBL_MAX;
       case 3: return DBL_EPSILON/FLT_RADIX;
       case 4: return DBL_EPSILON;
       case 5: return log10( (double)FLT_RADIX );
    }
    fprintf( stderr, "Error: pdaD1mach called with argument out of bounds" );
    return 0.0;
}
