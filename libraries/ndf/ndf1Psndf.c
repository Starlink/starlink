#include "dat_par.h"
#include "mers.h"
#include "ndf1.h"
#include "ndf_ast.h"
#include "ndf_err.h"
#include "sae_par.h"
#include "star/util.h"
#include <math.h>
#include <stdlib.h>
#include <string.h>

void ndf1Psndf( const char *str, double lbnd, double ubnd, int axis,
                AstFrameSet *iwcs, int wcssec, double *value1, double *value2,
                int *frame1, int *frame2, int *isbnd, int *isdef1,
                int *isdef2, int *status ){
/*
*+
*  Name:
*     ndf1Psndf

*  Purpose:
*     Parse an NDF dimension bound field.

*  Synopsis:
*     void ndf1Psndf( const char *str, double lbnd, double ubnd, int axis,
*                     AstFrameSet *iwcs, int wcssec, double *value1,
*                     double *value2, int *frame1, int *frame2, int *isbnd,
*                     int *isdef1, int *isdef2, int *status )

*  Description:
*     This function parses a dimension bound field for an NDF to determine
*     two values which specify the bounds for a dimension when selecting an
*     NDF section. The lower and upper bounds may be separated in the
*     normal way by a colon or semi-colon (e.g. "10:20"), or by "~" (e.g.
*     "31~10"). The latter indicates that the bounds should be centred on
*     the first value and have a dimension size equal to the second value.
*     Suitable default values are returned if either or both halves of the
*     field are omitted (e.g. "100:", ":100", ":", "~15", "33~" etc.). If
*     no field separator is present, then the upper bound is set to equal
*     the lower bound (unless the string is blank, which is equivalent to
*     ":"). If the values of bounds are supplied using integer format, then
*     they are interpreted as pixel indices. Otherwise, they are
*     interpreted as value in the current Frame of the supplied WCS
*     FrameSet.

*  Parameters:
*     str
*        Pointer to a null terminated string holding the string to be
*        parsed.
*     lbnd
*        Default lower axis bound. This should be a WCS axis value if
*        "wcssec" is non-zero, and a pixel coordinate otherwise.
*     ubnd
*        Default upper axis bound. This should be a WCS axis value if
*        "wcssec" is non-zero, and a pixel coordinate otherwise.
*     axis
*        The 1-based index of the axis number to which "str" relates. If
*        "wcssec" is zero, then the "axis" value is the index of a pixel
*        axis. Otherwise, it is the index of a WCS axis.
*     iwcs
*        An AST pointer to the NDF's WCS FrameSet.
*     wcssec
*        If non-zero, then the section specifier uses "WCS syntax".
*        Otherwise, it uses the old pixel/axis syntax. In WCS syntax, the
*        supplied "str" string should contain a specification for the
*        bounds on each WCS axis, supplied in the order of the WCS axes.
*        Each bound specification must be given using WCS axis values. The
*        number of bounds specifications must equal the number of WCS axes.
*        If "wcssec" is zero, the supplied "str" string should contain a
*        specification for the bounds on each pixel axis, supplied in the
*        order of the pixel axes. Each bound specification must be given
*        using either pixel indices (integers), or WCS values (non-
*        integers).
*     *value1
*        Returned holding the first value specifying the dimension bounds.
*     *value2
*        Returned holding the second value specifying the dimension bounds.
*     *frame1
*        Returned holding the 0 ==> "value1" is to be interpreted as a WCS
*        or axis coordinate value, 1 ==> it is a pixel index, 2 ==> it is a
*        FRACTION value.
*     *frame2
*        Returned holding the 0 ==> "value2" is to be interpreted as a WCS
*        or axis coordinate value, 1 ==> it is a pixel index, 2 ==> it is a
*        FRACTION value.
*     *isbnd
*        Returned holding the whether "value1" and "value2" specify the
*        lower and upper bounds directly (i.e. non-zero ==> a ":" separator
*        was given or implied, whereas zero ==> a "~" separator was given).
*     *isdef1
*        Returned holding the non-zero ==> the "value1" value is a default
*        value and was not specified in the supplied string. zero ==>
*        "value1" was specified explicitly in the supplied string.
*     *isdef2
*        Returned holding the non-zero ==> the "value1" value is a default
*        value and was not specified in the supplied string. zero ==>
*        "value1" was specified explicitly in the supplied string.
*     *status
*        The global status.

*  Notes:
*     -  The values obtained by parsing the string are not constrained to
*     lie within the NDF bounds. The lower bound returned may also exceed
*     the upper bound.

*  Copyright:
*     Copyright (C) 2018 East Asian Observatory
*     All rights reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or modify
*     it under the terms of the GNU General Public License as published by
*     the Free Software Foundation; either version 2 of the License, or (at
*     your option) any later version.
*
*     This program is distributed in the hope that it will be useful,but
*     WITHOUT ANY WARRANTY; without even the implied warranty of
*     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
*     General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 51 Franklin Street,Fifth Floor, Boston, MA
*     02110-1301, USA

*  Authors:
*     RFWS: R.F. Warren-Smith (STARLINK)
*     DSB: David S. Berry (EAO)

*  History:
*     3-APR-2019 (DSB):
*        Original version, based on equivalent Fortran function by RFWS.

*-
*/

/* Local Variables: */
   char c;               /* Original separator character */
   char *lstr;           /* Copy of str without leading or trailing spaces */
   char *psep;           /* Pointer to separator character */
   char attr[ 20 ];      /* AST attribute name */
   double def1;          /* Default first value */
   double def2;          /* Default second value */
   size_t f;             /* Position of first non-blank character */
   size_t isep;          /* Character position of separator */
   size_t l;             /* Position of last non-blank character */

/* Check inherited global status. */
   if( *status != SAI__OK ) return;

/* Create a copy of the string excluding any leading or trailing spaces. */
   lstr = ndf1Strip( NULL, str, 1, 0, &l, &f, status );
   if( *status == SAI__OK ) {

/* If the string is blank, then return the default field values (a ":"
   separator being implied). */
      if( l == 0 ) {
         *value1 = lbnd;
         *value2 = ubnd;

         if( wcssec ) {
            *frame1 = 0;
            *frame2 = 0;
         } else {
            *frame1 = 1;
            *frame2 = 1;
         }

         *isbnd = 1;
         *isdef1 = 1;
         *isdef2 = 1;

/* Otherwise, set "f" and "l" so that they are the indices of the first and
   last non-blank characters in the local copy ("lstr"). */
      } else {
         f = 0;
         l--;

/* Locate the separator between the two values. We use ";" as an alternative
   to ":" because ":" is used as a separator within formatted angle and time
   values. */
         psep = strchr( lstr, '~' );
         if( !psep ) psep = strchr( lstr, ';' );
         if( !psep ) psep = strchr( lstr, ':' );

/* Determine if the separator is a ":" either explicitly or by implication. */
         *isbnd = 1;
         if( psep ) {
            *isbnd = ( *psep == ':' ) || ( *psep == ';' );
            isep = psep - lstr;
         } else {
            isep = l + 1;
         }

/* Set up suitable defaults for each value, depending on which separator
   was found. */
         if( *isbnd ) {
            def1 = lbnd;
            def2 = ubnd;

         } else if( !wcssec ) {
            def1 = ( lbnd + ubnd )/2.0;
            def2 = ubnd - lbnd + 1.0;

         } else {
            def2 = 0.5*astAxDistance( iwcs, axis, lbnd, ubnd );
            def1 = astAxOffset( iwcs, axis, lbnd, def2 );
         }

/* If the separator appears at the start, then use the default first
   value. */
         if( isep == 0 ) {
            *value1 = def1;

            if( wcssec ) {
               *frame1 = 0;
            } else {
               *frame1 = 1;
            }

            *isdef1 = 1;

/* Otherwise, parse the string in front of the separator to obtain the
   first bound, supplying the appropriate default. Temporarily terminate
   the string by replacing the separator with a terminating null. */
         } else {
            c = lstr[ isep ];
            lstr[ isep ] = 0;
            ndf1Psndb( lstr, def1, axis, iwcs, wcssec, value1, frame1,
                       isdef1, status );
            lstr[ isep ] = c;
         }

/* If there is no separator present, then the second value equals the
   first value. */
         if( isep > l ) {
            *value2 = *value1;
            *frame2 = *frame1;
            *isdef2 = *isdef1;

/* Otherwise, if the separator appears at the end of the string, then
   use the default second value. */
         } else if( isep == l ) {
            *value2 = def2;

            if( wcssec ) {
               *frame2 = 0;
            } else {
               *frame2 = 1;
            }

            *isdef2 = 1;

/* Otherwise, parse the string which follows the separator to determine
   the second value. */
         } else {
            ndf1Psndb( lstr + isep + 1, def2, axis, iwcs, wcssec, value2,
                       frame2, isdef2, status );
         }
      }

/* If no error has occurred and the second value obtained specifies the
   extent of the dimension (rather than its upper bound), then check
   that this extent is not negative. */
      if( *status == SAI__OK ) {
         if( !*isbnd ) {

/* If the extent is in pixels, then the nearest integer value must be
   positive. */
            if( *frame2 == 1 && NDF_NINT( *value2 ) <= 0 ) {
               *status = NDF__BNDIN;
               errRep( " ", "Invalid dimension extent specified; a positive "
                       "number of pixels is required.", status );

/* If the extent is in WCS coords, then the value itself must be
   non-negative. */
            } else if( wcssec && *value2 < 0 ) {
               sprintf( attr, "Label(%d)", axis );
               msgSetc( "L", astGetC( iwcs, attr ) );
               *status = NDF__BNDIN;
               errRep( " ", "Invalid dimension extent specified; a positive "
                       "increment in ^L is required.", status );

/* If the extent is in axis units, then we can also allow a value of
   zero (which translates into an extent of one pixel). */
            } else if( ( *frame2 == 0 ) && ( *value2 < 0.0 ) ) {
               *status = NDF__BNDIN;
               errRep( " ", "Invalid dimension extent specified; value must "
                       "not be negative.", status );
            }
         }
      }
   }

/* Free resources. */
   lstr = astFree( lstr );

/* Call error tracing function and exit. */
   if( *status != SAI__OK ) ndf1Trace( "ndf1Psndf", status );

}

