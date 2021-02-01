#include <stdlib.h>
#include <string.h>
#include "sae_par.h"
#include "dat_par.h"
#include "ndf1.h"
#include "ndf_err.h"
#include "ndf_ast.h"
#include "mers.h"

void ndf1Psnde( const char *str, int nax, const double lbnd[],
                const double ubnd[], AstFrameSet *iwcs, int wcssec,
                double value1[], double value2[], int *nval, int frame1[],
                int frame2[], int isgeo[], int isbnd[], int isdef1[],
                int isdef2[], int *status ){
/*
*+
*  Name:
*     ndf1Psnde

*  Purpose:
*     Parse an NDF dimension bounds expression.

*  Synopsis:
*     void ndf1Psnde( const char *str, int nax, const double lbnd[],
*                     const double ubnd[], AstFrameSet *iwcs, int wcssec,
*                     double value1[], double value2[], int *nval,
*                     int frame1[], int frame2[], int isgeo[],
*                     int isbnd[], int isdef1[], int isdef2[], int *status )

*  Description:
*     This function parses an NDF section bound expression (such as
*     "1:10,2", "3:,,~7" or "31~5,,6") and returns two values specifying
*     the section's bounds in each dimension, together with additional
*     information specifying how the bounds should be calculated from the
*     returned values. Suitable defaults are used where appropriate.

*  Parameters:
*     str
*        Pointer to a null terminated string holding the string containing
*        the expression to be parsed.
*     nax
*        Number of axes for which default bounds are available. If "wcssec"
*        is non-zero, this should be the number of WCS axes. Otherwise it
*        should be the number of pixel axes.
*     lbnd
*        Axis lower bounds (used to calculate defaults). These should be
*        pixel coordinates. The supplied "lbnd" array should have at least
*        "nax" elements.
*     ubnd
*        Axis upper bounds (used to calculate defaults). These should be
*        WCS axis values if "wcssec" is non-zero, and should be pixel
*        coordinates otherwise. The supplied "ubnd" array should have at
*        least "nax" elements.
*     iwcs
*        An AST pointer to the NDF's WCS FrameSet.
*     wcssec
*        If non-zero, then the section specifier uses WCS syntax.
*        Otherwise, it uses the old pixel/axis syntax. In WCS syntax, the
*        supplied "str" string should contain a specification for the
*        bounds on each WCS axis, supplied in the order of the WCS axes.
*        Each bound specification must be given using WCS axis values. The
*        number of bounds specifications must equal the number of WCS axes
*        (supplied in "nax"). If "wcssec" is zero, the supplied "str"
*        string should contain a specification for the bounds on each pixel
*        axis, supplied in the order of the pixel axes. Each bound
*        specification must be given using either pixel indices (integers),
*        or WCS axis values.
*     value1
*        Returned holding the first value specifying section bounds for
*        each dimension. The supplied "value1" array should have at least
*        "NDF__MXDIM" elements.
*     value2
*        Returned holding the second value specifying section bounds for
*        each dimension. The supplied "value2" array should have at least
*        "NDF__MXDIM" elements.
*     *nval
*        Number of axes for which values are returned (cannot exceed
*        NDF__MXDIM).
*     frame1
*        Returned holding the 0 ==> the corresponding "value1" value is to
*        be interpreted as a WCS or axis coordinate value, 1 ==> it is a
*        pixel index, 2 ==> it is a FRACTION value. The supplied "frame1"
*        array should have at least "NDF__MXDIM" elements.
*     frame2
*        Returned holding the 0 ==> the corresponding "value2" value is to
*        be interpreted as a WCS or axis coordinate value, 1 ==> it is a
*        pixel index, 2 ==> it is a FRACTION value. The supplied "frame2"
*        array should have at least "NDF__MXDIM" elements.
*     isgeo
*        If the bounds for an axis are defined by a centre value and
*        width and the width value is a WCS value, then the value
*        returned in "isgeo" indicates if the width in "value2" is a
*        geodesic distance. If not, it will be an axis increment. For
*        instance an RA increment will correspond to a different geodesic
*        distance at different Declinations. The supplied "isgeo" array
*        should have at least "NDF__MXDIM" elements.
*     isbnd
*        Returned holding the non-zero ==> the corresponding "value1" and
*        "value2" values specify the lower and upper bounds of the section
*        directly, zero ==> "value1" specifies the centre of the
*        dimension's extent and "value2" specifies the dimension's size.
*        The supplied "isbnd" array should have at least "NDF__MXDIM"
*        elements.
*     isdef1
*        Returned holding the non-zero ==> the corresponding "value1" value
*        is a default value and was not specified in the supplied string.
*        zero ==> "value1" was specified explicitly in the supplied string.
*        The supplied "isdef1" array should have at least "NDF__MXDIM"
*        elements.
*     isdef2
*        Returned holding the non-zero ==> the corresponding "value2" value
*        is a default value and was not specified in the supplied string.
*        zero ==> "value2" was specified explicitly in the supplied string.
*        The supplied "isdef2" array should have at least "NDF__MXDIM"
*        elements.
*     *status
*        The global status.

*  Notes:
*     -  The number of dimension bounds implied by the expression supplied
*     (one more than the number of separating commas which it contains)
*     must not exceed NDF__MXDIM. An error will be reported if it does. It
*     need not match the number of NDF dimensions supplied.

*  Copyright:
*     Copyright (C) 2018-2021 East Asian Observatory
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
*     29-JAN-2021 (DSB):
*        Added argument "isgeo".

*-
*/

/* Local Variables: */
   char *fld;            /* Dynamic string holding next field */
   const char *pcomma;   /* Pointer to next comma */
   double lbnd0;         /* Default lower dimension bound */
   double ubnd0;         /* Default upper dimension bound */
   int comma;            /* Comma terminated a field? */
   int i1;               /* First character position in field */
   int i2;               /* Last character position in field */
   int ival;             /* Index of current field */
   int lstr;             /* Length of supplied string */

/* Check inherited global status. */
   if( *status != SAI__OK ) return;

/* Initialise. */
   *nval = 0;
   i1 = 0;
   comma = 1;
   lstr = strlen( str );

/* Loop to extract each dimension bound field from the expression. */
   while( ( *status == SAI__OK ) && comma ){

/* If we are still within the bounds of the expression string, then
   search for the end of the next field (the last character before a
   comma or end of string). Note if a comma did not terminate this
   field. */
      if( i1 < lstr ) {
         pcomma = strchr( str + i1, ',' );
         if( !pcomma ) {
            i2 = lstr - 1;
            comma = 0;
         } else {
            i2 = ( pcomma - str ) - 1;
         }

/* If we are outside the bounds of the expression, but have to make one
   more pass to process the (blank) field following a final comma, then
   use the end of string as the end of the field. */
      } else {
         i2 = lstr - 1;
         comma = 0;
      }

/* Increment the count of dimension bounds and report an error if this
   exceeds the maximum number of dimensions. */
      ival = (*nval)++;
      if( wcssec && *nval > nax ) {
         *status = NDF__BNDIN;
         msgSetc( "SECTION", str );
         msgSeti( "MXDIM", nax );
         errRep( " ", "Too many WCS axes given in the NDF section "
                 "expression '(^SECTION)'; the maximum number of WCS axes "
                 "is ^MXDIM.", status );

      } else if( *nval > NDF__MXDIM ) {
         *status = NDF__BNDIN;
         msgSetc( "SECTION", str );
         msgSeti( "MXDIM", NDF__MXDIM );
         errRep( " ", "Too many dimensions given in the NDF section "
                 "expression '(^SECTION)'; the maximum number of NDF "
                 "dimensions is ^MXDIM.", status );

/* Set up values of the default lower and upper bounds for the current
   dimension. */
      } else {
         if( ival < nax ) {
            lbnd0 = lbnd[ ival ];
            ubnd0 = ubnd[ ival ];
         } else {
            lbnd0 = 1.0;
            ubnd0 = 1.0;
         }

/* Initialise "isgeo" to indicate that "value" is not a geodesic distance. */
         isgeo[ ival ] = 0;

/* If the field does not exist (i.e. there are two consecutive commas
   or a comma at the start or end of the string) then use the default
   values for the current dimension. */
         if( i1 > i2 ) {
            value1[ ival ] = lbnd0;
            value2[ ival ] = ubnd0;

            if( wcssec ) {
               frame1[ ival ] = 0;
               frame2[ ival ] = 0;
            } else {
               frame1[ ival ] = 1;
               frame2[ ival ] = 1;
            }

            isbnd[ ival ] = 1;
            isdef1[ ival ] = 1;
            isdef2[ ival ] = 1;

/* Otherwise, get a copy of the of the current dimension field without
   any trailing or leading spaces. */
         } else {
            fld = ndf1Strip( NULL, str, i1, i2, NULL, NULL, status );

/* If the field is blank, then apply the default bounds. */
            if( *status != SAI__OK || *fld == 0 ) {
               value1[ ival ] = lbnd0;
               value2[ ival ] = ubnd0;

               if( wcssec ) {
                  frame1[ ival ] = 0;
                  frame2[ ival ] = 0;
               } else {
                  frame1[ ival ] = 1;
                  frame2[ ival ] = 1;
               }

               isbnd[ ival ] = 1;
               isdef1[ ival ] = 1;
               isdef2[ ival ] = 1;

/* Otherwise, parse the field to determine the values which specify the
   dimension bounds. */
            } else {
               ndf1Psndf( fld, lbnd0, ubnd0, *nval, iwcs, wcssec,
                          value1 + ival, value2 + ival, frame1 + ival,
                          frame2 + ival, isgeo + ival, isbnd + ival,
                          isdef1 + ival, isdef2 + ival, status );

/* Make a contextual error report if an error occurs. */
               if( *status != SAI__OK ) {
                  msgSeti( "NBND", *nval );
                  msgSetc( "SECTION", str );
                  errRep( " ", "Error in dimension ^NBND of the NDF "
                          "section expression '(^SECTION)'.", status );
               }
            }

/* Free resources. */
            fld = astFree( fld );
         }
      }

/* Increment the pointer to the start of the next field and return to
   process it. */
      i1 = i2 + 2;
   }

/* Call error tracing function and exit. */
   if( *status != SAI__OK ) ndf1Trace( "ndf1Psnde", status );

}

