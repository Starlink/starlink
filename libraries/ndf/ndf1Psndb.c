#include <stdlib.h>
#include <string.h>
#include "sae_par.h"
#include "ndf_err.h"
#include "ndf_ast.h"
#include "ndf1.h"
#include "ast.h"
#include <string.h>
#include "mers.h"
#include "star/util.h"

void ndf1Psndb( const char *str, double def, int axis, AstFrameSet *iwcs,
                int wcssec, double *value, int *frame, int *isdef,
                int *isgeo, int *status ){
/*
*+
*  Name:
*     ndf1Psndb

*  Purpose:
*     Parse an NDF dimension bound.

*  Synopsis:
*     void ndf1Psndb( const char *str, double def, int axis,
*                     AstFrameSet *iwcs, int wcssec, double *value,
*                     int *frame, int *isdef, int *isgeo, int *status )

*  Description:
*     This function parses a string representing an upper or lower
*     dimension bound of an NDF section. If the string is blank, then a
*     default value is returned. Leading and trailing spaces are ignored.

*  Parameters:
*     str
*        Pointer to a null terminated string holding the string to be
*        parsed.
*     def
*        Default value to be returned if the string is blank.
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
*     *value
*        Returned holding the dimension bound value.
*     *frame
*        Returned holding the 0 ==> "value" is to be interpreted as a WCS
*        or axis coordinate value, 1 ==> it is a pixel index, 2 ==> it is a
*        FRACTION value.
*     *isdef
*        Returned holding the non-zero ==> the "value" value is a default
*        value and was not specified in the supplied string. zero ==>
*        "value" was specified explicitly in the supplied string.
*     *isgeo
*        If the bounds for the axis are defined by a centre value and
*        width and the width value is a WCS value, then the value
*        returned in "*isgeo" indicates if the width in "*value2" is a
*        geodesic distance. If not, it will be an axis increment. For
*        instance an RA increment will correspond to a different geodesic
*        distance at different Declinations.
*     *status
*        The global status.

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
   char *lstr;           /* Copy of str without leading or trailing spaces */
   char attr[ 20 ];      /* AST attribute name */
   const char *domain;   /* Pointer to axis Domain name string */
   int ncused;           /* No. of characters read from STR */
   size_t nc;            /* No. of characters in "lstr" excluding null */

/* Check inherited global status. */
   if( *status != SAI__OK ) return;

/* Create a copy of the string excluding any leading or trailing spaces. */
   lstr = ndf1Strip( NULL, str, 1, 0, &nc, NULL, status );
   if( *status == SAI__OK ) {

/* Initialise "isgeo" to indicate that "value" is not a geodesic distance. */
      *isgeo = 0;

/* If the input string is blank, then return the default value. */
      *isdef = ( nc == 0 );
      if( *isdef ) {
         *value = def;

         if( wcssec ) {
            *frame = 0;
         } else {
            *frame = 1;
         }

      } else {

/* If we are using the old pixel/axis syntax, see if the supplied value is
   an integer, in which case it is assumed to be a pixel index. */
         *frame = 0;
         if( !wcssec ) {

/* If the last character is a percent sign, the value is a FRACTION.
   Remove the %, convert the remaining percentage value to a fraction and
   flag the value as a FRACTION value. */
            if( lstr[ nc - 1 ] == '%' ) {
               *frame = 2;

               lstr[ nc - 1 ] = 0;
               *value = astChr2Double( lstr );
               lstr[ nc - 1 ] = '%';

               if( *value == AST__BAD ){
                  *status = NDF__BNDIN;
                  msgSetc( "BADBOUND", lstr );
                  errRep( " ", "Invalid FRACTION bound '^BADBOUND' specified; "
                          "bad syntax.", status );
               } else {
                  *value *= 0.01;
               }

/* Otherwise, first see if it is numerical. The returned value will be
   AST__BAD but no error will be reported if not. */
            } else {
               *value = astChr2Double( lstr );

/* If it is numerical, see if it is integer. */
               if( *value != AST__BAD && !strchr( lstr, '.') && !strchr( lstr, 'E' ) ){
                  *frame = 1;
               }
            }
         }

/* If the value is not a pixel index, and is not a FRACTION value, we
   interpret the string as a numerical geodesic distance or as a formated
   axis value. */
         if( *frame == 0 ) {

/* If the value ends with "as", "am", or "ad", interpret the value as an
   geodesic distance in arcseconds, arcminutes or arcdegrees. */
            if( lstr[ nc - 2 ] == 'a' ) {

               sprintf( attr, "Domain(%d)", axis );
               domain = astGetC( iwcs, attr );

               lstr[ nc - 2 ] = 0;
               *value = astChr2Double( lstr );
               lstr[ nc - 2 ] = 'a';

               if( *value != AST__BAD && domain && !strcmp( domain, "SKY" ) ) {
                  if( lstr[ nc - 1 ] == 's' ) {
                     *value *= AST__DD2R/3600.0;
                  } else if( lstr[ nc - 1 ] == 'm' ) {
                     *value *= AST__DD2R/60.0;
                  } else if( lstr[ nc - 1 ] == 'd' ) {
                     *value *= AST__DD2R;
                  }
                  *isgeo = 1;

               } else if( astOK ) {
                  sprintf( attr, "Label(%d)", axis );
                  msgSetc( "L", astGetC( iwcs, attr ) );
                  msgSetc( "BADBOUND", str );
                  *status = NDF__BNDIN;
                  errRep( " ", "Invalid ^L bound '^BADBOUND' specified; bad "
                          "syntax.", status );
               }

/* If it is not a geodesic distance, interpret the value using the
   astUnformat method. */
            } else {

/* Now read the value from the formatted text. */
               ncused = astUnformat( iwcs, axis, str, value );

/* Report an error if there was any spurious text in the string. */
               if( ncused < strlen( str ) && *status == SAI__OK ) {
                  sprintf( attr, "Label(%d)", axis );
                  msgSetc( "L", astGetC( iwcs, attr ) );
                  msgSetc( "BADBOUND", str );
                  *status = NDF__BNDIN;
                  errRep( " ", "Invalid ^L bound '^BADBOUND' specified; bad "
                          "syntax.", status );
               }
            }
         }
      }
   }

/* Free resources. */
   lstr = astFree( lstr );

/* Call error tracing function and exit. */
   if( *status != SAI__OK ) ndf1Trace( "ndf1Psndb", status );

}

