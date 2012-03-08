/*
*+
*  Name:
*     palDafin

*  Purpose:
*     Sexagesimal character string to angle

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     Library routine

*  Invocation:
*     void palDafin ( const char *string, int *ipos, double *a, int *j );

*  Arguments:
*     string = const char * (Given)
*        String containing deg, arcmin, arcsec fields
*     ipos = int * (Given & Returned)
*        Position to start decoding "string". First character
*        is position 1 for compatibility with SLA. After
*        calling this routine "iptr" will be positioned after
*        the sexagesimal string.
*     a = double * (Returned)
*        Angle in radians.
*     j = int * (Returned)
*        status:  0 = OK
*                +1 = default, A unchanged
*                -1 = bad degrees      )
*                -2 = bad arcminutes   )  (note 3)
*                -3 = bad arcseconds   )

*  Description:
*     Extracts an angle from a sexagesimal string with degrees, arcmin,
*     arcsec fields using space or comma delimiters.

*  Authors:
*     TIMJ: Tim Jenness (JAC, Hawaii)
*     {enter_new_authors_here}

*  Example:
*     argument    before                           after
*
*     STRING      '-57 17 44.806  12 34 56.7'      unchanged
*     IPTR        1                                16 (points to 12...)
*     A           ?                                -1.00000D0
*     J           ?                                0

*  Notes:
*     - The first three "fields" in STRING are degrees, arcminutes,
*       arcseconds, separated by spaces or commas.  The degrees field
*       may be signed, but not the others.  The decoding is carried
*       out by the palDfltin routine and is free-format.
*     - Successive fields may be absent, defaulting to zero.  For
*       zero status, the only combinations allowed are degrees alone,
*       degrees and arcminutes, and all three fields present.  If all
*       three fields are omitted, a status of +1 is returned and A is
*       unchanged.  In all other cases A is changed.
*     - Range checking:
*
*           The degrees field is not range checked.  However, it is
*           expected to be integral unless the other two fields are absent.
*
*           The arcminutes field is expected to be 0-59, and integral if
*           the arcseconds field is present.  If the arcseconds field
*           is absent, the arcminutes is expected to be 0-59.9999...
*
*           The arcseconds field is expected to be 0-59.9999...
*
*     - Decoding continues even when a check has failed.  Under these
*       circumstances the field takes the supplied value, defaulting
*       to zero, and the result A is computed and returned.
*     - Further fields after the three expected ones are not treated
*       as an error.  The pointer IPOS is left in the correct state
*       for further decoding with the present routine or with palDfltin
*       etc. See the example, above.
*     - If STRING contains hours, minutes, seconds instead of degrees
*       etc, or if the required units are turns (or days) instead of
*       radians, the result A should be multiplied as follows:
*
*           for        to obtain    multiply
*           STRING     A in         A by
*
*           d ' "      radians      1       =  1.0
*           d ' "      turns        1/2pi   =  0.1591549430918953358
*           h m s      radians      15      =  15.0
*           h m s      days         15/2pi  =  2.3873241463784300365

*  History:
*     2012-03-08 (TIMJ):
*        Initial version from SLA/F using Fortran documentation
*     {enter_further_changes_here}

*  Copyright:
*     Copyright (C) 2012 Science and Technology Facilities Council.
*     All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either version 3 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful, but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,
*     MA 02110-1301, USA.

*  Bugs:
*     {note_any_bugs_here}
*-
*/

#include "pal.h"
#include "palmac.h"
#include "sofam.h"

#include <math.h>

void palDafin ( const char *string, int *ipos, double *a, int *j ) {

  int jd = 0;    /* Status for degree parsing */
  int jm = 0;    /* Status for arcmin parsing */
  int js = 0;    /* Status for arcsec parsing */
  int jf = 0;    /* Internal copy of status */
  double deg = 0.0;
  double arcmin = 0.0;
  double arcsec = 0.0;

  /* Decode degrees, arcminutes, arcseconds */
  palDfltin( string, ipos, &deg, &jd );
  if (jd > 1) {
    jf = -1;
  } else {

    palDfltin( string, ipos, &arcmin, &jm );
    if ( jm < 0 || jm > 1 ) {
      jf = -2;
    } else {

      palDfltin( string, ipos, &arcsec, &js );
      if (js < 0 || js > 1) {
        jf = -3;

      } else if (jd > 0) { /* See if combination of fields is credible */
        /* No degrees: arcmin, arcsec ought also to be absent */
        if (jm == 0) {
          /* Suspect arcmin */
          jf = -2;
        } else if (js == 0) {
          /* Suspect arcsec */
          jf = -3;
        } else {
          /* All three fields absent */
          jf = 1;
        }

      } else if (jm != 0 && js == 0) { /* Deg present: if arcsec present should have arcmin */
        jf = -3;

      /* Tests for range and integrality */
      } else if (jm == 0 && dint(deg) != deg) { /* Degrees */
        jf = -1;

      } else if (js == 0 && dint(arcmin) != arcmin) { /* Arcmin */
        jf = -2;

      } else if (arcsec >= 60.0) { /* Arcsec */
        jf = -3;
      }
    }
  }

  /* Unless all three fields absent, compute angle value */
  if (jf <= 0) {
    *a = PAL__DAS2R * ( 60.0 * ( 60.0 * fabs(deg) + arcmin) + arcsec );
    if ( jd < 0 ) *a *= -1.;
  }

  *j = jf;

}
