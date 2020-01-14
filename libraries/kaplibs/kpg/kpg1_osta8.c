#include <string.h>
#include "f77.h"
#include "kaplibs.h"
#include "mers.h"
#include "sae_par.h"
#include "star/thr.h"

F77_SUBROUTINE(kpg1_osta8)( CHARACTER(TYPE), LOGICAL(BAD), INTEGER8(EL),
                            const void *data, INTEGER(NCLIP), REAL_ARRAY(CLIP),
                            INTEGER8_ARRAY(ISTAT), DOUBLE_ARRAY(DSTAT),
                            INTEGER8_ARRAY(ISTATC), DOUBLE_ARRAY(DSTATC),
                            INTEGER(STATUS) TRAIL(TYPE) ) {
/*
*+
*  Name:
*     KPG1_OSTA8

*  Purpose:
*     Computes simple statistics for an array.

*  Language:
*     C, designed to be called from Fortran.

*  Invocation:
*     CALL KPG_OSTA8( TYPE, BAD, EL, DATA, NCLIP, CLIP, ISTAT, DSTAT,
*                     ISTATC, DSTATC, STATUS )

*  Description:
*     This routine computes simple statistics for an array, namely: the
*     number of valid pixels; the minimum and maximum pixel values (and
*     their positions); the pixel sum; the mean; and the population
*     standard deviation, skewness, and excess kurtosis.  Iterative
*     K-sigma clipping may also be optionally applied.
*
*     It uses a one-pass recursive algorithm for efficiency using the
*     formulae of Terriberry (2007).

*  Arguments:
*     TYPE = CHARACTER*(*) (Given)
*        The HDS type of the DATA array.
*     BAD = LOGICAL (Given)
*        Whether checks for bad pixels should be performed on the array
*        being analysed.
*     EL = INTEGER*8 (Given)
*        Number of pixels in the array.
*     DATA( EL ) = ? (Given)
*        Array to be analysed.
*     NCLIP = INTEGER (Given)
*        Number of K-sigma clipping iterations to apply (may be zero).
*     CLIP( NCLIP ) = REAL (Given)
*        Array of clipping limits for successive iterations, expressed
*        as standard deviations.
*     ISTAT( 3 ) = INTEGER*8 (Returned)
*        The integer statistics before clipping.  The meanings of the
*        elements in order are as follows.
*        -  Number of valid pixels
*        -  Index where the pixel with the lowest value was (first)
*        found
*        -  Index where the pixel with the highest value was (first)
*        found
*     DSTAT( 7 ) = DOUBLE PRECISION (Returned)
*        The floating-point statistics before clipping derived from the
*        valid pixel values in DATA.  The meanings of the elements in
*        order are as follows.
*        -  Minimum value
*        -  Maximum value
*        -  Sum
*        -  Mean
*        -  Population standard deviation
*        -  Population skewness
*        -  Population excess kurtosis.  This is zero for a Gaussian.
*     ISTATC( 3 ) = INTEGER*8 (Returned)
*        The integer statistics after clipping derived from the valid
*        pixel values in DATA.  The attributions of the elements are
*        the same as for argument ISTAT.  If NCLIP is zero, the
*        array will contain the same values as ISTAT.
*     DSTATC( 7 ) = DOUBLE PRECISION (Returned)
*        The floating-point statistics after clipping derived from the
*        valid pixel values in DATA.  The attributions of the elements
*        are the same as for argument DSTAT.  If NCLIP is zero, the
*        array will contain the same values as DSTAT.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     -  If no clipping is performed (i.e. if NCLIP = 0) then the values
*     of arguments which return results after clipping will be the same
*     as for those returning results before clipping.
*     -  If ISTAT(1) or ISTATC(1) is zero, then the values of all the
*     derived statistics will be undefined and will be set to the "bad"
*     value appropriate to their data type (except for the pixel sum,
*     which will be zero).

*  Copyright:
*     Copyright (C) 2020 East Asian Observatory
*     All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either Version 2 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful, but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
*     02110-1301, USA.

*  Authors:
*     DSB: David S. Berry (EAO)
*     {enter_new_authors_here}

*  History:
*     13-JAN-2020 (DSB):
*        Original version.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}
*-
*/
   GENPTR_CHARACTER(TYPE)
   GENPTR_LOGICAL(BAD)
   GENPTR_INTEGER8(EL)
   GENPTR_INTEGER(NCLIP)
   GENPTR_REAL_ARRAY(CLIP)
   GENPTR_INTEGER8_ARRAY(ISTAT)
   GENPTR_DOUBLE_ARRAY(DSTAT)
   GENPTR_INTEGER8_ARRAY(ISTATC)
   GENPTR_DOUBLE_ARRAY(DSTATC)
   GENPTR_INTEGER(STATUS)

/* Find the number of cores/processors available and create a pool of
   threads of the same size. Assume we're being called from a KAPPA
   application (sadly, we probably need an extra argument to indicate
   what package is being used). */
   ThrWorkForce *wf = thrGetWorkforce( thrGetNThread( "KAPPA_THREADS", STATUS ),
                                       STATUS );

/* Call the appropriate routine to compute the statistics. */
   if( !strncmp( TYPE, "_BYTE", 5 ) ) {
      kpgOsta8B( wf, F77_ISTRUE( *BAD ), *EL, data, *NCLIP, CLIP,
                 ISTAT, DSTAT, ISTATC, DSTATC, STATUS );

   } else if( !strncmp( TYPE, "_DOUBLE", 7 ) ) {
      kpgOsta8D( wf, F77_ISTRUE( *BAD ), *EL, data, *NCLIP, CLIP,
                 ISTAT, DSTAT, ISTATC, DSTATC, STATUS );

   } else if( !strncmp( TYPE, "_INTEGER", 8 ) ) {
      kpgOsta8I( wf, F77_ISTRUE( *BAD ), *EL, data, *NCLIP, CLIP,
                 ISTAT, DSTAT, ISTATC, DSTATC, STATUS );

   } else if( !strncmp( TYPE, "_INT64", 6 ) ) {
      kpgOsta8K( wf, F77_ISTRUE( *BAD ), *EL, data, *NCLIP, CLIP,
                 ISTAT, DSTAT, ISTATC, DSTATC, STATUS );

   } else if( !strncmp( TYPE, "_REAL", 5 ) ) {
      kpgOsta8F( wf, F77_ISTRUE( *BAD ), *EL, data, *NCLIP, CLIP,
                 ISTAT, DSTAT, ISTATC, DSTATC, STATUS );

   } else if( !strncmp( TYPE, "_WORD", 5 ) ) {
      kpgOsta8W( wf, F77_ISTRUE( *BAD ), *EL, data, *NCLIP, CLIP,
                 ISTAT, DSTAT, ISTATC, DSTATC, STATUS );

   } else if( *STATUS == SAI__OK ){
      *STATUS = SAI__ERROR;
      errRepf( " ", "KPG1_OSTA8: Unknown HDS data type '%.*s'.", STATUS,
               TYPE_length, TYPE );
   }

}
