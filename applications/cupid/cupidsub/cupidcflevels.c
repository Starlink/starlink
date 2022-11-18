#include "sae_par.h"
#include "mers.h"
#include "cupid.h"
#include "cupid_err.h"
#include "ast.h"
#include "prm_par.h"
#include <stdio.h>
#include <float.h>

double *cupidCFLevels( AstKeyMap *config, double maxd, double mind,
                       double rms, int *nlevels, int *status ){
/*
*+
*  Name:
*     cupidCFLevels

*  Purpose:
*     Get the contour levels for use by the ClumpFind algorithm.

*  Language:
*     Starlink C

*  Synopsis:
*     double *cupidCFLevels( AstKeyMap *config, double maxd, double mind,
*                            double rms, int *nlevels, int *status )

*  Description:
*     This function obtains a series of contour levels at which the
*     ClumpFind algorithm will search for peaks.

*  Parameters:
*     config
*        An AST KeyMap holding tuning parameters for the algorithm.
*     maxd
*        The maximum data value in the data array.
*     mind
*        The minimum data value in the data array.
*     rms
*        The RMS noise level in the data.
*     nlevels
*        Pointer to an int to receive the number of contour levels.
*     status
*        Pointer to the inherited status value.

*  Returned Value:
*     A pointer to a dynamically allocated array containing "*nlevels"
*     floating point contour values. It should be freed using astFree
*     when no longer needed.

*  Copyright:
*     Copyright (C) 2005 Particle Physics & Astronomy Research Council.
*     All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either version 2 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful, but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 51 Franklin Street,Fifth Floor, Boston, MA
*     02110-1301, USA

*  Authors:
*     DSB: David S. Berry
*     {enter_new_authors_here}

*  History:
*     7-DEC-2005 (DSB):
*        Original version.
*     18-SEP-2007 (DSB):
*        Correct calculation of number of contour levels based on TLOW
*        and TDELTA values.Original version. Previously, the number of
*        contours was too small by 1.
*     3-MAR-2011 (DSB):
*        More error checking.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
*/

/* Local Variables: */

   char name[ 20 ];        /* Name of "LEVELn" value */
   double *ret;            /* Pointer to returned array of contour levels */
   double cdelta;          /* Increment between contour levels */
   double clevel;          /* Contour level */
   double clow;            /* Lowest contour level */
   int i;                  /* Contour index */
   int ihi;                /* Index of last unsorted contour */
   int more;               /* Levels not yet sorted? */

/* Initialise */
   ret = NULL;
   *nlevels = 0;

/* Abort if an error has already occurred. */
   if( *status != SAI__OK ) return ret;

/* If the supplied KeyMap contains a set of "LEVELn" values, use them as
   the contour levels. The first value is called "LEVEL1". */
   i = 0;
   while( 1 ) {
      i++;
      sprintf( name, "LEVEL%d", i );
      clevel = cupidConfigRMS( config, name, rms, VAL__BADD, status );
      if( clevel == AST__BAD ) {
         i--;
         break;
      } else {
         ret = astGrow( ret, i, sizeof( double ) );
         if( ret ) {
            ret[ i - 1 ] = clevel;
         } else {
            break;
         }
      }
   }

/* If a set of LEVELn values was found, sort them into decreasing order
   and return the number. */
   if( ret ) {
      *nlevels = i;
      ihi = *nlevels - 1;
      more = 1;
      while( more ) {
         more = 0;
         for( i = 0; i < ihi; i++ ) {
            if( ret[ i ] < ret[ i + 1 ] ){
               clevel = ret[ i ];
               ret[ i ] = ret[ i + 1 ];
               ret[ i + 1 ] = clevel;
               more = 1;
            }
         }
         ihi--;
      }

/* Otherwise, use contour levels at increments of DELTAT, starting at
   TLOW. */
   } else {

/* Get the lowest contour level using twice the RMS as the default. */
      clow = cupidConfigRMS( config, "TLOW", rms, 2.0*rms, status );

/* Report an error if the lowest contour level is below the minimum value
   in the data array. */
      if( clow < mind && *status == SAI__OK ) {
         *status = SAI__ERROR;
         msgSetd( "TLOW", clow );
         msgSetd( "MIND", mind );
         errRep( "CUPIDCFLEVELS_ERR1", "The supplied lowest contour level "
                 "(Tlow=^TLOW) is below the minimum value in the data "
                 "array (^MIND).", status );

/* Report an error if the lowest contour level is above the maximum value
   in the data array. */
      } else if( clow >= maxd && *status == SAI__OK ) {
         *status = SAI__ERROR;
         msgSetd( "TLOW", clow );
         msgSetd( "MAXD", maxd );
         errRep( "CUPIDCFLEVELS_ERR2", "The supplied lowest contour level "
                 "(Tlow=^TLOW) is above the maximum value in the data "
                 "array (^MAXD).", status );

/* Otherwise, use 2*RMS as the default. */
      } else {
         cdelta = 2.0*rms;

/* Get the contour interval using the above default. */
         cdelta = cupidConfigRMS( config, "DELTAT", rms, cdelta, status );

/* Report an error if it is negative or zero. */
         if( cdelta <= 0.0 && *status == SAI__OK ) {
            *status = SAI__ERROR;
            msgSetd( "CD", cdelta );

            errRep( "CUPIDCFLEVELS_ERR3", "The supplied contour interval "
                    "(DeltaT=^CD) is zero or negative.", status );

/* Otherwise, find the number of levels needed for this deltat. */
         } else if( *status == SAI__OK ) {
            *nlevels = (int) ( ( maxd - clow )/cdelta ) + 1;

/* Check the number of levels is reasonable. */
            if( ( *nlevels < 2 || *nlevels > 10000000 ) ) {
               *status = CUPID__CFCNT;
               msgSetd( "T", clow );
               msgSetd( "D", cdelta );
               msgSetd( "R", rms );
               msgSeti( "NL", *nlevels );
               errRep( "CUPIDCFLEVELS_ERR4", "The supplied values for "
                       "parameters TLOW (^T), DELTAT (^D) and RMS (^R) "
                       "would result in an unusable number (^NL) of "
                       "contours.", status );

/* If so, allocate the array and fill it with the appropriate contour levels. */
            } else {
               ret = astMalloc( sizeof( double )*(*nlevels) );
               if( ret ) {
                  clevel = clow;
                  for( i = *nlevels - 1; i >= 0; i-- ) {
                     ret[ i ]= clevel;
                     clevel += cdelta;
                  }
               }
            }
         }
      }
   }

/* Return no levels if an error occurred. */
   if( *status != SAI__OK ) ret = astFree( ret );
   if( !ret ) *nlevels = 0;

/* Return the array of contour levels. */
   return ret;

}
