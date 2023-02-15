/*
*+
*  Name:
*     smf_flat_standardpow

*  Purpose:
*     Calculate power relations for all bolometers

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     Subroutine

*  Invocation:
*     smf_flat_standardpow( const smfData * bolvald, double refohms,
*                           const double resistance[],
*                           smfData** powref, smfData ** bolref, int * status);

*  Arguments:
*     bolvald = const smfData * (Given)
*        3d smfData of all the flatfield data. The heater settings are stored
*        in a smfDA structure.
*     refohms = double (Given)
*        Representative heater resistance.
*     resistance = const double [] (Given)
*        Resistance for each pixel heater. One value for each bolometer.
*     powref = smfData**  (Returned)
*        Resistance input powers. Must have space for the same number of
*        elements as frames stored in "heatframes". Struct allocated by
*        this routine. Will be NULL on error.
*     bolref = smfData** (Returned)
*        Response of each bolometer to powref. Dimensioned as number of
*        bolometers  time the number of frames in "heatframes". Struct
*        allocated by this routine. Will be NULL on error.
*     status = int* (Given and Returned)
*        Pointer to global status.

*  Description:
*      Convert the list of heater settings (heatref) to the equivalent power
*      generated in the reference resistor (powref). For each bolometer
*      calculate the actual power delivered at each of the (heatref) heater
*      settings and interpolate to each of the powref values.

*  Notes:

*  Authors:
*     BDK: Dennis Kelly (UKATC)
*     TIMJ: Tim Jenness (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     2007-08-22 (BDK):
*        Original version
*     2008-06-29 (BDK):
*        insert fix to interpolation from 13Feb2008.
*     2008-08-27 (TIMJ):
*        Rewrite for SMURF from sc2flat.c
*     2008-09-03 (TIMJ):
*        Throw out any bolometers that have any bad values in their darks.
*     2010-01-28 (TIMJ):
*        Switch to a smfData API
*     2010-02-03 (TIMJ):
*        Propagate variance
*     2010-03-03 (TIMJ):
*        Use smf_flat_malloc
*     2010-03-05 (TIMJ):
*        Change API to use merged smfData (see smf_flat_mergedata) rather than
*        a smfArray. This lets it be used by flatfield ramps or old fashioned
*        flatfields.
*     {enter_further_changes_here}

*  Copyright:
*     Copyright (C) 2007-2008, 2010 Science and Technology Facilities Council.
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
*     You should have received a copy of the GNU General Public
*     License along with this program; if not, write to the Free
*     Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,
*     MA 02110-1301, USA

*  Bugs:
*     {note_any_bugs_here}
*-
*/

#include "smf_typ.h"
#include "smf.h"
#include "smurf_par.h"

#include "mers.h"
#include "prm_par.h"
#include "sae_par.h"

void
smf_flat_standardpow( const smfData * bolvald, double refohms,
                      const double resistance[],
                      smfData ** powrefd, smfData ** bolrefd, int * status) {

  double current;         /* current through heaters */
  double *heatframe = NULL; /* Pointer to flatfield heater data  */
  double *heatframevar = NULL; /* Pointer to flatfield heatframe variance */
  double * heatref = NULL; /* local copy of heater settings */
  dim_t i;               /* loop counter */
  dim_t j;               /* loop counter */
  dim_t k;               /* loop counter */
  dim_t nheat;           /* Number of input frames */
  dim_t numbol;          /* Number of bolometers */
  double *powbol = NULL;  /* pointer to individual bolometer powers */
  double s;               /* interpolation slope factor */

  double * powref = NULL; /* Data array inside powrefd */
  double * bolref = NULL; /* Data array inside bolrefd */
  double * bolrefvar = NULL; /* Variance inside bolrefd */

  *powrefd = NULL;
  *bolrefd = NULL;

  if (*status != SAI__OK) return;

  if ( !smf_dtype_check_fatal( bolvald, NULL, SMF__DOUBLE, status ) ) return;

  if (!bolvald->da) {
    *status = SAI__ERROR;
    errRep( "", "Unable to proceed with flatfield calculation. Heater settings not "
            "present in smfData", status);
    return;
  }

  nheat = (bolvald->dims)[2];
  numbol = (bolvald->dims)[0] * (bolvald->dims)[1];
  heatref = bolvald->da->heatval;

  /* Create a smfData for powref and bolref */
  smf_flat_malloc( nheat, bolvald, powrefd, bolrefd, status );

  if (*status == SAI__OK) {
    powref = (*powrefd)->pntr[0];
    bolref = (*bolrefd)->pntr[0];
    bolrefvar = (*bolrefd)->pntr[1];
  }

  /* Get some memory -
     bolometer power per heater setting */
  powbol = astCalloc( nheat, sizeof(*powbol) );

  /* pointers to the flatfield data and variance */
  heatframe = (bolvald->pntr)[0];
  heatframevar = (bolvald->pntr)[1];

  /* Choose the reference heater powers to be the actual heater settings acting on
     the adopted reference resistance.  */

  for ( j=0; j<nheat; j++ ) {
    current = heatref[j] * SC2FLAT__DTOI;
    powref[j] = SIMULT * current * current * refohms;
  }

  if (*status != SAI__OK) goto CLEANUP;

  /* For each bolometer interpolate to find the measurement it would report at the
     standard power input values */

  for ( i=0; i<numbol; i++ )
    {

      /* Calculate the actual power seen by each bolometer at each heater setting */

      for ( j=0; j<nheat; j++ )
        {
          current = heatref[j] * SC2FLAT__DTOI;
          if ( resistance[i] == VAL__BADD )
            {
              powbol[j] = VAL__BADD;
            }
          else
            {
              powbol[j] = SIMULT * current * current * resistance[i];
            }
        }

      /* Trap bolometers that have any bad values in their measurements */
      for ( j=0; j<nheat; j++) {
        if ( heatframe[i + j*numbol] == VAL__BADD) {
          powbol[0] = VAL__BADD;
          break;
        }
      }

      /* Trap bolometers which are unresponsive */

      if ( powbol[0] != VAL__BADD &&
           (heatframe[i] == VAL__BADD || heatframe[i + (nheat-1)*numbol] == VAL__BADD ||
            (fabs ( heatframe[i] - heatframe[i + (nheat-1)*numbol] ) < 1.0) ) )
        {
          powbol[0] = VAL__BADD;
        }

      for ( j=0; j<nheat; j++ )
        {
          if ( powbol[0] == VAL__BADD )
            {
              bolref[j*numbol+i] = VAL__BADD;
              bolrefvar[j*numbol+i] = VAL__BADD;
            }
          else
            {

              if ( powref[j] < powbol[0] )
                {
                  double var = VAL__BADD;

                  /* Standard point is below actual measured range for this bolometer,
                     extrapolate */

                  s = ( heatframe[i + 1*numbol] - heatframe[i + 0*numbol] ) /
                    ( powbol[1] - powbol[0] );

                  /* calculate error in gradient using standard rules */
                  if (heatframevar && heatframevar[i+ 1*numbol] != VAL__BADD &&
                      heatframevar[i + 0*numbol] != VAL__BADD) {
                    var = ( heatframevar[i + 1*numbol] + heatframevar[i + 0*numbol] )
                      / pow( powbol[1] - powbol[0], 2 );
                  }

                  bolref[j*numbol+i] = heatframe[i+ 0*numbol] +
                    s * ( powref[j] - powbol[0] );

                  if (var != VAL__BADD) {
                    bolrefvar[j*numbol+i] = heatframevar[i + 0*numbol] +
                      ( var * pow( powref[j] - powbol[0], 2 ));

                  } else {
                    bolrefvar[j*numbol+i] = VAL__BADD;
                  }


                }
              else if ( powref[j] > powbol[nheat-1] )
                {
                  double var = VAL__BADD;

                  /* Standard point is above actual measured range for this bolometer,
                     extrapolate */

                  s = ( heatframe[i + (nheat-1)*numbol] - heatframe[i + (nheat-2)*numbol] ) /
                    ( powbol[nheat-1] - powbol[nheat-2] );

                  /* calculate error in gradient using standard rules */
                  if (heatframevar && heatframevar[i + (nheat-1)*numbol] != VAL__BADD &&
                      heatframevar[i + (nheat-2)*numbol] != VAL__BADD) {
                    var = ( heatframevar[i + (nheat-1)] + heatframevar[i + (nheat-2)*numbol] )
                      / pow( powbol[nheat-1] - powbol[nheat-2], 2 );
                  }

                  bolref[j*numbol+i] = heatframe[i + (nheat-2)*numbol] +
                    s * ( powref[j] - powbol[nheat-2] );

                  if (var != VAL__BADD) {
                    bolrefvar[j*numbol+i] = heatframevar[i +(nheat-2)*numbol] +
                      ( var * pow( powref[j] - powbol[nheat-2], 2 ));

                  } else {
                    bolrefvar[j*numbol+i] = VAL__BADD;
                  }

                }
              else
                {

                  /* Standard point is within actual measured range for this bolometer,
                     search for the points to interpolate */

                  for ( k=1; k<nheat; k++ )
                    {
                      if ( powref[j] <= powbol[k] )
                        {
                          double var = VAL__BADD;
                          s = ( heatframe[i + k*numbol] -
                                heatframe[i + (k-1)*numbol] ) /
                            ( powbol[k] - powbol[k-1] );

                          /* calculate error in gradient using standard rules */
                          if (heatframevar && heatframevar[i + k*numbol] != VAL__BADD &&
                              heatframevar[i +(k-1)*numbol] != VAL__BADD) {
                            var = ( heatframevar[i + k*numbol] + heatframevar[i + (k-1)*numbol] )
                              / pow( powbol[k] - powbol[k-1], 2 );
                          }
                          bolref[j*numbol+i] = heatframe[i + (k-1)*numbol] +
                            s * ( powref[j] - powbol[k-1] );

                          if (var != VAL__BADD) {
                            bolrefvar[j*numbol+i] = heatframevar[i +(k-1)*numbol] +
                              ( var * pow( powref[j] - powbol[k-1], 2 ));
                          } else {
                            bolrefvar[j*numbol+i] = VAL__BADD;
                          }

                          break;
                        }
                    }

                }
            }

        }
    }

 CLEANUP:
  if (powbol) powbol = astFree( powbol );

  if (*status != SAI__OK) {
    if (*bolrefd) smf_close_file( NULL, bolrefd, status );
    if (*powrefd) smf_close_file( NULL, powrefd, status );
  }

}
