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
*     smf_flat_standardpow( const smfArray * heatframes, double refohms,
*                           const double heatref[], const double resistance[],
*                           double powref[], double bolref[], int * status);

*  Arguments:
*     heatframes = const smfArray* (Given)
*        Collection of heat frames.
*     refohms = double (Given)
*        Representative heater resistance.
*     heatref = const double[] (Given)
*        For each frame in "heatframes", this is the heater setting that
*        was used.
*     resistance = const double [] (Given)
*        Resistance for each pixel heater.
*     powref = double [] (Given)
*        Resistance input powers. Must have space for the same number of
*        elements as frames stored in "heatframes".
*     bolref = double [] (Given)
*        Response of each bolometer to powref. Dimensioned as number of 
*        frames in "heatframes" times the number of bolometers.
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
*     {enter_further_changes_here}

*  Copyright:
*     Copyright (C) 2007-2008 Science and Technology Facilities Council.
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
*     Software Foundation, Inc., 59 Temple Place, Suite 330, Boston,
*     MA 02111-1307, USA

*  Bugs:
*     {note_any_bugs_here}
*-
*/

#include "smf_typ.h"
#include "smf.h"
#include "smurf_par.h"

#include "prm_par.h"
#include "sae_par.h"

void
smf_flat_standardpow( const smfArray * heatframes, double refohms,
                      const double heatref[], const double resistance[],
                      double powref[], double bolref[], int * status) {

  double current;         /* current through heaters */
  double **heatframe = NULL; /* Pointers to nheat heatframes */
  size_t i;               /* loop counter */
  size_t j;               /* loop counter */
  size_t k;               /* loop counter */
  size_t nheat;           /* Number of input frames */
  size_t numbol;          /* Number of bolometers */
  double *powbol = NULL;  /* pointer to individual bolometer powers */
  double s;               /* interpolation slope factor */

  if (*status != SAI__OK) return;

  nheat = heatframes->ndat;
  numbol = (heatframes->sdata)[0]->dims[0] *
    (heatframes->sdata)[0]->dims[1];

  /* Get some memory - 
     bolometer power per input frame */
  powbol = smf_malloc( nheat, sizeof(*powbol), 1, status );

  /* pointers to the data array associated with each input frame */
  heatframe = smf_malloc( nheat, sizeof(heatframe), 0, status );

  /* Choose the reference heater powers to be the actual heater settings acting on
     the adopted reference resistance.  */
   
  for ( j=0; j<nheat; j++ ) {
    current = heatref[j] * SC2FLAT__DTOI;
    powref[j] = current * current * refohms;
  }

  /* Store pointers to data array for each frame */
  for ( j=0; j<nheat; j++) {
    smfData * fr = (heatframes->sdata)[j];
    heatframe[j] = (fr->pntr)[0];
  }


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
              powbol[j] = current * current * resistance[i];
            }
        }

      /* Trap bolometers which are unresponsive */

      if ( fabs ( heatframe[0][i] - heatframe[nheat-1][i] ) < 1.0 )
        {
          powbol[0] = VAL__BADD;
        }
      
      for ( j=0; j<nheat; j++ )
        {
          if ( powbol[0] == VAL__BADD )
            {
              bolref[j*numbol+i] = VAL__BADD;
            }
          else
            {
              if ( powref[j] < powbol[0] )
                {

                  /* Standard point is below actual measured range for this bolometer, 
                     extrapolate */
   
                  s = ( heatframe[1][i] - heatframe[0][i] ) /
                    ( powbol[1] - powbol[0] );
                  bolref[j*numbol+i] = heatframe[0][i] + 
                    s * ( powref[j] - powbol[0] );
                }
              else if ( powref[j] > powbol[nheat-1] )
                {

                  /* Standard point is above actual measured range for this bolometer, 
                     extrapolate */
   
                  s = ( heatframe[nheat-1][i] -
                        heatframe[nheat-2][i] ) /
                    ( powbol[nheat-1] - powbol[nheat-2] );
                  bolref[j*numbol+i] = heatframe[nheat-2][i] + 
                    s * ( powref[j] - powbol[nheat-2] );
                }
              else
                {

                  /* Standard point is within actual measured range for this bolometer, 
                     search for the points to interpolate */
   
                  for ( k=1; k<nheat; k++ )
                    {
                      if ( powref[j] <= powbol[k] )
                        {
                          s = ( heatframe[k][i] -
                                heatframe[k-1][i] ) /
                            ( powbol[k] - powbol[k-1] );
                          bolref[j*numbol+i] = heatframe[k-1][i] + 
                            s * ( powref[j] - powbol[k-1] );
		     
                          break;
                        }
                    }
   
                }
            }

        }
    }
   
  if (powbol) powbol = smf_free ( powbol, status );
  if (heatframe) heatframe = smf_free( heatframe, status );

}
