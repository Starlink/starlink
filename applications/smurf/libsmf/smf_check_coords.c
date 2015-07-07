/*
*+
*  Name:
*     smf_check_coords

*  Purpose:
*     Compare the AZEL and tracking boresight positions in JCMTSTATE

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     C function

*  Invocation:
*     void smf_check_coords( smfData *Data, int *status );

*  Arguments:
*     data = smfData * (Given and Returned)
*        The data containing the values to be compared.
*     status = int* (Given and Returned)
*        Pointer to global status.

*  Description:
*     This function uses AST to convert all the AZEL boresight positions
*     in the JCMTState info of the supplied smfData to the tracking system.
*     It then find the arc-distance from each converted AZEL position to
*     the corresponding tracking posiiton store in JCMTState, and
*     displays statistics of these separations.

*  Authors:
*     David S Berry (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     25-NOV-2013 (DSB):
*        Original version.
*     7-JUL-2015 (DSB):
*        Indicate that sky separations below 0.05 arc-seconds (SC2AST__SKYTOL)
*        are insignificant.
*     {enter_further_changes_here}

*  Copyright:
*     Copyright (C) 2013 Science & Technology Facilities Council.
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

/* Starlink includes */
#include "ast.h"
#include "sae_par.h"
#include "mers.h"

/* SMURF includes */
#include "smf.h"
#include "smf_typ.h"
#include "sc2da/sc2ast.h"

/* Seconds per day */
#define SPD 86400.0

/* Radians to arc-seconds factor */
#define R2AS AST__DR2D*3600

void smf_check_coords( smfData *data, int *status ) {

/* Local Variables: */
   AstFrameSet *fs;
   AstMapping *tmap1;
   AstMapping *tmap2;
   AstSkyFrame *azel = NULL;
   AstSkyFrame *tracking = NULL;
   JCMTState *state;
   double amean;
   double ang;
   double asigma;
   double cmean;
   double cvar;
   double d;
   double dut1;
   double epoch;
   double maxd;
   double mean;
   double mind;
   double p1[ 2 ];
   double p2[ 2 ];
   double p3[ 2 ] = { 0.0, AST__DPIBY2 };
   double rms;
   double s1;
   double s2;
   double s3;
   double s4;
   double s5;
   double s6;
   double sigma;
   double smean;
   double svar;
   int ns;
   size_t itime;
   smfHead *hdr;

/* Check inherited status */
   if( *status != SAI__OK ) return;

/* Start an AST object context. */
   astBegin;

/* Get a convenient pointer to the header. */
   hdr = data->hdr;
   if( !hdr ) {
      *status = SAI__ERROR;
      if( data->file && data->file->name ) {
         errRepf( " ", "No JCMT header found in '%s'.", status,
                 data->file->name );
      } else {
         errRep( " ", "No JCMT header found in input data file.", status );
      }
      goto L999;
   }

/* Read DUT1 from the header */
   if( astGetFitsF( hdr->fitshdr, "DUT1", &dut1 ) ) {
      dut1 *= SPD;
   } else {
      dut1 = 0.0;
   }

/* Create an (az,el)  SkyFrame with the correct telescope position and
   DUT1 value. */
   azel = astSkyFrame( "System=AzEl" );
   astSetD( azel, "ObsLon", -hdr->telpos[ 0 ] );
   astSetD( azel, "ObsLat", hdr->telpos[ 1 ] );
   astSetD( azel, "Dut1", dut1 );
   astSetD( azel, "SkyTol", SC2AST__SKYTOL );

/* Take a copy of this SkyFrame, and set the System of the copy to the
   AST equivalent of the TCS tracking system. */
   tracking = astCopy( azel );
   astSetC( tracking, "System",
            sc2ast_convert_system( hdr->allState->tcs_tr_sys, status ) );

/* Initialise statistics. */
   s1 = 0.0;
   s2 = 0.0;
   s3 = 0.0;
   s4 = 0.0;
   s5 = 0.0;
   s6 = 0.0;
   maxd = 0.0;
   mind = DBL_MAX;
   ns = 0;

/* Loop round all the time slices in the input file. */
   state = hdr->allState;
   for( itime = 0; itime < (data->dims)[ 2 ] && *status == SAI__OK;
        itime++,state++ ) {

/* Modify the Epoch attribute of both SkyFrames to match the current time
   slice. Remember to convert from TAI to TDB (as required by the Epoch #
   attribute). */
      epoch = state->tcs_tai + 32.184/SPD;
      astSet( azel, "Epoch=MJD %.*g", DBL_DIG, epoch );
      astSet( tracking, "Epoch=MJD %.*g", DBL_DIG, epoch );

/* Get a Mapping from (az,el) to the tracking system. */
      fs = astConvert( azel, tracking, "SKY" );
      tmap1 = astGetMapping( fs, AST__BASE, AST__CURRENT );
      tmap2 = astSimplify( tmap1 );

/* Use this mapping to transform the boresight (az,el) position to
   tracking. */
      astTran2( tmap1, 1, &(state->tcs_az_ac1), &(state->tcs_az_ac2), 1,
                p1, p1 + 1 );

/* Find the arc-distance from this transformed positon to the tracking
   system position recorded in JCMTState. */
      p2[ 0 ] = state->tcs_tr_ac1;
      p2[ 1 ] = state->tcs_tr_ac2;
      d = astDistance( tracking, p1, p2 );

/* Find the position angle of the offset from tcs_az_ac1/2 to tcs_tr_ac1/2. */
      ang = astAngle( tracking, p3, p1, p2 );

/* Update statistics. */
      if( d != AST__BAD ) {
         s1 += d;
         s2 += d*d;
         ns++;
         if( d > maxd ) maxd = d;
         if( d < mind ) mind = d;

         d = cos( ang );
         s3 += d;
         s4 += d*d;

         d = sin( ang );
         s5 += d;
         s6 += d*d;
      }

/* Free resources. */
      tmap1 = astAnnul( tmap1 );
      tmap2 = astAnnul( tmap2 );
      fs = astAnnul( fs );
   }

/* Calculate and report statistics */
   if( ns > 0 ) {
      mean = s1/ns;
      sigma = sqrt( s2/ns - mean*mean );
      rms = sqrt( s2/ns );

      cmean = s3/ns;
      cvar = s4/ns - cmean*cmean;

      smean = s5/ns;
      svar = s6/ns - smean*smean;

      amean = atan2( smean, cmean );
      if( amean < 0.0 ) amean += 2*AST__DPI;

      asigma = sqrt( svar*cmean*cmean + cvar*smean*smean );

      msgOutf( " ", "Comparing tcs_tr_ac1/2 and tcs_az_ac1/2 in %s:",
               status, data->file->name );
      msgOutf( " ", "   RMS separation: %g arc-sec", status, rms*R2AS );
      msgOutf( " ", "   Maximum separation: %g arc-sec", status, maxd*R2AS );
      msgOutf( " ", "   Minimum separation: %g arc-sec", status, mind*R2AS );
      msgOutf( " ", "   Mean separation: %g arc-sec", status, mean*R2AS );
      msgOutf( " ", "   Standard deviation of separations: %g arc-sec",
               status, sigma*R2AS );
      msgOutf( " ", "   Mean position angle of error: %g deg", status,
               amean*AST__DR2D );
      msgOutf( " ", "   Standard deviation of position angles: %g deg", status,
               asigma*AST__DR2D );
      msgBlank( status );

   } else {
      msgOutf( " ", "No usable boresight positions found in %s:", status,
               data->file->name );
      msgBlank( status );
   }

L999:

/* Delete all remaining AST Objects created in the current AST context. */
   astEnd;

}

