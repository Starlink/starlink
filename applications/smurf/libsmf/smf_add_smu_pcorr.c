/*
*+
*  Name:
*     smf_add_smu_pcorr

*  Purpose:
*     Add pointing corrections to the jiggle positions in a JCMTState

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     C function

*  Invocation:
*     void smf_add_smu_pcorr( JCMTState *state, int isazel, double dlon,
*                             double dlat, int *status )

*  Arguments:
*     state = JCMTState * (Given & Returned)
*        JCMTSTATE structure to modify with the supplied longitude and
*        latitude offsets. The offset is applied by adjusting the SMU
*        JCMTSTATE fields.
*    isazel = int (Given
*        If true the pointing correction is being supplied as AZEL
*        offsets, else will be applied as offsets to the TRACKING system.
*    dlon = double (Given)
*        Longitude offset in arcsec.
*    dlat = double (Given)
*        Latitude offset in arcsec.
*     status = int* (Given and Returned)
*        Pointer to global status.

*  Description:
*     This function reads a table of longitude and latitude offsets with
*     associated times from the supplied text file. It then adds these
*     offsets onto the jiggle positions in the supplied smfHead, using
*     linear interpolation in the table to get offsets at non-tabulated
*     times.

*  Authors:
*     David S Berry (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     17-MAY-2011 (DSB):
*        Original version.
*     {enter_further_changes_here}

*  Copyright:
*     Copyright (C) 2011 Science & Technology Facilities Council.
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
#include "sae_par.h"

/* SMURF includes */
#include "libsmf/smf.h"

void smf_add_smu_pcorr( JCMTState *state, int azel, double dlon,
                        double dlat, int *status) {

/* Local Variables: */
   double cosrot;
   double dlat_az;
   double dlat_tr;
   double dlon_az;
   double dlon_tr;
   double rot;
   double sinrot;

/* Check the inherited status. */
   if( *status != SAI__OK ) return;

/* Get the rotation, in rads, from tracking north to elevation, measured
   positive from tracking north to tracking east. */
   rot = state->tcs_az_ang - state->tcs_tr_ang;
   cosrot = cos( rot );
   sinrot = sin( rot );

/* Get the DLON/DLAT values in tracking and in azel. */
   if( azel ) {
     dlon_az = dlon;
     dlat_az = dlat;
     dlon_tr = -dlon_az*cosrot + dlat_az*sinrot;
     dlat_tr =  dlon_az*sinrot + dlat_az*cosrot;

   } else {
     dlon_tr = dlon;
     dlat_tr = dlat;
     dlon_az = -dlon_tr*cosrot + dlat_tr*sinrot;
     dlat_az =  dlon_tr*sinrot + dlat_tr*cosrot;
   }

/* Add the dlon and dlat values onto the SMU jiggle positions (note, these
   are in units of arc-seconds, not radians). */
   if(  state->smu_az_jig_x != VAL__BADD ) {
     state->smu_az_jig_x -= dlon_az;
   }

   if(  state->smu_az_jig_y != VAL__BADD ) {
     state->smu_az_jig_y -= dlat_az;
   }

   if(  state->smu_tr_jig_x != VAL__BADD ) {
     state->smu_tr_jig_x -= dlon_tr;
   }

   if(  state->smu_tr_jig_y != VAL__BADD ) {
     state->smu_tr_jig_y -= dlat_tr;
   }
}
