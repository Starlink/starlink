/*
*+
*  Name:
*     smf_scale2freq

*  Purpose:
*     Convert high and low pass filter scales to frequencies.

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     C function

*  Invocation:
*     void smf_scale2freq( double f_edgesmall, double f_edgelarge,
*                          const smfHead *hdr, double *f_edgelow,
*                          double *f_edgehigh, int *status )

*  Arguments:
*     f_edgesmall = double (Given)
*        The low pass spatial scale in arcseconds to be converted to a
*        frequency.
*     f_edgelarge = double (Given)
*        The high pass spatial scale in arcseconds to be converted to a
*        frequency.
*     data = smfData * (Given and Returned)
*        The data that will be repaired (in-place). Locations of steps
*        will have bit SMF__Q_JUMP set.
*     f_edgelow = double * (Given and Returned)
*        The low pass frequency, in Hz. The supplied value is left
*        unchanged if "f_edgesmall" is zero or the telescope is stationary.
*     f_edgehigh = double * (Given and Returned)
*        The high pass frequency, in Hz. The supplied value is left
*        unchanged if "f_edgelarge" is zero or the telescope is stationary.
*     status = int* (Given and Returned)
*        Pointer to global status.

*  Description:
*     Convert low and high pass filter specificiations from spatial
*     scales to frequencies.
*
*     Status is set to SMF__TELSTAT if the telescope is stationary.

*  Authors:
*     EC: Edward Chapin (UBC)
*     DSB: David S Berry (JAC, Hawaii)
*     TIMJ: Tim Jenness (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     4-OCT-2010 (DSB):
*        Original version (copied from smf_filter_fromkeymap.c by EC).
*     2011-04-20 (TIMJ):
*        More explicit error message
*        Set status to SMF__TELSTAT if the telescope is not moving.
*     18-FEB-2013 (DSB):
*        Added edge width arguments.
*     21-OCT-2013 (DSB):
*        Removed edge width arguments.
*     {enter_further_changes_here}

*  Copyright:
*     Copyright (C) 2010-2013 Science & Technology Facilities Council.
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
#include "mers.h"
#include "sae_par.h"

/* SMURF includes */
#include "libsmf/smf.h"
#include "libsmf/smf_err.h"
#include "libsmf/smf_typ.h"

void smf_scale2freq( double f_edgesmall, double f_edgelarge,
                     const smfHead *hdr, double *f_edgelow,
                     double *f_edgehigh, int *status ){

/* Check the inherited status. */
   if( *status != SAI__OK ) return;

/* Return without action if neither spatial scale was supplied. */
   if( f_edgesmall || f_edgelarge ) {

/* Check the supplied header is usable. If not, report an error. */
      if( !hdr ) {
        *status = SAI__ERROR;
        errRep( "", "smf_scale2freq: FILT_EDGE_SMALLSCALE or "
                "FILT_EDGE_LARGESCALE, but no smfHead supplied",
                 status );

      } else if ( hdr->scanvel <= 0 ) {
        *status = SMF__TELSTAT;
        errRep( "", "smf_scale2freq: FILT_EDGE_SMALLSCALE or "
                "FILT_EDGE_LARGESCALE, but telescope was stationary",
                 status );

/* Ohterwise convert the supplied values. */
      } else {
         msgOutiff( MSG__VERB, "", "smf_scale2freq: Based on a slew "
                    "speed of %.1lf arcsec/sec, setting:", status,
                    hdr->scanvel );

         if( f_edgesmall > 0.0 ) {
            *f_edgelow = hdr->scanvel / f_edgesmall;
            msgOutiff( MSG__VERB, "", "smf_scale2freq: FILT_EDGELOW = "
                       "%.3lf Hz (> %.1lf arcsec scales)", status, *f_edgelow,
                       f_edgesmall );
         }

         if( f_edgelarge > 0.0 ) {
            *f_edgehigh = hdr->scanvel / f_edgelarge;
            msgOutiff( MSG__VERB, "", "smf_scale2freq: FILT_EDGEHIGH = "
                       "%.3lf Hz (< %.1lf arcsec scales)", status, *f_edgehigh,
                       f_edgelarge );
         }
      }
   }
}
