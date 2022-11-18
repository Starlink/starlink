/*
*+
*  Name:
*     smf_validate_tcs_position

*  Purpose:
*     Validate telescope position by comparing TCS_TR_AC1/2 and DC1/2 values.

*  Language:
*     C

*  Type of Module:
*     C function

*  Invocation:
*     int smf_validate_tcs_position( smfHead * hdr, double tolerance,
*                                    int setbad, int * status );

*  Arguments:
*     hdr = smfHead * (Given)
*        smfHead to be examined.
*     tolerance = double
*        Positional tolerance (arcseconds).
*     setbad = int
*        Whether to update the jos_drcontrol bitfield of errant samples.
*     status = int * (Given & Returned)
*        Pointer to global status.

*  Returned Value:
*     1 if all positions were valid, 0 otherwise.

*  Description:
*     Computes the separation between the TCS_TR_AC1/2 and DC1/2 positions
*     in the JCMTSTATE.  If any separation values exceed the given tolerance
*     then return false.  Additionally if setbad is true then set the
*     DRCNTRL__TCS_POSN_BIT of jos_drcontrol.

*  History:
*     2022-10-07 (GSB):
*        Initial version.

*  Copyright:
*     Copyright (C) 2022 East Asian Observatory.
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

#include "sae_par.h"
#include "mers.h"
#include "prm_par.h"
#include "mers.h"
#include "star/pal.h"
#include "star/palmac.h"

#include "smf.h"
#include "smf_err.h"

/* Indent for informational messages */
#define INDENT "   "

int smf_validate_tcs_position(
        smfHead* hdr, double tolerance, int setbad, int* status) {
    JCMTState *state;
    dim_t iframe;
    dim_t nbad;
    double lat0;
    double lat;
    double lon0;
    double lon;
    double sep;

    /* Check inherited status. */
    if (*status != SAI__OK) return 1;

    /* Loop round all time slices. */
    nbad = 0;
    state = hdr->allState;
    for (iframe = 0; iframe < hdr->nframes; iframe ++, state ++) {
        lon = state->tcs_tr_ac1;
        lat = state->tcs_tr_ac2;
        lon0 = state->tcs_tr_dc1;
        lat0 = state->tcs_tr_dc2;

        if (lon != VAL__BADD && lat != VAL__BADD &&
                lon0 != VAL__BADD && lat0 != VAL__BADD) {
            /* Check the separation between demand and actual position. */
            sep = palDsep(lon, lat, lon0, lat0) * PAL__DR2AS;

            if (sep > tolerance) {
                nbad ++;

                if (setbad) {
                    state->jos_drcontrol |= DRCNTRL__TCS_POSN_BIT;
                }
                else {
                    break;
                }
            }
        }
    }

    /* If time slices were invalidated, tell the user. */
    if (setbad && nbad > 0) {
        msgOutf( " ", INDENT "WARNING: Rejecting %zu time-slices due to "
                 "extreme excursion", status, nbad );
    }

    return nbad ? 0 : 1;
}
