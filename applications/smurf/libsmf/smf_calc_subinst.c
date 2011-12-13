/*
 *+
 *  Name:
 *     smf_calc_subinst

 *  Purpose:
 *     Determines the sub-instrument identifier

 *  Language:
 *     Starlink ANSI C

 *  Type of Module:
 *     Library routine

 *  Invocation:
 *     subinst = smf_calc_subinst( const smfHead *hdr, int *status );

 *  Arguments:
 *     hdr = const smfHead * (Given)
 *        Header struct.
 *     status = int* (Given and Returned)
 *        Pointer to global status.

 *  Description:
 *     Determine the "sub-instrument" based on header information. For
 *     SCUBA-2 this really means simply whether it is a 850 or 450 micron
 *     data file. This is more reliable than using an equality check with
 *     the wavelength because the wavelength can change as filters are
 *     updated.

 *  Returned Value:
 *     smf_calc_subinst = smf_subinst_t
 *        Sub instrument enum constant. Returns SMF__SUBINST_NONE if the
 *        sub instrument can not be determined for whatever reason.

 *  Authors:
 *     Tim Jenness (JAC, Hawaii)
 *     {enter_new_authors_here}

 *  History:
 *     2010-01-08 (TIMJ):
 *        Initial Version

 *  Notes:
 *     Only useful for SCUBA-2 at present.

 *  Copyright:
 *     Copyright (C) 2010 Science and Technology Facilities Council.
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

#include "sae_par.h"

smf_subinst_t smf_calc_subinst ( const smfHead * hdr, int * status ) {
  double lambda = 0.0;    /* WAVELEN fits header value */
  smf_subinst_t subinst = SMF__SUBINST_NONE; /* Return value */

  if (*status != SAI__OK) return subinst;
  if (! smf_validate_smfHead( hdr, 1, 0, status ) ) return subinst;

  /* do not worry about non-SCUBA2 for now */
  if ( hdr->instrument != INST__SCUBA2 ) return subinst;

  /* Keep things simple and just look at the WAVELEN header */
  smf_fits_getD(hdr, "WAVELEN", &lambda, status );
  if (*status != SAI__OK) return subinst;

  if (lambda > 600.0E-6) {
    subinst = SMF__SUBINST_850;
  } else {
    subinst = SMF__SUBINST_450;
  }

  return subinst;

}
