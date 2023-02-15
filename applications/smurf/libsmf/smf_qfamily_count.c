/*
*+
*  Name:
*     smf_qfamily_count

*  Purpose:
*     Return number of bits in a quality family

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     Library routine

*  Invocation:
*     dim_t smf_qfamily_count( smf_qfam_t qfamily, int * status );

*  Arguments:
*     qfamily = smf_qfam_t (Given)
*        Quality family.
*     status = int* (Given and Returned)
*        Pointer to global status.

*  Description:
*     Returns the number of quality bits defined in the supplied quality
*     family.

*  Returned Value:
*     Returns the number of quality bits for the supplied family. Returns
*     0 on error.

*  Authors:
*     TIMJ: Tim Jenness (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     2010-06-23 (TIMJ):
*        Initial version
*     {enter_further_changes_here}

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
#include "mers.h"

dim_t smf_qfamily_count( smf_qfam_t family, int * status ) {

  dim_t retval = 0;

  if (*status != SAI__OK) return retval;

  /* work out how many quality items are in this family */
  switch( family ) {
  case SMF__QFAM_TSERIES:
    retval = SMF__NQBITS_TSERIES;
    break;
  case SMF__QFAM_MAP:
    retval = SMF__NQBITS_MAP;
    break;
  case SMF__QFAM_TCOMP:
    retval = SMF__NQBITS_TCOMP;
    break;
  case SMF__QFAM_NULL:
    retval = 0;
    break;
  default:
    *status = SAI__ERROR;
    errRepf( "", "Unsupported quality family with value %d",
             status, (int)family );
  }

  return retval;
}
