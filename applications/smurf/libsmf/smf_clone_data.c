/*
*+
*  Name:
*     smf_clone_data

*  Purpose:
*     Low-level clone of an input smfData struct

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     SMURF subroutine

*  Invocation:
*     smf_clone_data( smfData *data, int *status );

*  Arguments:
*     data = smfData* (Given)
*        Pointer to a smfData struct
*     status = int* (Given and Returned)
*        Pointer to global status.

*  Description:
*     This routine returns a pointer to a cloned version of the input.

*  Authors:
*     Andy Gibb (UBC)
*     {enter_new_authors_here}

*  History:
*     2005-12-09 (AGG):
*        Initial test version.
*     2005-12-14 (AGG):
*        Increment the reference counter
*     {enter_further_changes_here}

*  Copyright:
*     Copyright (C) 2005 Particle Physics and Astronomy Research Council.
*     University of British Columbia.
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

#if HAVE_CONFIG_H
#include <config.h>
#endif

#include <string.h>
#include <stdio.h>

#include "mers.h"
#include "prm_par.h"
#include "sae_par.h"
#include "msg_par.h"

#include "smurf_par.h"
#include "libsmf/smf.h"
#include "libsmf/smf_err.h"

void smf_clone_data ( smfData *idata, smfData **odata,
                      int *status __attribute__((unused)) ) {


  /* Set the output pointer */
  *odata = (smfData *)idata;

  /* Update the reference counter */
  (*odata)->refcount++;

}

