/*
*+
*  Name:
*     smf_isfft

*  Purpose:
*     Decide whether the supplied smfData is FFT'd data

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     SMURF subroutine

*  Invocation:
*     int smf_isfft( const smfData * infile, int * status );

*  Arguments:
*     infile = const smfData * (Given)
*        smfData to test.
*     status = int* (Given and Returned)
*        Pointer to global status.

*  Returned Value:
*     Boolean int. True if a FFT, otherwise false.

*  Description:
*     Tests a smfData to see if it is FFT'd data.

*  Authors:
*     Ed Chapin (UBC)
*     {enter_new_authors_here}

*  Notes:
*     - Currently just checks for reasonable dimensionality, should check
*       header as well.

*  History:
*     2008-07-13 (EC):
*        Initial version copied from smf_isfft.

*  Copyright:
*     Copyright (C) 2008 Science and Technology Facilities Council.
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

#if HAVE_CONFIG_H
#include <config.h>
#endif

/* System includes */
#include <stdlib.h>
#include <stdio.h>
#include <string.h>

/* Starlink includes */
#include "sae_par.h"
#include "mers.h"
#include "ndf.h"
#include "star/ndg.h"
#include "star/grp.h"
#include "msg_par.h"
#include "star/one.h"

/* SMURF routines */
#include "smf.h"
#include "smf_typ.h"
#include "smf_err.h"

int smf_isfft( const smfData * indata, int * status ) {
  smfHead * hdr;
  double shutval;

  if (*status != SAI__OK) return 0;

  if (indata == NULL) {
    *status = SAI__ERROR;
    errRep( " ", "NULL pointer given to smf_isfft"
            " (possible programming error)", status);
    return 0;
  }

  if( ( (indata->ndims==2) && (indata->dims[1]==2) ) || 
      ( (indata->ndims==4) && (indata->dims[3]==2) ) ) {
    return 1;
  }

  return 0;
}
