/*
*+
*  Name:
*     smf_isdark

*  Purpose:
*     Decide whether the supplied smfData is a dark observation

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     SMURF subroutine

*  Invocation:
*     int smf_isdark( const smfData * infile, int * status );

*  Arguments:
*     infile = const smfData * (Given)
*        smfData to test for darkness.
*     status = int* (Given and Returned)
*        Pointer to global status.

*  Returned Value:
*     Boolean int. True if a dark, otherwise false.

*  Description:
*     Tests a smfData to see if it is a dark observation.

*  Authors:
*     Tim Jenness (JAC, Hawaii)
*     {enter_new_authors_here}

*  Notes:
*     - The smfData must contain a smfHead and that must contain
*       a fitshdr. This prevents us silently assuming that the file
*       is not a dark.

*  History:
*     2008-07-17 (TIMJ):
*        Initial version
*     2008-07-22 (TIMJ):
*        SHUTTER is now a float.
*     2008-07-31 (TIMJ):
*        but leave in support for string.

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
#include "ast_err.h"

/* SMURF routines */
#include "smf.h"
#include "smf_typ.h"
#include "smf_err.h"

int smf_isdark( const smfData * indata, int * status ) {
  smfHead * hdr;
  double shutval;
  char shutter[SZFITSCARD];

  if (*status != SAI__OK) return 0;

  if (indata == NULL) {
    *status = SAI__ERROR;
    errRep( " ", "NULL pointer given to smf_isdark"
            " (possible programming error)", status);
    return 0;
  }

  hdr = indata->hdr;

  /* Shutter is no a double. 0 indicates closed */
  smf_fits_getD( indata->hdr, "SHUTTER", &shutval, status );

  /* Old data was a string. Handle it until we no longer need to */
  if (*status == AST__FTCNV) {
    errAnnul( status );
    smf_fits_getS( indata->hdr, "SHUTTER", shutter, sizeof(shutter), status);
    if (strcmp(shutter, "CLOSED") == 0) {
      return 1;
    } else {
      return 0;
    }
  }


  if (shutval < 0.00001 ) {
    return 1;
  }

  return 0;
}
