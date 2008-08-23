/*
*+
*  Name:
*     smf_reduce_dark

*  Purpose:
*     Process a dark time series if necessary.

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     SMURF subroutine

*  Invocation:
*     smf_reduce_dark( const smfData * indark, smfData **outdark,
*                      int * status );

*  Arguments:
*     indark = const smfData * (Given)
*        Dark to be processed.
*     outdark = smfData ** (Returned)
*        Pointer to return processed dark. *outdark is NULL
*        if the input dark has already been reduced (is 2d).
*     status = int* (Given and Returned)
*        Pointer to global status.

*  Description:
*     This routine reduces the input dark time series by calculating
*     a mean signal for each bolometer. A new smfData is created with the
*     result. If the dark is already 2D the routine returns without action.
*     The caller should check for this condition by making seeing if 
*     *outdark is non-NULL. This is done for efficiency.

*  Notes:
*     - Use smf_close_file to free the reduced dark memory.

*  Authors:
*     Tim Jenness (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     2008-07-17 (TIMJ):
*        Initial version

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

void smf_reduce_dark( const smfData *indark, smfData **outdark, 
                      int *status ) {

  if (*status != SAI__OK) return;

  /* check we have a dark (inefficient if the caller has already 
     done so but we can not guarantee) */
  if (!smf_isdark( indark, status)) {
    *status = SAI__ERROR;
    errRep( " ", "Attempting to reduce an observation as if it is a dark"
            " but this file is not a dark", status);
    return;
  }

  /* see if we have a 2d dark input (likely reduced) */
  if (indark->ndims == 2 ||
      (indark->ndims == 3 && (indark->dims)[2] == 1) ) {
    *outdark = NULL;
    return;
  }

  /* now calculate the average and standard deviation. Retaining the result
     as integers. Flag any bolometers that have constant signal or a signal
     to noise less than 1. */
  smf_collapse_tseries( indark, 0, 1.0, 1, SMF__NULL, outdark, status );

  return;
}
