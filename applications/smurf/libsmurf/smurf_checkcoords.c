/*
*+
*  Name:
*     CHECKCOORDS

*  Purpose:
*     Check for discrepancies between AZEL and TRACKING coordinates.

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     smurf_checkcoords( int *status );

*  Arguments:
*     status = int* (Given and Returned)
*        Pointer to global status.

*  Description:
*     This routine checks that the AZEL boresight positions stored in the
*     TCS_AZ_AC1/2 arrays in the JCMTSTATE extension of the supplied
*     input NDF are in agreement with the corresponding TRACKING positions
*     given by the TCS_TR_AC1/2 arrays. It does this by using AST to
*     convert each TCS_AZ_AC1/2 into the tracking system and then finding
*     the arc-distance from this converted position to the corresponding
*     TCS_TR_AC1/2 position. This is done for every time slice, and the
*     statistics of the resulting separations are displayed, in arc-seconds.
*     A warning message is reported if any separations larger than 3
*     arc-seconds are found.

*  ADAM Parameters:
*     IN = NDF (Read)
*        The time series to be checked.

*  Authors:
*     DSB: David Berry (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     25-NOV-2013 (DSB):
*        Initial version.
*     27-NOV-2013 (DSB):
*        Also check the detector positions for ACSIS data.
*     11-MAY-2017 (DSB):
*        Set the max expected discrepancy at 3 arc-seconds.

*  Copyright:
*     Copyright (C) 2017 East Asian Observatory.
*     Copyright (C) 2013 Science and Technology Facilities Council.
*     All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either version 3 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful,but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public
*     License along with this program; if not, write to the Free
*     Software Foundation, Inc., 51 Franklin Street,Fifth Floor, Boston,
*     MA 02110-1301, USA

*  Bugs:
*     {note_any_bugs_here}
*-
*/

#if HAVE_CONFIG_H
#include <config.h>
#endif

#include <stdio.h>
#include <ctype.h>
#include <float.h>
#include <stdlib.h>

/* STARLINK includes */
#include "ast.h"
#include "mers.h"
#include "ndf.h"
#include "par.h"
#include "sae_par.h"
#include "star/grp.h"

/* SMURF includes */
#include "smurf_typ.h"
#include "smurflib.h"
#include "libsmf/smf.h"


/* Local constants */
#define FUNC_NAME "smurf_checkcoords"

/* Main entry */
void smurf_checkcoords( int *status ) {

/* Local Variables */
   Grp *igrp = NULL;
   size_t i;
   size_t size;
   smfData *data = NULL;

/* Check inherited status */
   if (*status != SAI__OK) return;

/* begin an NDF context. */
   ndfBegin();

/* Get a group of input files */
   kpg1Rgndf( "IN", 0, 1, "  Give more NDFs...", &igrp, &size, status );

/* Loop round each one. */
   for( i = 1; i <= size; i++ ) {

/* Open the file. */
      smf_open_file( NULL, igrp, i, "READ", SMF__NOCREATE_DATA, &data, status );

/* Check the detector positions (for ACSIS data). */
      msgBlank( status );
      smf_check_detpos( data, 3.0, 1, status );

/* Calculate and display statistics of the AZEL <> TRACKING separations
   in the current file. */
      smf_check_coords( data, status );

/* Close the file. */
      smf_close_file( NULL, &data, status);
   }

/* Free resources. */
   grpDelet( &igrp, status );

/* End the NDF context. */
   ndfEnd( status );

/* If anything went wrong issue a context message. */
   if( *status != SAI__OK ) msgOutif( MSG__VERB, " ", "CHECKCOORDS failed.",
                                      status );
}

