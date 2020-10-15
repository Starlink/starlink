/*
*+
*  Name:
*     smf_checkdets

*  Purpose:
*     Check a group of detector names.

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     C function

*  Invocation:
*     void smf_checkdets( Grp *detgrp, smfData *data, int *status )

*  Arguments:
*     detgrp = Grp * (Given)
*        Group of detector names supplied by the user.
*     data = smfData * (Given)
*        The input data holding the list of available detectors.
*     status = int * (Given and Returned)
*        Pointer to the inherited status.

*  Description:
*     This function checks each detetor name in the supplied group, and
*     issues a warning if any are found that are not listed in the
*     supplied data header.
*
*     If the first detector name starts with a minus sign, the minus sign
*     is removed, and the supplied group is modified to hold all available
*     detector names except for those that were initially in the supplied
*     group.

*  Authors:
*     David S Berry (JAC, UCLan)
*     {enter_new_authors_here}

*  History:
*     25-OCT-2007 (DSB):
*        Initial version.
*     {enter_further_changes_here}

*  Copyright:
*     Copyright (C) 2007 Science & Technology Facilities Council.
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
#include "ast.h"
#include "grp.h"
#include "sae_par.h"
#include "mers.h"

/* SMURF includes */
#include "libsmf/smf.h"

void smf_checkdets( Grp *detgrp, smfData *data, int *status ){

/* Local Variables */
   Grp *alldets = NULL;       /* GRP group holding all detector labels */
   char *baddets = NULL;      /* A buffer for a list of bad detector names */
   char *buf = NULL;          /* A buffer for a text pointer */
   char detname[ GRP__SZNAM ];/* Detector name */
   const char *lab = NULL;    /* Pointer to start of next detector name */
   dim_t irec;                /* Index of current input detector */
   dim_t ndetgrp;             /* Size of "detgrp" group */
   int baddetslen;            /* Used length of "baddets" string */
   int exclude;               /* Exclude the listed detectors? */
   size_t found;              /* Was current detector name found in detgrp? */

/* Check inherited status. Also check a group was supplied. */
   if( *status != SAI__OK || !detgrp ) return;

/* Check the first element in the supplied group. If it starts with a
   minus sign, remove it and set a flag indicating that the supplied
   detectors are to be excluded rather than included. */
   buf = detname;
   grpGet( detgrp, 1, 1, &buf, GRP__SZNAM, status );
   astRemoveLeadingBlanks( detname );
   if( detname[ 0 ] == '-' ) {
      detname[ 0 ] = ' ';
      astRemoveLeadingBlanks( detname );
      grpPut1( detgrp, detname, 1, status );
      exclude = 1;
   } else {
      exclude = 0;
   }

/* Create a group holding the names of all detector names in the data. */
   alldets = grpNew( "All detector names", status );
   lab = data->hdr->detname;
   for( irec = 0; irec < (data->dims)[ 1 ]; irec++ ) {
      grpPut1( alldets, lab, 0, status );
      lab += strlen( lab ) + 1;
   }

/* Check that each entry in "detgrp" is also in "alldets". Issue a
   warning if any of the detector names specified in "detgrp"
   were not found in the data. */
   baddets = NULL;
   buf = detname;
   ndetgrp = grpGrpsz( detgrp, status );
   for( irec = 1; irec <= ndetgrp; irec++ ) {
      grpGet( detgrp, irec, 1, &buf, GRP__SZNAM, status );
      found = grpIndex( detname, alldets, 1, status );
      if( !found ) {
         baddets = astAppendString( baddets, &baddetslen, " " );
         baddets = astAppendString( baddets, &baddetslen, detname );
      }
   }

   if( baddets ) {
      msgSetc( "DETS", baddets );
      msgOutif( MSG__NORM, " ", "   WARNING: The following "
                "detector names supplied for parameter DETECTORS "
                "are not present in the data: ^DETS", status );
      msgBlank( status );
      baddets = astFree( baddets );
   }

/* If the supplied detectors are to be excluded, modify the group holding
   all detector names by replacing each detector name that is to be excluded
   with the string "REMOVE". */
   if( exclude ) {
      for( irec = 1; irec <= (data->dims)[ 1 ]; irec++ ) {
         grpGet( alldets, irec, 1, &buf, GRP__SZNAM, status );
         found = grpIndex( detname, detgrp, 1, status );
         if( found ) grpPut1( alldets, "REMOVE", irec, status );
      }

/* Empty the supplied group. */
      grpSetsz( detgrp, 0, status );

/* Copy all the remaining names from alldets to the supplied group. */
      for( irec = 1; irec <= (data->dims)[ 1 ]; irec++ ) {
         grpGet( alldets, irec, 1, &buf, GRP__SZNAM, status );
         if( strcmp( detname, "REMOVE" ) ) {
            grpPut1( detgrp, detname, 0, status );
         }
      }
   }

/* Free resources */
   grpDelet( &alldets, status );
}

