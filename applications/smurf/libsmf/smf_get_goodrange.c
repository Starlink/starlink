/*
*+
*  Name:
*     smf_get_goodrange

*  Purpose:
*     Determine the timeslice range that is not flagged

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     C function

*  Invocation:
*     void smf_get_goodrange( const smf_qual_t *quality, dim_t ntslice,
*                             dim_t tstride, smf_qual_t mask,
*                             dim_t *istart, dim_t *iend, int *status );

*  Arguments:
*     quality = const smf_qual_t *(Given)
*        Pointer to quality array
*     ntslice = dim_t (Given)
*        Number of tslices.
*     tstride = dim_t (Given)
*        How many elements to skip to get to the next time slice for the
*        current bolometer in quality
*     mask = smf_qual_t (Given)
*        Which quality bits used to determine the range.
*     istart = dim_t (Returned)
*        Starting good index along the time axis
*     iend = dim_t (Returned)
*        Ending good index along the time axis.
*     status = int * (Given and Returned)
*        Pointer to inherited status.

*  Returned Value:

*  Description:
*     The purpose of this function is to identify the range of good
*     indices along the time axis that exclude things like padding and
*     apodization at the ends of a time series (e.g. setting mask to
*     SMF__Q_PAD | SMF__Q_APOD). Only a single bolometer is checked since
*     it is assumed that all of the bolometers will have these same quality
*     bits set.

*  Authors:
*     Edward Chapin (UBC)
*     {enter_new_authors_here}

*  History:
*     2009-11-17 (EC)
*        Initial version factored out from smf_apodize

*  Copyright:
*     Copyright (C) 2009 University of British Columbia.
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
#include "ast.h"
#include "mers.h"
#include "smf.h"
#include "smf_err.h"

#define FUNC_NAME "smf_get_goodrange"

void smf_get_goodrange( const smf_qual_t *quality, dim_t ntslice,
                        dim_t tstride, smf_qual_t mask,
                        dim_t *istart, dim_t *iend, int *status ) {

  dim_t i1;
  dim_t i2;
  dim_t i;

   /* Check the inherited status */
   if ( *status != SAI__OK ) return;

   /* Check for valid quality pointer data */
   if( !quality ) {
     *status = SAI__ERROR;
     errRep(" ", FUNC_NAME
            ": NULL quality pointer provided", status);
    return;
   }

   i1 = 0;
   i2 = ntslice-1;

   for( i=0; i<ntslice; i++ ) { /* First sample */
     if( !(quality[i*tstride]&mask) ) break;
   }

   if( i==ntslice ) {
     *status=SAI__ERROR;
     errRepf( "", FUNC_NAME ": Entire array (%zu slices) is flagged!",
             status, ntslice );
   } else {
     i1 = i;
     /* Last sample if first was found */
     for( i=ntslice-1; i>0; i-- ) {
       if( !(quality[i*tstride]&mask) ) break;
     }
     i2 = i;
   }

   if( istart ) {
     *istart = i1;
   }

   if( iend ) {
     *iend = i2;
   }
}
