/*
*+
*  Name:
*     smf_samedims_smfData

*  Purpose:
*     Return 1 if the two smfData's have the same dimensions

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     C function

*  Invocation:
*     int smf_samedims_smfData( const smfData *data1, const smfData *data2,
*                               int *status );

*  Arguments:
*     data1 = const smfData * (Given)
*        Pointer to a smfData
*     data2 = const smfData * (Given)
*        Pointer to another smfData
*     status = int * (Given and Returned)
*        Pointer to inherited status.

*  Returned Value:
*     1 if two smfDatas have the same dimensions, otherwise 0.

*  Description:
*     Check ndims, and the individual dims to see that they are the same

*  Authors:
*     Edward Chapin (UBC)
*     {enter_new_authors_here}

*  History:
*     2010-03-30 (EC)
*        Initial version.
*     {enter_further_changes_here}

*  Copyright:
*     Copyright (C) 2010 University of British Columbia.
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

#define FUNC_NAME "smf_samedims_smfData"

int smf_samedims_smfData( const smfData *data1, const smfData *data2,
                           int *status ) {

  dim_t i;
  int retval=0;

   /* Check the inherited status */
   if ( *status != SAI__OK ) return retval;

   if( (!data1) || (!data2) ) {
     *status = SAI__ERROR;
     errRep("", FUNC_NAME ": Error, null smfData pointer supplied", status);
     return retval;
   }

   if( data1->ndims == data2->ndims ) {
     retval=1;
     for( i=0; i<data1->ndims-1; i++ ) {
       if( data1->dims[i] != data2->dims[i] ) {
         retval=0;
       }
     }
   }

   return retval;
}
