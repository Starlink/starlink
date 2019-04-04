#include <stdlib.h>
#include "sae_par.h"
#include "dat_par.h"
#include "ndf1.h"
#include "ndf_ast.h"
#include <string.h>
#include "ndf.h"
#include "mers.h"

void ndfIsin_( int indf1, int indf2, int *isin, int *status ){
/*
*+
*  Name:
*     ndfIsin

*  Purpose:
*     See if one NDF is contained within another NDF.

*  Synopsis:
*     void ndfIsin( int indf1, int indf2, int *isin, int *status )

*  Description:
*     This function returns a logical flag indicating if the first supplied
*     NDF is contained within an extension of the second supplied NDF. The
*     search is recursive, so for instance a true value will be returned if
*     the second supplied NDF is contained within an extension of an
*     intermediate NDF that is contained within an extension of the first
*     supplied NDF.

*  Parameters:
*     indf1
*        The first NDF.
*     indf2
*        The second NDF.
*     *isin
*        Returned holding the non-zero of the first NDF is contained within
*        an extension of the second NDF, and zero otherwise.
*     *status
*        The global status.

*  Notes:
*     -  A non-zero value is returned if the two supplied NDF identifiers
*     refer to the same base NDF.
*     -  If an identifier for an NDF section is supplied to this function,
*     then the search will be applied to the associated base NDF.
*     -  If this function is called with "status" set, then a value of zero
*     will be returned for the "isin" parameter, although no further
*     processing will occur. The same value will also be returned if the
*     function should fail for any reason.

*  Copyright:
*     Copyright (C) 2018 East Asian Observatory
*     All rights reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or modify
*     it under the terms of the GNU General Public License as published by
*     the Free Software Foundation; either version 2 of the License, or (at
*     your option) any later version.
*
*     This program is distributed in the hope that it will be useful,but
*     WITHOUT ANY WARRANTY; without even the implied warranty of
*     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
*     General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 51 Franklin Street,Fifth Floor, Boston, MA
*     02110-1301, USA

*  Authors:
*     RFWS: R.F. Warren-Smith (STARLINK)
*     DSB: David S. Berry (EAO)

*  History:
*     3-APR-2019 (DSB):
*        Original version, based on equivalent Fortran function by RFWS.

*-
*/

/* Local Variables: */
   NdfACB *acb1;         /* Pointer to 1st NDF entry in the ACB */
   NdfACB *acb2;         /* Pointer to 2nd NDF entry in the ACB */
   char file1[ NDF__SZFIL + 1 ];   /* Path of 1st NDF container file */
   char file2[ NDF__SZFIL + 1 ];   /* Path of 2nd NDF container file */
   char path1[ NDF__SZPTH + 1 ];   /* Path of 1st NDF within container
                                     file */
   char path2[ NDF__SZPTH + 1 ];   /* Path of 2nd NDF within container
                                     file */
   int nlev1;            /* Depth of 1st NDF within container file */
   int nlev2;            /* Depth of 2nd NDF within container file */
   size_t plen2;         /* Used length of PATH2 */

/* Set an initial value for the returned "isin" flag. */
   *isin = 0;

/* Check inherited global status. */
   if( *status != SAI__OK ) return;

/* Ensure the NDF library has been initialised. */
   NDF_INIT( status );

/* Import the NDF identifiers. */
   ndf1Impid( indf1, &acb1, status );
   ndf1Impid( indf2, &acb2, status );

/* For each NDF, obtain an index to the data object entry in the DCB */
   if( *status == SAI__OK ) {

/* Obtain HDS locators for the two data object entries in the DCB, and
   get the paths to the container files, and the paths to the objects
   within the container files. */
      hdsTrace( acb1->dcb->loc, &nlev1, path1, file1, status,
                sizeof( path1 ), sizeof( file1 ) );
      hdsTrace( acb2->dcb->loc, &nlev2, path2, file2, status,
                sizeof( path2 ), sizeof( file2 ) );

/* Check that the container files are the same, and that the depth of the
   second NDF is less than or equal to the depth of the first NDF. */
      if( !strcmp( file1, file2 ) && nlev1 >= nlev2 ) {

/* See if the HDS path for the first NDF starts with the HDS path for
   the second NDF. If so, the NDFs are equal or the first is a child of
   the second. */
         plen2 = astChrLen( path2 );
         *isin = !strncmp( path1, path2, plen2 );

      }
   }

/* If an error occurred, then return zero */
   if( *status != SAI__OK ) {
      *isin = 0;

/* Report context information and call the error tracing function. */
      errRep( " ", "ndfIsin: Error checking if an NDF is contained within "
              "another NDF.", status );
      ndf1Trace( "ndfIsin", status );
   }

/* Restablish the original AST status pointer */
   NDF_FINAL

}

