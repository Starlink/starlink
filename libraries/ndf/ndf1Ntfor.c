#include <stdlib.h>
#include "sae_par.h"
#include "dat_par.h"
#include "ndf1.h"
#include "mers.h"
#include "ems.h"
#include "ndf_ast.h"

void ndf1Ntfor( const char *forfil, NdfFCB *fcb, int keep, HDSLoc **ndfloc,
                char *ndfnam, size_t ndfnam_length, size_t *lnam, int *status ){
/*
*+
*  Name:
*     ndf1Ntfor

*  Purpose:
*     Identify a native NDF to be associated with a foreign file.

*  Synopsis:
*     void ndf1Ntfor( const char *forfil, NdfFCB *fcb, int keep,
*                     HDSLoc **ndfloc, char *ndfnam, size_t ndfnam_length,
*                     size_t *lnam, int *status )

*  Description:
*     This function accepts the name of a foreign format file from (or to)
*     which data conversion is to be performed and identifies an NDF object
*     which can be associated with it and used to hold the (converted)
*     native NDF format version of the data. The NDF name is obtained by
*     translating an appropriate environment variable. A default NDF name
*     is supplied if this does not yield a suitable result. The identified
*     NDF is not actually created by this function.

*  Parameters:
*     forfil
*        Pointer to a null terminated string holding the name of the
*        foreign format file, optionally ending with a foreign extension
*        specifier.
*     fcb
*        Pointer to an object describing the foreign file format (must be
*        non-NULL).
*     keep
*        If a non-zero value is supplied, it indicates that the NDF will be
*        kept. Otherwise it will be a temporary object.
*     *ndfloc
*        Returned holding the locator which, in conjunction with the
*        "ndfnam" value, identifies the NDF. On successful exit, this
*        locator will either have the root locator value NULL (in
*        which case "ndfnam" contains the full NDF name) or will be an
*        active locator (in which case "ndfnam" contains the relative name
*        of the NDF). If active, this locator should be annulled by the
*        caller when no longer required.
*     ndfnam
*        Pointer to an array in which to return a null terminated string
*        holding absolute or relative HDS name which, in conjunction with
*        the "ndfloc" value, identifies the NDF object.
*     ndfnam_length
*        The length of the supplied 'ndfnam' array. This should include
*        room for the terminating null.
*     *lnam
*        Returned holding the number of characters in the "ndfnam" value.
*     *status
*        The global status.

*  Notes:
*     -  If this function is called with "status" set, then an invalid
*     locator will be returned via the "ndfloc" parameter. The same value
*     will also be returned if this function should fail for any reason.

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
*     xxx (DSB):
*        Original version, based on equivalent Fortran function by RFWS.

*-
*/

/* Local Variables: */
   char *envname;        /* Buffer for environment variable name */
   char name[ NDF__SZREF + 1 ];    /* Buffer for raw NDF name */
   int def;              /* Environment variable defined? */
   int nc;               /* Nu,ber of characters in string */

/* Set an initial null value for the "ndfloc" parameter. */
   *ndfloc = NULL;

/* Check inherited global status. */
   if( *status != SAI__OK ) return;

/* Attempt to translate the appropriate environment variable to obtain
   the native NDF name. */
   envname = astAppendStringf( NULL, &nc, keep?"NDF_KEEP_%s":"NDF_TEMP_%s",
                               fcb->name );
   if( *status == SAI__OK ) {
      ndf1Gtenv( envname, &def, name, sizeof( name ), lnam, status );
      envname = astFree( envname );
   }

/* If a non-blank name was obtained, return a root locator value to
   accompany it. */
   if( *status == SAI__OK ) {
      if( *lnam != 0 ) {
         *ndfloc = NULL;

/* Mark the error stack to prevent any interference with previously
   defined message tokens and define standard message tokens for
   substitution into the NDF name. */
         errMark();
         ndf1Cvtok( forfil, fcb, NULL, " ", status );

/* Substitute these token values into the NDF name, returning the
   result and its length. Use a low-level (EMS) function to ensure the
   message text supplied is used without change. */
         emsMload( " ", name, ndfnam, &nc, status );
         *lnam = nc;

/* Release the error stack. */
         errRlse();
      }
   }

/* If a (non-blank) NDF name was not obtained, then obtain a default
   locator and name instead. */
   if( *status == SAI__OK ) {
      if( *lnam == 0 ) {
         ndf1Dnfor( forfil, fcb, keep, ndfloc, ndfnam, ndfnam_length, lnam,
                    status );
      }
   }

/* If an error occurred, then return an invalid locator value. */
   if( *status != SAI__OK ) *ndfloc = NULL;

/* Call error tracing function and exit. */
   if( *status != SAI__OK ) ndf1Trace( "ndf1Ntfor", status );
}

