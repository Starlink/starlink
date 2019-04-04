#include <stdlib.h>
#include "sae_par.h"
#include "dat_par.h"
#include "ndf1.h"
#include "ndf.h"
#include "mers.h"

void ndfMapz_( int indf, const char *comp, const char *type,
               const char *mmod, void *rpntr[], void *ipntr[], size_t *el,
               int *status ){
/*
*+
*  Name:
*     ndfMapz

*  Purpose:
*     Obtain complex mapped access to an array component of an NDF.

*  Synopsis:
*     void ndfMapz( int indf, const char *comp, const char *type,
*                    const char *mmod, void *rpntr[], void *ipntr[],
*                    size_t *el, int *status )

*  Description:
*     This function obtains complex mapped access to an array component of
*     an NDF, returning pointers to the mapped real and imaginary values
*     and a count of the number of elements mapped.

*  Parameters:
*     indf
*        NDF identifier.
*     comp
*        Pointer to a null terminated string holding the name of the NDF
*        array component to be mapped: "DATA" or "VARIANCE" (or "ERROR").
*     type
*        Pointer to a null terminated string holding the numeric type to be
*        used for access (e.g.  "_REAL").
*     mmod
*        Pointer to a null terminated string holding the mapping mode for
*        access to the array: "READ", "UPDATE" or "WRITE", with an optional
*        initialisation mode "/ZERO" or "/BAD" appended.
*     rpntr
*        Returned holding the pointer(s) to the mapped real (i.e. non-
*        imaginary) values (see the Notes section).
*     ipntr
*        Returned holding the pointer(s) to the mapped imaginary values
*        (see the Notes section).
*     *el
*        Returned holding the number of elements mapped.
*     *status
*        The global status.

*  Notes:
*     -  A comma-separated list of component names may also be given, in
*     which case the function will map all the requested components using
*     the same data type and mapping mode. Pointers to the values of these
*     mapped components will be returned (in the specified order) in the
*     elements of the arrays "rpntr" and "ipntr", which must be of
*     sufficient size to accommodate them.
*     -  Access to an NDF's QUALITY component is not available using this
*     function.
*     -  If this function is called with "status" set, then a value of 1
*     will be returned for the "el" parameter, although no further
*     processing will occur.  The same value will also be returned if the
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
   NdfACB *acb;          /* Pointer to NDF entry in the ACB */

/* Ensure the NDF library has been initialised. */
   NDF_INIT( status );

/* Check inherited global status. */
   if( *status == SAI__OK ) {

/* Import the NDF identifier. */
      ndf1Impid( indf, &acb, status );

/* Map the array component(s). */
      ndf1Map( acb, comp, type, 1, mmod, rpntr, ipntr, status );

/* Obtain the number of mapped data elements. */
      if( *status == SAI__OK ) arySize( acb->did, el, status );

/* If an error occurred, then report context information and call the
   error tracing function. */
      if( *status != SAI__OK ) {
         errRep( " ", "ndfMapz: Error obtaining complex mapped access to "
                 "an array component of an NDF.", status );
         ndf1Trace( "ndfMapz", status );
      }
   }

/* Under error conditions, return a "safe" value of "el". */
   if( *status != SAI__OK ) *el = 1;

/* Restablish the original AST status pointer */
   NDF_FINAL

}

