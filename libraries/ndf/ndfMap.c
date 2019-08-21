#include <stdlib.h>
#include "sae_par.h"
#include "dat_par.h"
#include "ndf1.h"
#include "ndf.h"
#include "mers.h"

void ndfMap_( int indf, const char *comp, const char *type,
              const char *mmod, void *pntr[], size_t *el, int *status ){
/*
*+
*  Name:
*     ndfMap

*  Purpose:
*     Obtain mapped access to an array component of an NDF.

*  Synopsis:
*     void ndfMap( int indf, const char *comp, const char *type,
*                   const char *mmod, void *pntr[], size_t *el, int *status )

*  Description:
*     This function obtains mapped access to an array component of an NDF,
*     returning a pointer to the mapped values and a count of the number of
*     elements mapped.

*  Parameters:
*     indf
*        NDF identifier.
*     comp
*        Pointer to a null terminated string holding the name of the NDF
*        array component to be mapped: "DATA", "QUALITY" or "VARIANCE" (or
*        "ERROR").
*     type
*        Pointer to a null terminated string holding the numeric type to be
*        used for access (e.g. "_REAL").
*     mmod
*        Pointer to a null terminated string holding the mapping mode for
*        access to the array: "READ", "UPDATE" or "WRITE", with an optional
*        initialisation mode "/BAD" or "/ZERO" appended.
*     pntr
*        Returned holding the pointer(s) to the mapped values (see the
*        Notes section).
*     *el
*        Returned holding the number of elements mapped.
*     *status
*        The global status.

*  Notes:
*     -  A comma-separated list of component names may also be given, in
*     which case the function will map all the requested components using
*     the same data type and mapping mode. Pointers to the values of these
*     mapped components will be returned (in the specified order) in the
*     elements of the array "pntr", which must be of sufficient size to
*     accommodate them.
*     -  The result of mapping the QUALITY component with a data type other
*     than "_UBYTE" is not defined and should not be used.
*     -  If the array is stored in scaled form, then the mapped values will
*     be the result of applying the approproate scale and zero terms to the
*     elements of the underlying array.
*     -  If the array is stored in delta compressed form, then the mapped
*     values will be the original uncompressed values.
*     -  Scaled and delta arrays are read-only. An error will be reported
*     if the array is stored in scaled or delta form and the access mode is
*     UPDATE or WRITE.
*     -  If the "mmod" parameter includes the "/ZERO" option, the bad pixel
*     flag for the array will be reset to false to indicate that there are
*     no bad values present in the array (see ndfBad). If any bad values
*     are then placed into the mapped array, ndfSbad should normally be
*     called to set the bad pixel flag true. If this is not done, the bad
*     values in the array may be treated as literal values by subsequent
*     applications.
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
   void *dummy;          /* Dummy variable */

/* Ensure the NDF library has been initialised. */
   NDF_INIT( status );

/* Check inherited global status. */
   if( *status == SAI__OK ) {

/* Import the NDF identifier. */
      ndf1Impid( indf, &acb, status );

/* Map the array component(s). */
      ndf1Map( acb, comp, type, 0, mmod, pntr, &dummy, status );

/* Calculate the number of mapped data elements. */
      if( *status == SAI__OK ) arySize( acb->did, el, status );

/* If an error occurred, then report context information and call the
   error tracing function. */
      if( *status != SAI__OK ) {
         errRep( " ", "ndfMap: Error obtaining mapped access to an array "
                 "component of an NDF.", status );
         ndf1Trace( "ndfMap", status );
      }
   }

/* Under error conditions, return a "safe" value of "el". */
   if( *status != SAI__OK ) *el = 1;

/* Restablish the original AST status pointer */
   NDF_FINAL

}

