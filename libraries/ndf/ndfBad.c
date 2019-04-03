#include "sae_par.h"
#include "dat_par.h"
#include "ndf1.h"
#include "ndf.h"
#include "mers.h"

void ndfBad_( int indf, const char *comp, int check, int *bad, int *status ){
/*
*+
*  Name:
*     ndfBad

*  Purpose:
*     Determine if an NDF array component may contain bad pixels.

*  Synopsis:
*     void ndfBad( int indf, const char *comp, int check, int *bad,
*                  int *status )

*  Description:
*     This function returns a logical value indicating whether an array
*     component of an NDF may contain bad pixels for which checks must be
*     made when the array's values are processed. Only if the returned
*     value is zero can such checks be omitted. If the "check" parameter to
*     this function is set non-zero, then it will also perform an explicit
*     check (if necessary) to see whether bad pixels are actually present.

*  Parameters:
*     indf
*        NDF identifier.
*     comp
*        Pointer to a null terminated string holding the name of the NDF
*        array component: "DATA", "QUALITY" or "VARIANCE".
*     check
*        Whether to perform an explicit check to see whether bad pixels are
*        actually present.
*     *bad
*        Returned holding the whether it is necessary to check for bad
*        pixels when processing the array's values.
*     *status
*        The global status.

*  Notes:
*     -  A comma-separated list of component names may also be supplied, in
*     which case the function returns the logical "OR" of the results for
*     each component.
*     -  If "check" is set zero, then the returned value of "bad" will
*     indicate whether bad pixels might be present and should therefore be
*     checked for during subsequent processing. However, even if "bad" is
*     returned non-zero in such circumstances, it is still possible that
*     there may not actually be any bad pixels present (for instance, in an
*     NDF section, the accessible region of an array component might happen
*     to avoid all the bad pixels).
*     -  If "check" is set non-zero, then an explicit check will be made,
*     if necessary, to ensure that "bad" is only returned non-zero if bad
*     pixels are actually present.
*     -  If a component is mapped for access through the identifier
*     supplied, then the value of "bad" will refer to the actual mapped
*     values. It may differ from its original (unmapped) value if
*     conversion errors occurred during the mapping process, if an
*     initialisation option of "/ZERO" was specified for a component whose
*     value was initially undefined, or if the mapped values have
*     subsequently been modified.
*     -  A "bad"=non-zero result will be returned for any components which
*     are in an undefined state, except in the case of the QUALITY
*     component for which a zero result is always returned under these
*     circumstances.

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
   NdfACB *acb;          /* Pointer to the NDF entry in the ACB */


   *bad = 0;

/* Check inherited global status. */
   if( *status != SAI__OK ) return;

/* Ensure the NDF library has been initialised. */
   NDF_INIT( status );

/* Import the NDF identifier. */
   ndf1Impid( indf, &acb, status );

/* Determine if bad pixels may be present. */
   ndf1Bad( acb, comp, check, bad, status );

/* If an error occurred, then report context information and call the
   error tracing function. */
   if( *status != SAI__OK ) {
      errRep( " ", "ndfBad: Error determining if an NDF array component "
              "may contain bad pixels.", status );
      ndf1Trace( "ndfBad", status );
   }

/* Restablish the original AST status pointer */
   NDF_FINAL

}

