#include "sae_par.h"
#include "dat_par.h"
#include "ndf1.h"
#include "ndf.h"
#include "mers.h"

void ndfMbad_( int badok, int indf1, int indf2, const char *comp, int check,
              int *bad, int *status ){
/*
*+
*  Name:
*     ndfMbad

*  Purpose:
*     Merge the bad-pixel flags of the array components of a pair of NDFs.

*  Synopsis:
*     void ndfMbad( int badok, int indf1, int indf2, const char *comp,
*                   int check, int *bad, int *status )

*  Description:
*     This function merges the bad-pixel flag values of an array component
*     (or components) for a pair of NDFs, returning the logical "OR" of the
*     separate values for each NDF. In addition, if bad pixels are found to
*     be present in either NDF but the application indicates that it cannot
*     correctly handle such values, then an error to this effect is
*     reported and a "status" value is set.

*  Parameters:
*     badok
*        Whether the application can correctly handle NDF array components
*        containing bad pixel values.
*     indf1
*        Identifier for the first NDF whose bad-pixel flag value is to be
*        merged.
*     indf2
*        Identifier for the second NDF.
*     comp
*        Pointer to a null terminated string holding the name of the NDF
*        array component: "DATA", "QUALITY" or "VARIANCE".
*     check
*        Whether to perform explicit checks to see whether bad pixels are
*        actually present. (This parameter performs the same function as in
*        the function ndfBad.)
*     *bad
*        Returned holding the combined bad-pixel flag value (the logical
*        "OR" of the values obtained for each NDF).
*     *status
*        The global status.

*  Notes:
*     -  A comma-separated list of component names may also be supplied, in
*     which case the function will take the logical "OR" of all the
*     specified components when calculating the combined bad-pixel flag
*     value.
*     -  The effective value of the bad-pixel flag for each NDF array
*     component which this function uses is the same as would be returned
*     by a call to the function ndfBad.
*     -  If this function detects the presence of bad pixels which the
*     application cannot support (as indicated by a zero value for the
*     "badok" parameter), then an error will be reported to this effect and
*     a "status" value of NDF__BADNS (bad pixels not supported) will be
*     returned.  The value of the "bad" parameter will be set to non-zero
*     under these circumstances. The NDF__BADNS constant is defined in the
*     header file "ndf_err.h".

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
   int n;                /* Number of different identifiers */
   int ndfs[ 2 ];        /* Array for NDF identifiers */

/* Check inherited global status. */
   if( *status != SAI__OK ) return;

/* Ensure the NDF library has been initialised. */
   NDF_INIT( status );

/* Assign the first NDF identifier to the first element of an array. */
   ndfs[ 0 ] = indf1;

/* If the second identifier is different, then assign it to the second
   array element. */
   if( indf2 != indf1 ) {
      ndfs[ 1 ] = indf2;
      n = 2;

/* Otherwise, use only one identifier. */
   } else {
      n = 1;
   }

/* Merge the array component bad pixel flags. */
   ndf1Mbad( badok, n, ndfs, comp, check, bad, status );

/* If an error occurred, then report context information and call the
   error tracing function. */
   if( *status != SAI__OK ) {
      errRep( " ", "ndfMbad: Error merging the bad-pixel flags of the "
              "array components of a pair of NDFs.", status );
      ndf1Trace( "ndfMbad", status );
   }

/* Restablish the original AST status pointer */
   NDF_FINAL

}

