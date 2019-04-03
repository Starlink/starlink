#include <stdlib.h>
#include "sae_par.h"
#include "dat_par.h"
#include "ndf1.h"
#include "ndf.h"
#include "mers.h"

void ndfMtype_( const char *typlst, int indf1, int indf2, const char *comp,
               char *itype, size_t itype_length, char *dtype,
               size_t dtype_length, int *status ){
/*
*+
*  Name:
*     ndfMtype

*  Purpose:
*     Match the types of the array components of a pair of NDFs.

*  Synopsis:
*     void ndfMtype( const char *typlst, int indf1, int indf2,
*                    const char *comp, char *itype, size_t itype_length,
*                    char *dtype, size_t dtype_length, int *status )

*  Description:
*     This function matches the types of the array components of a pair of
*     NDFs, selecting a numeric type which an application may use to
*     process these components. It also returns the type which should be
*     used for storing the result of this processing.

*  Parameters:
*     typlst
*        Pointer to a null terminated string holding A comma-separated list
*        of the numeric types which the application can process explicitly;
*        e.g. "_INTEGER,_REAL". The first type which has sufficient
*        precision will be selected from this list, so they should normally
*        be given in order of increasing computational cost.
*     indf1
*        Identifier for the first NDF whose type is to be matched.
*     indf2
*        Identifier for the second NDF.
*     comp
*        Pointer to a null terminated string holding the name of the NDF
*        array component whose type is to be considered.
*     itype
*        Pointer to an array in which to return a null terminated string
*        holding the numeric type which the application should use to
*        process the NDF components. This value is returned as an upper
*        case character string of maximum length NDF__SZTYP. Its value is
*        the first entry in the "typlst" list to which the NDF array
*        components may be converted without unnecessary loss of
*        information.
*     itype_length
*        The length of the supplied 'itype' array. This should include
*        room for the terminating null.
*     dtype
*        Pointer to an array in which to return a null terminated string
*        holding the data type required to hold the result of processing
*        the NDF array components. This result is returned as an upper case
*        character string of maximum length NDF__SZFTP. It is intended to
*        be used as input to the ndfStype function to set the type of the
*        output NDF component into which the result will be written.
*     dtype_length
*        The length of the supplied 'dtype' array. This should include
*        room for the terminating null.
*     *status
*        The global status.

*  Notes:
*     -  A comma-separated list of component names may also be supplied, in
*     which case the results returned by this function will take account of
*     the types of all the specified components in both NDFs.
*     -  Matching of the type of a single NDF to an application may be
*     performed by supplying the same identifier value for both the "indf1"
*     and "indf2" arguments. There is no extra cost in doing this.
*     -  If the "typlst" parameter does not specify any type to which the
*     NDF components may be converted without loss of information, then the
*     function will return the highest precision type which is available.
*     An error will be reported, however, and "status" will be set to
*     NDF__TYPNI (type not implemented).
*     -  The constants NDF__SZTYP and NDF__SZFTP are defined in the include
*     file "ndf.h". The error code NDF__TYPNI is defined in the include
*     file "ndf_err.h".

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
   int n;                /* Number of different NDFs to match */
   int ndfs[ 2 ];        /* Array of NDF identifiers */

/* Check inherited global status. */
   if( *status != SAI__OK ) return;

/* Ensure the NDF library has been initialised. */
   NDF_INIT( status );

/* Store the first identifier in the "ndfs" array. */
   ndfs[ 0 ] = indf1;

/* Also store the second, if different. */
   if( indf2 != indf1 ) {
      ndfs[ 1 ] = indf2;
      n = 2;

/* Otherwise, just process one. */
   } else {
      n = 1;
   }

/* Match the types of the array component(s) to the application. */
   ndf1Mtyp( typlst, n, ndfs, comp, itype, itype_length, dtype,
             dtype_length, status );

/* If an error occurred, then report context information and call the
   error tracing function. */
   if( *status != SAI__OK ) {
      errRep( " ", "ndfMtype: Error matching the types of the array "
              "components of a pair of NDFs.", status );
      ndf1Trace( "ndfMtype", status );
   }

/* Restablish the original AST status pointer */
   NDF_FINAL

}

