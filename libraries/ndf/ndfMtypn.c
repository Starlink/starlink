#include <stdlib.h>
#include "sae_par.h"
#include "dat_par.h"
#include "ndf1.h"
#include "ndf.h"
#include "mers.h"

void ndfMtypn_( const char *typlst, int n, const int ndfs[],
               const char *comp, char *itype, size_t itype_length,
               char *dtype, size_t dtype_length, int *status ){
/*
*+
*  Name:
*     ndfMtypn

*  Purpose:
*     Match the types of the array components of a number of NDFs.

*  Synopsis:
*     void ndfMtypn( const char *typlst, int n, const int ndfs[],
*                    const char *comp, char *itype, size_t itype_length,
*                    char *dtype, size_t dtype_length, int *status )

*  Description:
*     This function matches the types of the array components of a number
*     of NDFs, selecting a type which an application may use to process
*     these components. It also returns the numeric type which should be
*     used for storing the result of this processing.

*  Parameters:
*     typlst
*        Pointer to a null terminated string holding A comma-separated list
*        of the numeric types which the application can process explicitly;
*        e.g. "_INTEGER,_REAL". The first type which has sufficient
*        precision will be selected from this list, so they should normally
*        be given in order of increasing computational cost.
*     n
*        Number of NDFs whose types are to be matched.
*     ndfs
*        Array of identifiers for the NDFs to be matched. The supplied
*        "ndfs" array should have at least "n" elements.
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
*     the types of all the specified components in all the NDFs.
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

/* Check inherited global status. */
   if( *status != SAI__OK ) return;

/* Ensure the NDF library has been initialised. */
   NDF_INIT( status );

/* Match the types of the array component(s) to the application. */
   ndf1Mtyp( typlst, n, ndfs, comp, itype, itype_length, dtype,
             dtype_length, status );

/* If an error occurred, then report context information and call the
   error tracing function. */
   if( *status != SAI__OK ) {
      errRep( " ", "ndfMtypn: Error matching the types of the array "
              "components of a number of NDFs.", status );
      ndf1Trace( "ndfMtypn", status );
   }

/* Restablish the original AST status pointer */
   NDF_FINAL

}

