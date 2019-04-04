#include <stdlib.h>
#include "sae_par.h"
#include "dat_par.h"
#include "ndf1.h"
#include "ndf.h"
#include "star/util.h"
#include "mers.h"

void ndfAtype_( int indf, const char *comp, int iaxis, char *type,
               size_t type_length, int *status ){
/*
*+
*  Name:
*     ndfAtype

*  Purpose:
*     Obtain the numeric type of an NDF axis array.

*  Synopsis:
*     void ndfAtype( int indf, const char *comp, int iaxis, char *type,
*                    size_t type_length, int *status )

*  Description:
*     This function returns the numeric type of an NDF axis array as an
*     upper-case character string (e.g. "_REAL").

*  Parameters:
*     indf
*        NDF identifier.
*     comp
*        Pointer to a null terminated string holding the name of the axis
*        array component whose numeric type is required: "CENTRE",
*        "VARIANCE" or "WIDTH".
*     iaxis
*        Number of the NDF axis for which information is required.
*     type
*        Pointer to an array in which to return a null terminated string
*        holding the numeric type of the axis array.
*     type_length
*        The length of the supplied 'type' array. This should include
*        room for the terminating null.
*     *status
*        The global status.

*  Notes:
*     -  A comma-separated list of axis array component names may also be
*     supplied to this function. In this case the result returned will be
*     the lowest precision numeric type to which all the specified axis
*     arrays can be converted without unnecessary loss of information.
*     -  A value of zero may be supplied for the "iaxis" parameter, in
*     which case the function will combine the results for all the NDF"s
*     axes in the same way as described above.
*     -  The symbolic constant NDF__SZTYP may be used for declaring the
*     length of a character variable which is to hold the numeric type of
*     an NDF axis array. This constant is defined in the include file
*     "ndf.h".

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

/* Local variables: */
   const char *datyp[ NDF__MXTYP ];  /* Data */
   NdfACB *acb;          /* Pointer to NDF entry in the ACB */
   int itype;            /* Integer type code of result */

/* Local Data: */
   datyp[ NDF__TYPB ] = "_BYTE";
   datyp[ NDF__TYPD ] = "_DOUBLE";
   datyp[ NDF__TYPI ] = "_INTEGER";
   datyp[ NDF__TYPK ] = "_INT64";
   datyp[ NDF__TYPR ] = "_REAL";
   datyp[ NDF__TYPUB ] = "_UBYTE";
   datyp[ NDF__TYPUW ] = "_UWORD";
   datyp[ NDF__TYPW ] = "_WORD";

/* Check inherited global status. */
   if( *status != SAI__OK ) return;

/* Ensure the NDF library has been initialised. */
   NDF_INIT( status );

/* Import the NDF identifier. */
   ndf1Impid( indf, &acb, status );

/* Determine the integer type code of the result. */
   ndf1Atyp( iaxis, acb, comp, &itype, status );

/* Translate the type code into a string and return it. */
   if( *status == SAI__OK ) ndf1Ccpy( datyp[ itype ], type,
                                      type_length, status );

/* If an error occurred, then report context information and call the
   error tracing function. */
   if( *status != SAI__OK ) {
      errRep( " ", "ndfAtype: Error obtaining the numeric type of an NDF "
              "axis array.", status );
      ndf1Trace( "ndfAtype", status );
   }

/* Restablish the original AST status pointer */
   NDF_FINAL

}

