#include "mers.h"
#include "ndf1.h"
#include "ndf_ast.h"
#include "ndf_err.h"
#include "sae_par.h"
#include <stdint.h>
#include <string.h>

void ndf1Cmpfl( const char *name1, size_t start1, size_t end1,
                const char *name2, int *same, int *status ){
/*
*+
*  Name:
*     ndf1Cmpfl

*  Purpose:
*     Compare two file names for equality.

*  Synopsis:
*     void ndf1Cmpfl( const char *name1, size_t start1, size_t end1,
*                     const char *name2, int *same, int *status )

*  Description:
*     This function compares two file names (or file name fields) for
*     equality, taking account of whether the host file system uses case
*     sensitive file names.

*  Parameters:
*     name1
*        Pointer to a null terminated string holding the first file name
*        (or file name field) to be compared.
*     start1
*        The zero-based index of the first character to consider in "name1".
*        The whole string is used if "start1" > "end1".
*     end1
*        The zero-based index of the last character to consider in "name1".
*        The whole string is used if "start1" > "end1".
*     name2
*        Pointer to a null terminated string holding the second file name
*        (or file name field) to be compared.
*     *same
*        Returned holding the whether the two file names are the same.
*     *status
*        The global status.

*  Notes:
*     This function does not perform any file name expansion and does not
*     compare the files themselves. For instance, it is possible for two
*     file names to be judged unequal by this function, but for them to
*     refer to the same file.

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
*        Support for VMS has been removed.
*-
*/

/* Local Variables; */
   char *tname1;

/* Check inherited global status. */
   if( *status != SAI__OK ) return;

/* Get a null terminated copy of the section of the "name1" string to be
   checked. */
   tname1 = ndf1Substr( name1, start1, SIZE_MAX, status );

/* If OK, then compare the two file names for equality. POSIX file names
   are case sensitive. */
   if( *status == SAI__OK ) *same = !strcmp( tname1, name2 );

/* Free resoureces */
   tname1 = astFree( tname1 );

/* Call error tracing function and exit. */
   if( *status != SAI__OK ) ndf1Trace( "ndf1Cmpfl", status );

}

