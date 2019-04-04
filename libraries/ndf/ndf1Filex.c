#include "sae_par.h"
#include "ndf_err.h"
#include "ndf1.h"
#include "mers.h"
#include <string.h>

void ndf1Filex( const char *file, const char *mode, int report, int *ok,
                int *status ){
/*
*+
*  Name:
*     ndf1Filex

*  Purpose:
*     Determine if a file exists and is accessible.

*  Synopsis:
*     void ndf1Filex( const char *file, const char *mode, int report,
*                     int *ok, int *status )

*  Description:
*     This function determines whether a file exists and (optionally)
*     whether it is accessible using a specified access mode, and returns a
*     logical result. If the file does not exist or is not accessible, the
*     function will optionally set "status" and report an appropriate
*     error.

*  Parameters:
*     file
*        Pointer to a null terminated string holding the name of the file.
*     mode
*        Pointer to a null terminated string holding the required mode of
*        access: "READ", "UPDATE" or "WRITE" (case insensitive). If an
*        existence test only is required, then this parameter should be
*        blank.
*     report
*        Whether to set "status" and report an error if the file does not
*        exist or is not accessible (non-zero ==> report an error, zero ==>
*        return without further action).
*     *ok
*        Returned holding the whether the file exists and is accessible.
*     *status
*        The global status.

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

/* Check inherited global status. */
   if( *status != SAI__OK ) return;

/* Enquire whether the file exists, checking for errors. */
   ndf1Filac( file, "EXISTS", report, ok, status );

/* If it exists, and "mode" is blank, then there is nothing more to
   do. Otherwise, determine whether the required mode of access is
   available. */
   if( *ok && astChrLen( mode ) > 0 ) ndf1Filac( file, mode, report,
                                                 ok, status );

/* Call error tracing function and exit. */
   if( *status != SAI__OK ) ndf1Trace( "ndf1Filex", status );

}

