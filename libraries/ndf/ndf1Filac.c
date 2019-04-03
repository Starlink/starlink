#define _POSIX_SOURCE 1      /* Declare POSIX source */

#include <stdlib.h>          /* Define malloc, free, etc. */
#include <errno.h>           /* Define errno */
#include <ctype.h>           /* Character functions */
#include <string.h>          /* String functions */
#include <unistd.h>          /* Define access function */

#include "sae_par.h"         /* Standard SAE constants */
#include "ems.h"             /* ems_ error reporting routines */
#include "ndf1.h"            /* Internal NDF definitions */
#include "ndf_err.h"         /* NDF error definitions */

void ndf1Filac( const char *fname, const char *mode, int report, int *ok,
                int *status ){
/*
*+
*  Name:
*     ndf1Filac

*  Purpose:
*     Check if a specified mode of access is available for a file.

*  Synopsis:
*     void ndf1Filac( const char *fname, const char *mode, int report, int *ok,
*                     int *status )

*  Description:
*     This function checks whether a specified mode of access is available
*     for a named file and returns a logical result. If access is not
*     available, the routine will also (optionally) set "status" and make
*     an appropriate error report.

*  Arguments:
*     fname
*        Pointer to a null terminated string holding the name of the file.
*     mode
*        Pointer to a null terminated string holding the required mode of
*        access: 'READ', 'UPDATE' or 'WRITE' (case insensitive). The
*        value 'EXISTS' may also be supplied, in which case a check is
*        made that the file exists but no check is made on the access
*        available to the file.
*     report
*        Whether to set "status" and make an error report if the requested
*        mode of access is not available.
*     *ok
*        Returned holding a flag indicating if the requested mode of access
*        was available.
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
*     xxx (DSB):
*        Original version, based on equivalent function by RFWS.
*        VMS support removed.

*-
*/

/* Local Variables: */
   int flags = 0;       /* Access mode flags */
   size_t nc;           /* Used length of string */

/* Check inherited global status. */
   if( *status != SAI__OK ) return;

/* Get the length of the mode string excluding trailing blanks. */
   nc = astChrLen( mode );

/* Compare the requested access mode with each recognised value in turn and
   set the access mode flags accordingly. */
   if( astChrMatchN( mode, "READ", nc ) ) {
      flags = R_OK;
   } else if( astChrMatchN( mode, "WRITE", nc ) ) {
      flags = W_OK;
   } else if( astChrMatchN( mode, "UPDATE", nc ) ) {
      flags = R_OK | W_OK;
   } else if( astChrMatchN( mode, "EXISTS", nc ) ) {
      flags = F_OK;

/* Report an error if the access mode is not recognised. */
   } else {
      *status = NDF__FATIN;
      emsRepf( " ", "Routine ndf1Filac called with an invalid file access mode "
               "of '%s' (internal programming error).", status, mode );
   }

/* Enquire if the requested file access is available and set "*ok" accordingly. */
   if( *status == SAI__OK ) {
      *ok = !access( fname, flags );

/* If access is not available, set *status and report an error if required. */
      if( ! *ok && report ) {
         if( flags == F_OK ) {
            *status = NDF__FILNF;
            emsRepf( " ", "Unable to open the file '%s'; file does not "
                 "exist.", status, fname );
         } else {
            *status = NDF__FILPR;
            emsErrno( "MESSAGE", errno );
            emsRepf( " ", "Unable to open the file '%s' for %s access "
                     "- ^MESSAGE", status, fname, mode );
         }
      }
   }

/* If necessary, call the error tracing function. */
   if ( *status != SAI__OK ) ndf1Trace( "ndf1Filac", status );
}
