#define _POSIX_SOURCE 1		 /* Declare POSIX source */

/* Header files. */
/* ============= */
/* C run-time library header files. */
#include <stdlib.h>              /* Utility functions */

/* External interfaces. */
#include "ems.h"                 /* EMS Error message service */
#include "sae_par.h"		 /* Standard SAE constants */

/* Internal header files. */
#include "ndf1.h"                /* NDF private interface */

/* Function definitions. */
/* ===================== */
void ndfInit( int argc, char *const argv[], int *status ) {
/*
*+
*  Name:
*     ndfInit

*  Purpose:
*     Initialise the NDF_ library for use from a C main routine.

*  Language:
*     ANSI C

*  Synopsis:
*     void ndfInit( int argc, char *const argv[], int *status );

*  Description:
*     This function must be called to perform initialisation if the
*     NDF_ library is to be used in a program whose main routine is
*     written in C. It should be called before any other use of the
*     NDF_ library takes place.

*  Parameters:
*     argc
*        The number of command line arguments.
*     argv
*        Pointer to an array of pointers to null-terminated strings
*        (one for each argument) containing the command line argument
*        strings.
*     status
*        Pointer to the global status.

*  Copyright:
*     Copyright (C) 1998, 2004 Central Laboratory of the Research Councils.
*     Copyright (C) 2005 Particle Physics & Astronomy Research Council.
*     All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either version 2 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful,but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 51 Franklin Street,Fifth Floor, Boston, MA
*     02110-1301, USA

*  Authors:
*     RFWS: R. F. Warren-Smith (RAL, Starlink)
*     PWD: Peter Draper (Durham, Starlink)
*     TIMJ: Tim Jenness (JAC, Hawaii)

*  History:
*     30-SEP-1998 (RFWS):
*         Original version.
*     14-MAR-2004 (TIMJ):
*         Import and autoconf.
*     26-JAN-2005 (PWD):
*         Portability fixes.
*     17-AUG-2009 (TIMJ):
*         Use ndf1_setargvc instead of a global.

*  Notes:
*     - The actual arguments supplied for "argv" and "argc" should
*     normally be the standard arguments supplied to the C "main"
*     function by the operating system.
*     - If "argv" is created dynamically by the caller (for example
*     building up the arguments from a dynamic language such as perl)
*     the memory associated with "argv" should not be freed until NDF
*     has been shut down. NDF does not copy the arguments. If you do free
*     the arguments, expect a core dump when history writing is enabled.
*     - Do not pass in a stack variable that can be reused when the
*     calling routine ends unless the calling routine finishes with
*     NDF before it returns.
*     - If these arguments are not available, then an "argc" value of
*     zero should be given (whereupon the "argv" value will be
*     ignored).
*     - Additional initialisations of the Fortran Runtime may be
*     required for this to work correctly. The STAR_INITIALISE_FORTRAN_RTL
*     macro of the Starlink Build System or the cnfInitRTL function
*     will do this work, otherwise you need to arrange this for yourself.
*-
*/

/* Local Variables: */
   int iarg;                     /* Loop counter for arguments */

/*. */

/* Check inherited global status. */
   if ( *status != SAI__OK ) return;

/* Check the argument count for validity and report an error if
   necessary. */
   if ( argc < 0 ) {
      *status = NDF__ARCIN;
      emsSeti( "ARGC", argc );
      emsRep( "ndfInit_argc", "Invalid argument count (^ARGC); this value "
                 "should not be less than zero.", status );

/* Check the argument vector pointer similarly to ensure it is not
   NULL. */
   } else if ( ( argc > 0 ) && !argv ) {
      *status = NDF__ARGIN;
      emsRep( "ndfInit_arg1", "Invalid NULL pointer given for argument "
                 "list.", status );

/* Otherwise, check that each individual argument string pointer is
   valid (not NULL) and report an error if necessary. */
   } else {
      for ( iarg = 0; iarg < argc; iarg++ ) {
         if ( !argv[ iarg ] ) {
            *status = NDF__ARGIN;
            emsSeti( "ARG", iarg );
            emsRep( "ndfInit_arg2", "Invalid NULL string pointer given for "
                       "argument number ^ARG.", status );
            break;
         }
      }
   }

/* If OK, store the argument count and argument vector pointer in
   global variables for use by ndf1_gtarg. */
   if ( *status == SAI__OK ) {
      ndf1_setargvc( argc, argv, status );

/* Otherwise, report context information and call the error tracing
   function. */
   } else {
      emsRep( "ndfInit_err", "ndfInit: Error initialising the NDF_ library "
                 "for use from a C main routine.", status );
      ndf1Trace( "ndfInit", status );
   }
}
