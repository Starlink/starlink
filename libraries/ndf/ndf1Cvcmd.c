#include <stdlib.h>
#include "sae_par.h"
#include "dat_par.h"
#include "ndf1.h"
#include "ndf_err.h"
#include "mers.h"
#include <string.h>

void ndf1Cvcmd( const char *forfil, NdfFCB *fcb, HDSLoc *ndfloc,
                const char *ndfnam, int from, int report, int *def,
                char *cmd, size_t cmd_length, int *status ){
/*
*+
*  Name:
*     ndf1Cvcmd

*  Purpose:
*     Get a data format conversion command for a foreign file.

*  Synopsis:
*     void ndf1Cvcmd( const char *forfil, NdfFCB *fcb, HDSLoc *ndfloc,
*                     const char *ndfnam, int from, int report, int *def,
*                     char *cmd, size_t cmd_length, int *status )

*  Description:
*     This function returns the conversion command which converts between a
*     foreign format data file and a native format NDF object (or vice
*     versa) by translating the appropriate environment variable.

*  Parameters:
*     forfil
*        Pointer to a null terminated string holding the name of the
*        foreign format file, optionally containing a foreign extension
*        specifier.
*     fcb
*        Pointer to an object describing the format of the foreign file
*        (must be non-NULL).
*     ndfloc
*        Locator which, in conjunction with the "ndfnam" parameter,
*        identifies the native format NDF object. If a value of NULL
*        is given, then "ndfnam" should contain the absolute name of this
*        object.
*     ndfnam
*        Pointer to a null terminated string holding the relative HDS name
*        of the native format NDF object (or the absolute name if "ndfloc"
*        is set to NULL).
*     from
*        If a non-zero value is given, conversion is from the foreign
*        format to native NDF format. Otherwise the reverse conversion is
*        performed.
*     report
*        If a non-zero value is given, then an error is reported if the
*        conversion command is not defined.
*     *def
*        Returned holding a non-zero value if the requested conversion
*        command is defined.
*     cmd
*        Pointer to an array in which to return a null terminated string
*        holding the conversion command. The supplied character variable
*        should be at least NDF__SZCVT characters long.
*     cmd_length
*        The length of the supplied 'cmd' array. This should include
*        room for the terminating null.
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
*        Original version, based on equivalent Fortran function by RFWS.

*-
*/

/* Local Variables: */
   char *varname;        /* String holding environment variable name */
   int nc;               /* Length of dynamic string */
   size_t lcmd;          /* Length of command text */

/* Check inherited global status. */
   if( *status != SAI__OK ) return;

/* Attempt to translate the appropriate environment variable to obtain
   the conversion command. */
   varname = astAppendStringf( NULL, &nc, from ? "NDF_FROM_%s" : "NDF_TO_%s",
                               fcb->name );
   ndf1Gtenv( varname, def, cmd, cmd_length, &lcmd, status );
   varname = astFree( varname );

/* If no command was defined (or it was blank), then we must report an
   error (but only if requested). */
   if( *status == SAI__OK && report ) {
      if( lcmd == 0 ) {

/* Set "status" and define the required message tokens. */
         *status = NDF__NOCVT;
         msgSetc( "FILE", forfil );
         msgSetc( "FMT", fcb->name );
         if( !ndfloc ) {
            msgSetc( "NDF", ndfnam );
         } else {
            datMsg( "NDF", ndfloc );
            if( astChrLen( ndfnam ) > 0 ) {
               msgSetc( "NDF", "." );
               msgSetc( "NDF", ndfnam );
            }
         }

/* Adapt the error message to the direction of format conversion. */
         if( from ) {
            errRep( " ", "Unable to convert the ^FMT format file '^FILE' "
                    "to NDF format in the object ^NDF.", status );
            msgRenew();
            errRep( " ", "The NDF_FROM_^FMT environment variable does not "
                    "contain a suitable conversion command.", status );
         } else {
            errRep( " ", "Unable to convert the NDF format object ^NDF to "
                    "^FMT format in the file '^FILE'.", status );
            msgRenew();
            errRep( " ", "The NDF_TO_^FMT environment variable does not "
                    "contain a suitable conversion command.", status );
         }

      }
   }

/* Call error tracing function and exit. */
   if( *status != SAI__OK ) ndf1Trace( "ndf1Cvcmd", status );

}

