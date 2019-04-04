#include "sae_par.h"
#include "dat_par.h"
#include "ndf1.h"
#include "mers.h"
#include "ems.h"
#include <string.h>

void ndf1Cvfor( const char *forfil, NdfFCB *fcb, HDSLoc *ndfloc,
                const char *ndfnam, int from, int *status ){
/*
*+
*  Name:
*     ndf1Cvfor

*  Purpose:
*     Perform data format conversion on a foreign file.

*  Synopsis:
*     void ndf1Cvfor( const char *forfil, NdfFCB *fcb, HDSLoc *ndfloc,
*                     const char *ndfnam, int from, int *status )

*  Description:
*     This function obtains the conversion command which converts between a
*     foreign format data file and a native format NDF object (or vice
*     versa) by translating the appropriate environment variable. It then
*     substitutes the necessary file name (and other) fields into this
*     command and has it executed so as to perform the conversion.

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
*     *status
*        The global status.

*  Notes:
*     This function does not make any checks on the existence or
*     accessibility of the input file before conversion (it should exist
*     and be readable), nor on the existence of the output file (typically,
*     it should not exist prior to conversion and should exist afterwards).

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

/* Local Variables: */
   HDSLoc *toploc = NULL;/* Top level locator */
   char cmd[ NDF__SZCVT + 1 ];     /* Buffer for raw command text */
   char cvt[ NDF__SZCVT + 1 ];     /* Translated command text */
   int def;              /* Environment variable defined? */
   int nc;               /* Length of string */

/* Check inherited global status. */
   if( *status != SAI__OK ) return;

/* Attempt to translate the appropriate environment variable to obtain
   the conversion command. */
   ndf1Cvcmd( forfil, fcb, ndfloc, ndfnam, from, 1, &def, cmd,
              sizeof( cmd ), status );

/* If a valid conversion command was obtained, and we are converting to
   a foreign file format, then export any NDF extension information
   before performing the conversion. */
   if( *status == SAI__OK ) {
      if( strlen( cmd ) ) {
         if( !from ) ndf1Xtfor( forfil, fcb, ndfloc, ndfnam, 0, status );

/* If OK, mark the error stack to prevent any interference with
   previously defined message tokens and define standard message tokens
   for the conversion operation. */
         if( *status == SAI__OK ) {
            errMark();
            ndf1Cvtok( forfil, fcb, ndfloc, ndfnam, status );

/* Substitute these token values into the blank command, returning the
   resulting conversion command and its length. Use a low-level (EMS)
   function to ensure the message text supplied is used without change. */
            emsExpnd( cmd, cvt, sizeof( cvt ), 0, &nc, status );

/* The TCB values are held in global variables, so use a mutex to ensure
   the current thread has sole access. */
            NDF__TCB_LOCK_MUTEX;

/* If required, report details of the conversion being performed. */
            if( *status == SAI__OK ) {

               if( Ndf_TCB_shcvt ) {

/* Converting from a foreign format. */
                  if( from ) {
                     msgRenew();
                     msgOut( " ", "-->Converting: ^FMT file "
                             "^DIR^NAME^TYPE^VERS^FXS", status );
                     msgRenew();
                     msgOut( " ", "      to give: NDF object ^NDF", status );

/* Converting to a foreign format. */
                  } else {
                     msgRenew();
                     msgOut( " ", "-->Converting: NDF object ^NDF", status );
                     msgRenew();
                     msgOut( " ", "      to give: ^FMT file "
                             "^DIR^NAME^TYPE^VERS^FXS", status );
                  }

/* Display the command being used. */
                  msgSetc( "CVT", cvt );
                  msgOut( " ", "      command: ^CVT", status );
               }
            }

/* Release the mutex. */
            NDF__TCB_UNLOCK_MUTEX;

/* Release the error stack. */
            errRlse();

/* If the NDF container file is already open (we have a locator to it),
   then we must flush modifications and release all locks on it so that
   the data conversion process can access it. Obtain a top-level HDS
   locator in order to do this. */
            if( *status == SAI__OK ) {
               if( ndfloc ) {
                  ndf1Htop( ndfloc, "UPDATE", &toploc, status );

/* After freeing the file, do not perform any more operations on it
   until data conversion has completed, since this may cause the file
   to be locked again. */
                  hdsFree( toploc, status );
               }

/* Execute the conversion command. */
               ndf1Docmd( cvt, status );

/* Annul the top level locator, if obtained. */
               if( ndfloc ) datAnnul( &toploc, status );

/* If converting from a foreign file format, then import any NDF
   extension information after performing the conversion. */
               if( from ) ndf1Xtfor( forfil, fcb, ndfloc, ndfnam, 1, status );
            }
         }
      }
   }

/* Call error tracing function and exit. */
   if( *status != SAI__OK ) ndf1Trace( "ndf1Cvfor", status );

}

