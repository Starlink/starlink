#include "dat_par.h"
#include "mers.h"
#include "ems.h"
#include "ndf1.h"
#include "ndf_err.h"
#include "sae_par.h"
#include <errno.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

void ndf1Dlfor( const char *file, NdfFCB *fcb, int *status ){
/*
*+
*  Name:
*     ndf1Dlfor

*  Purpose:
*     Delete a foreign format file.

*  Synopsis:
*     void ndf1Dlfor( const char *file, NdfFCB *fcb, int *status )

*  Description:
*     This function deletes a foreign format file associated with an NDF.
*     If required, it first attempts to translate the environment variable
*     NDF_DEL_xxx (where xxx is the foreign format name) to see if this
*     contains an external deletion command. If so, it substitutes the
*     appropriate fields of the file name into this command and then has it
*     executed so as to delete the file. Otherwise, if no external deletion
*     command is defined (or if one is not requested), then the file is
*     deleted directly.

*  Parameters:
*     file
*        Pointer to a null terminated string holding the name of the
*        foreign format file.
*     fcb
*        Pointer to an object describing the foreign file format. If this
*        is non-NULL, then an external deletion command corresponding to
*        this format will be used (if available). If it is set to NULL,
*        then the file will be deleted directly (and its deletion will not
*        be reported as a foreign format conversion operation, even if the
*        TCB parameter SHCVT is set).
*     *status
*        The global status.

*  Notes:
*     -  This function will attempt to execute even if it is called with
*     "status" set, although no further error report will be made if it
*     subsequently fails under these circumstances.

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
*     MERCHANTABILITY or FITNESS "for" A PARTICULAR PURPOSE. See the GNU
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
   char cmd[ NDF__SZCVT + 1 ];     /* Buffer for blank command text */
   char del[ NDF__SZCVT + 1 ];     /* Deletion command */
   int def;              /* Environment variable defined? */
   int ldel;             /* Length of deletion command */
   int nc;               /* Length of dynamic string */
   int rstatus;          /* Status returned by "remove" cuntion */
   int there;            /* File exists? */
   size_t lcmd;          /* Length of blank command text */
   size_t x1;            /* First character of for. extension specifier */
   size_t x2;            /* Last character of for. extension specifier */

/* Begin a new error reporting environment. */
   errBegin( status );

/* Ensure there is no foreign extension specifier in the supplied file
   name. */
   ndf1Forxt( file, 1, 0, &x1, &x2, status );
   if( x1 <= x2 ) {
      *status = NDF__FATIN;
      msgSetc( "ROUTINE", "ndf1Dlfor" );
      msgSetc( "FILE", file );
      errRep( " ", "Function ^ROUTINE called with an invalid FILE "
              "parameter of ^FILE; this value should not include any "
              "foreign extension specifier (internal programming error).",
              status );
   }
   if( *status == SAI__OK ) {

/* Inquire whether the file exists. There is nothing to do if it
   does not exist. */
      ndf1Filex( file, " ", 0, &there, status );
      if( *status == SAI__OK ) {

/* If the file exists, and a foreign file format has been supplied, then
   obtain the character limits of the file format name in the FCB format
   list string. */
         lcmd = 0;
         if( there ) {
            if( fcb ) {

/* Attempt to translate the environment variable which (optionally)
   holds a command for deleting files with the specified format. */
               nc = 0;
               varname = astAppendStringf( NULL, &nc, "NDF_DEL_%s", fcb->name );
               ndf1Gtenv( varname, &def, cmd, sizeof( cmd ), &lcmd, status );
               varname = astFree( varname );
            }

/* External deletion command.
   =========================
   If a non-blank deletion command was obtained above, we must now use
   it to delete the file. Define standard message tokens for the
   deletion operation. */
            if( *status == SAI__OK ) {
               if( lcmd > 0 ) {
                  ndf1Cvtok( file, fcb, NULL, " ", status );

/* Substitute these token values into the blank command, returning the
   resulting deletion command and its length. Use a low-level (EMS)
   function to ensure the message text supplied is used without change. */
                  emsMload( " ", cmd, del, &ldel, status );
                  ldel = NDF_MAX( 1, ldel );

/* If required, display what's happening and the command to be executed. */
                  if( *status == SAI__OK ) {
                     if( Ndf_TCB_shcvt ) {
                        msgRenew();
                        msgOut( " ", "-->  Deleting: ^FMT file "
                                "^DIR^NAME^TYPE^VERS", status );
                        msgSetc( "DEL", del );
                        msgOut( " ", "      command: ^DEL", status );
                     }
                  }

/* Execute the command to delete the file. */
                  if( *status == SAI__OK ) ndf1Docmd( del, status );

/* No external deletion command.
   ============================
   If no deletion command has been specified (or none is required), then
   we must delete the file directly. This will involve opening the file. */
               } else {

/* If required, display what is happening. */
                  if( Ndf_TCB_shcvt && fcb ) {
                     msgSetc( "FILE", file );
                     msgSetc( "FMT", fcb->name );
                     msgOut( " ", "-->  Deleting: ^FMT file ^FILE", status );
                  }

/* Remove the file. */
                  rstatus = remove( file );

/* Check status value */
                  if( rstatus == -1 ) {
                     *status = NDF__DELER;
                     emsSyser( "ERRNO", errno);
                     emsSetc( "PATH", file );
                     emsRep( " ", "Error removing the file '^PATH': ^ERRNO",
                             status);
                  }
               }
            }
         }
      }
   }

/* Call error tracing function if appropriate. */
   if( *status != SAI__OK ) ndf1Trace( "ndf1Dlfor", status );

/* End the error reporting environment. */
   errEnd( status );

}

