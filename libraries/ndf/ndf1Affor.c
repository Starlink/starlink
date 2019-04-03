#include <stdlib.h>
#include "sae_par.h"
#include "ndf1.h"
#include "mers.h"
#include "ems.h"

void ndf1Affor( NdfFCB *fcb, int *status ){
/*
*+
*  Name:
*     ndf1Affor

*  Purpose:
*     Perform post-processing on a dataset.

*  Synopsis:
*     void ndf1Affor( NdfFCB *fcb, int *status )

*  Description:
*     This function obtains a post-processing command to apply to a dataset
*     after it has been released, by translating the appropriate
*     environment variable. It then substitutes the necessary message token
*     values into this command and has it executed so as to perform the
*     required post-processing.

*  Parameters:
*     fcb
*        Pointer to an object describing the format of the dataset (may be
*        NULL to indicate a native format NDF dataset).
*     *status
*        The global status.

*  Notes:
*     -  This function does not have access to data in the DCB describing
*     the dataset being processed. This is because the dataset will already
*     have been released by the time this function is executed. Instead,
*     the name of the dataset (and any related items) should be supplied
*     via message tokens which have been defined earlier.
*     -  All message tokens in the current context will be left in an
*     undefined state after this function exits.
*     -  This function does not make any checks on the existence or
*     accessibility of any file or dataset before attempting to process it.

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
   char cmd[ NDF__SZCVT + 1 ];     /* Buffer for raw command text */
   char post[ NDF__SZCVT + 1 ];    /* Translated command text */
   char *varname;        /* String holding environment variable name */
   int def;              /* Environment variable defined? */
   int lpost;            /* Length of converted text */
   int nc;               /* Length of dynamic string */
   size_t lcmd;          /* Length of blank command text */

/* If "status" is set on entry, make a dummy call to "msgLoad" to ensure
   that all message tokens become undefined. Do not make any further
   error reports. */
   if( *status != SAI__OK ) {
      msgLoad( " ", " ", post, sizeof(post), &lpost, status );

/* Otherwise... */
   } else {

/* Attempt to translate an environment variable of the form
   NDF_POST_<FMT> to obtain the post-processing command. Set the format
   to "NDF" if there is no foreign format file. */
      if( !fcb ) {
         ndf1Gtenv( "NDF_POST_NDF", &def, cmd, sizeof( cmd ), &lcmd, status );
      } else {
         nc = 0;
         varname = astAppendStringf( NULL, &nc, "NDF_POST_%s", fcb->name );
         ndf1Gtenv( varname, &def, cmd, sizeof( cmd ), &lcmd, status );
         varname = astFree( varname );
      }

/* If no command was defined (or it was blank), then there is nothing
   more to do. Otherwise, substitute the pre-defined message tokens into
   the blank command, returning the resulting post-processing command
   and its length. Use a low-level (EMS) function to ensure the message
   text supplied is used without change. */
      if( *status == SAI__OK ) {
         if( lcmd != 0 ) {
            emsMload( " ", cmd, post, &lpost, status );

/* If required, report details of the dataset being processed. */
            if( *status == SAI__OK ) {
               if( Ndf_TCB_shcvt ) {
                  msgRenew();
                  if( !fcb ) {
                     msgOut( " ", "--> Post-proc: NDF object ^NDF", status );
                  } else {
                     msgOut( " ", "--> Post-proc: ^FMT file "
                             "^DIR^NAME^TYPE^VERS", status );
                  }

/* Display the values of the flags which controlled the release of the
   dataset. */
                  msgRenew();
                  msgOut( " ", "        flags: keep=^KEEP mod=^MOD "
                          "del=^DEL", status );

/* Display the command being used. */
                  msgSetc( "POST", post );
                  msgOut( " ", "      command: ^POST", status );
               }

/* Execute the post-processing command. */
               ndf1Docmd( post, status );
            }
         }
      }

/* Before exiting, make a dummy call to "msgLoad" to ensure that all
   message tokens become undefined. */
      msgLoad( " ", " ", post, sizeof(post), &lpost, status );

/* Call error tracing function and exit. */
      if( *status != SAI__OK ) ndf1Trace( "ndf1Affor", status );
   }

}

