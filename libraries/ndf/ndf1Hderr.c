#include <string.h>
#include "sae_par.h"
#include "dat_par.h"
#include "ndf1.h"
#include "ems_par.h"
#include "ems.h"
#include "mers.h"
#include "star/util.h"
#include "ndf_ast.h"

void ndf1Hderr( NdfDCB *dcb, int rel, int *status ){
/*
*+
*  Name:
*     ndf1Hderr

*  Purpose:
*     Dump logged error messages to an NDF history record.

*  Synopsis:
*     void ndf1Hderr( NdfDCB *dcb, int rel, int *status )

*  Description:
*     This function writes information about any logged error messages to
*     the end of the current history record of an NDF. The error message
*     information is extracted from the Error Logging Block (ELB). No
*     history information is written unless errors (or an associated error
*     status) have previously been logged in this common block (by
*     ndf1Hlerr).

*  Parameters:
*     dcb
*        Pointer to the DCB entry for the data object whose history
*        component is to be updated.
*     rel
*        A non-zero value indicates that the NDF to which error information
*        is to be logged is in the process of being released from the NDF
*        system. Otherwise, it is assumed to be remaining in use. This flag
*        only affects the wording of the history text.
*     *status
*        The global status.

*  Notes:
*     -  This function attempts to execute even if "status" is set on
*     entry, although no further error report will be made if it
*     subsequently fails under these circumstances.
*     -  No history information will be written by this function if there
*     is no history component present in the NDF, nor if the history update
*     mode is DISABLED.

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
   char msg[ NDF__SZHMX + 1 ];/* Error message text */
   char textbuf[ 3 ][ NDF__SZHIS + 1 ];/* History text */
   char *blank = " ";         /* Blank text line */
   char *p;                   /* Pointer to string */
   char *text[ 3 ];           /* Pointers to history text */
   int i;                     /* Loop counter for error messages */

/* Get a pointer to the structure holding thread-specific data values for
   the current thread. */
   NDF_GETTSD

/* Set up pointers to histort text buffers */
   text[ 0 ] = textbuf[ 0 ];
   text[ 1 ] = textbuf[ 1 ];
   text[ 2 ] = textbuf[ 2 ];

/* Check whether the ELB contains any error messages which have been
   logged for recording in the history record, or whether an associated
   "status" value has been logged (otherwise, there is nothing to do). */
   if( NDF_TSD(elbNerr) > 0 || NDF_TSD(elbStat) != SAI__OK ) {

/* Begin a new error reporting environment. */
      errBegin( status );

/* Ensure that history component information is available in the DCB. */
      ndf1Dh( dcb, status );
      if( *status == SAI__OK ) {

/* Check that a history component is present and that the history
   recording mode is not disabled. Otherwise there is nothing to do. */
         if( ( dcb->hloc ) && ( dcb->humod != NDF__HDISA ) ) {

/* Write a separating blank line to the history record if history text
   has previously been written to it. */
            if( dcb->htlen != 0 ) ndf1Hfwrt( dcb, " ", 1, &blank, 0, 0, 0,
                                             status );

/* Set up text to report the logged error status value and its
   translation. */
            if( rel ) {
               star_strlcpy( textbuf[ 0 ], "*** WARNING *** This data "
                             "structure was released by the",
                             sizeof( textbuf[ 0 ] ) );
               star_strlcpy( textbuf[ 1 ], "application while its status was "
                             "set to the error value ^STAT,",
                             sizeof( textbuf[ 1 ] ) );
               star_strlcpy( textbuf[ 2 ], "which translates to the message:",
                             sizeof( textbuf[ 2 ] ) );
            } else {
               star_strlcpy( textbuf[ 0 ], "*** WARNING *** This application "
                             "ended with its status set to",
                             sizeof( textbuf[ 0 ] ) );
               star_strlcpy( textbuf[ 1 ], "the error value ^STAT,",
                             sizeof( textbuf[ 1 ] ) );
               star_strlcpy( textbuf[ 2 ], "which translates to the message:",
                             sizeof( textbuf[ 2 ] ) );
            }

/* Define the message token and write the expanded text to the history
   record, followed by a blank line. */
            msgSeti( "STAT", NDF_TSD(elbStat) );
            ndf1Hfwrt( dcb, " ", 3, text, 1, 1, 0, status );
            ndf1Hfwrt( dcb, " ", 1, &blank, 0, 0, 0, status );

/* Set up text for the error status translation. */
            star_strlcpy( textbuf[ 0 ], "   ^MESSAGE", sizeof( textbuf[ 0 ] ) );

/* Define the message token and write the expanded text to the history
   record, followed by a blank line. */
            emsFacer( "MESSAGE", NDF_TSD(elbStat) );
            ndf1Hfwrt( dcb, " ", 1, text, 1, 0, 0, status );
            ndf1Hfwrt( dcb, " ", 1, &blank, 0, 0, 0, status );

/* Set up text to explain the list of error messages (if any) which
   follows. */
            if( NDF_TSD(elbNerr) > 0 ) {
               star_strlcpy( textbuf[ 0 ], "The following additional error "
                             "messages were "
                             "pending...", sizeof( textbuf[ 0 ] ) );
            } else {
               star_strlcpy( textbuf[ 0 ], "There were no additional error "
                             "messages pending.", sizeof( textbuf[ 0 ] ) );
            }

/* Write this text to the history record, followed by a blank line if
   appropriate. */
            ndf1Hfwrt( dcb, " ", 1, text, 1, 1, 0, status );
            if( NDF_TSD(elbNerr) > 0 ) ndf1Hfwrt( dcb, " ", 1, &blank, 0, 0, 0, status );

/* Loop to append any logged error messages to the history record. Add
   an appropriate number of "!" characters at the start to simulate a
   normal error report. */
            for( i = 0; i < NDF_TSD(elbNerr); i++ ){
               if( i == 0 ) {
                  strcpy( msg, "!! " );
                  star_strlcpy( msg + 3, NDF_TSD(elbMsg)[ i ], sizeof( msg ) - 3 );
               } else {
                  strcpy( msg, "! " );
                  star_strlcpy( msg + 2, NDF_TSD(elbMsg)[ i ], sizeof( msg ) - 2 );
               }

/* If this causes any message to become too long to be accommodated by
   the history recording system, then append an ellipsis to indicate
   truncation. */
               if( astChrLen( NDF_TSD(elbMsg)[ i ] ) > ( NDF__SZHMX - 3 ) ) {
                  star_strlcpy( msg + NDF__SZHMX - 3, "...",
                                sizeof( msg ) - NDF__SZHMX + 3 );
               }

/* Write each message inside its own error reporting environment so
   that we get the maximum information transferred if errors occur. */
               errBegin( status );
               p = msg;
               ndf1Hfwrt( dcb, " ", 1, &p, 0, 0, 0, status );
               errEnd( status );
            }
         }
      }

/* Call error tracing function. */
      if( *status != SAI__OK ) ndf1Trace( "ndf1Hderr", status );

/* End the outer error reporting environment. */
      errEnd( status );
   }

}

