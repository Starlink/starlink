/*
*+
*  Name:
*     EMS1ESTOR

*  Purpose:
*     Store an error message in the current context of the error table.

*  Language:
*     Starlink ANSI C

*  Invocation:
*     ems1Estor( param, plen, msg, mlen, status )

*  Description:
*     This routine stores an error message in the current context of the 
*     error table. If there is no room in the error table, then the last
*     reported error message is replaced by a fault message.

*  Arguments:
*     param = char* (Given)
*        The error message name.
*     plen = char* (Given)
*        The length of error message name.
*     msg = char* (Given)
*        The error message text.
*     mlen = char* (Given)
*        The length of error message text.
*     status = int (Given and returned)
*        The global status. 

*  Algorithm:
*     -  Find a slot in the error table: if the error message table
*     is full, then replace the last reported error message an EMS_ 
*     fault message.
*     -  Stack the message for output in the error table.
*     -  Check if the error context level is the lowest: if it is, 
*     store the last reported status value and flush the error table.

*  Copyright:
*     Copyright (C) 1990 Science & Engineering Research Council.
*     Copyright (C) 2001 Central Laboratory of the Research Councls.
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
*     Foundation, Inc., 59 Temple Place,Suite 330, Boston, MA
*     02111-1307, USA

*  Authors:
*     PCTR: P.C.T. Rees (STARLINK)
*     RTP: R.T. Platon (STARLINK)
*     {enter_new_authors_here}

*  History:
*     2-MAR-1990 (PCTR):
*        Original FORTRAN version.
*     14-FEB-2001 (RTP)
*        Rewritten in C based on the Fortran routine EMS1_ESTOR
*     13-AUG-2001 (AJC):
*        #include ems1.h
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
*/

/*  Global Constants: */
#include "sae_par.h"
#include "ems1.h"                    /* EMS1_ function prototypes */
#include "ems_err.h"                 /* EMS_ error codes */
#include "ems_par.h"                 /* EMS_ public constants */
#include "ems_sys.h"                 /* EMS_ private constants */

/*  Global Variables: */
#include "ems_msgtb.h"               /* EMS_ error message table */

void ems1Estor( const char *param, int plen, const char *msg,
                 int mlen, int *status ) {
   int index;                     /* Table index */
   int istat;                     /* Local status */

   TRACE("ems1Estor");
   DEBUG("ems1Estor","msglev = %d", msglev );
 
/*  If the message table is full, then replace the last reported error
*  message with an EMS_ fault message. */
      if ( msgcnt[ msgmrk ] == EMS__MXMSG ) {
         strcpy( msgstr[ EMS__MXMSG ], "Error stack overflow (EMS fault)." );
         msglen[ EMS__MXMSG ] = (int) strlen( msgstr[ EMS__MXMSG ] );
         strcpy( msgpar[ EMS__MXMSG ], "ems_estor_ovflo" );
         msgpln[ EMS__MXMSG ] = (int) strlen( msgpar[ EMS__MXMSG ] );
         msgsta[ EMS__MXMSG ] = EMS__EROVF;

      } else {
/*     Increment the message count for the current context. */
         msgcnt[ msgmrk ]++;

/*     Store the STATUS. */
         index = msgcnt[ msgmrk ];
         msgsta[ index ] = *status;

/*     Store the error message name in the error table. */
         msgpln[ index ] = MIN( plen, EMS__SZPAR );
         strcpy( msgpar[ index ], param );

/*     Store the error message text in the error table. */
         msglen[ index ] = MIN( mlen, EMS__SZMSG );
         strcpy( msgstr[ index ], msg );
      }

/*  Check the error context and flush the error table if it is at the base 
*  level (i.e. EMS__BASE). */
      if ( msglev == EMS__BASE ) {

/*     Flush the error table. */
         istat = *status;
         ems1Flush( &istat );

/*     Store the last reported status. */
         msglst = *status;

/*     Set the returned status, STATUS, on output error. */
         if ( istat != SAI__OK ) *status = istat;
      }
      return;
}
