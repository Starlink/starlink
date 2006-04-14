/*+
 *  Name:
 *     emsEload

 *  Purpose:
 *     Return error messages from the current error context.

 *  Language:
 *     Starlink ANSI C

 *  Invocation:
 *     emsEload( param, parlen, opstr, oplen, status )

 *  Description:
 *     This function provides a C interface for the Error Message 
 *     Service routine EMS_ELOAD (written in Fortran).

 *  Arguments:
 *     param = char * (Returned)
 *        The error message name.
 *     parlen = int *  (Returned)
 *        The length of the error message name.
 *     opstr = char * (Returned)
 *        The error message.
 *     oplen = int * (Returned)
 *        The length of the error message.
 *     status = int * (Given and Returned)
 *        The global status.

*  Copyright:
*     Copyright (C) 1990 Science & Engineering Research Council.
*     Copyright (C) 1999, 2001 Central Laboratory of the Research Councls.
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
 *     AJC: A.J. Chipperfield (STARLINK)
 *     RTP: R.T.Platon (STARLINK)
 *     {enter_new_authors_here}

 *  History:
 *     6-JUN-1990 (PCTR):
 *        Original version, coded as a C macro function.
 *     17-SEP-1990 (PCTR):
 *        C function code.
 *     13-MAY-1999 (AJC):
 *        Renamed from ems_eload_c
 *     14-FEB-2001 (RTP):
 *        Rewritten in C from Fortran routine EMS_ELOAD
 *      5-MAR-2001 (AJC):
 *        Simplify
 *        Switch off REVEAL
 *     23-JUL-2001 (AJC):
 *        Correct message names length
 *     13-AUG-2001 (AJC):
 *        Remove unused variables
 *     {enter_further_changes_here}

 *  Bugs:
 *     {note_any_bugs_here}
 *-
 */

/* Include Statements: */
#include <string.h>                  /* String handling library functions */
#include "sae_par.h"
#include "ems_par.h"                 /* EMS_ public constant definitions */
#include "ems.h"                     /* EMS_ function prototypes */
#include "ems_sys.h"                 /* EMS_ private macro definitions */
#include "ems_err.h"
#include "ems_msgtb.h"               /* Error message tables */

/* Function Definitons: */
void emsEload( char *param, int *parlen, char *opstr, int *oplen, 
                  int *status ) {

   static short new = TRUE;            /* Whether a new context active */

/* Temporary holding area for message table */
   static int nerbuf;                      /* Number of error messages */
   static int opcnt;                       /* Message output counter */
   static int erblen[ EMS__MXMSG + 1 ];    /* Error message string lengths */
   static int prblen[ EMS__MXMSG + 1 ];    /* Message name string lengths */
   static int stabuf[ EMS__MXMSG + 1 ];    /* Status values */

   static char parbuf[ EMS__MXMSG + 1 ][ EMS__SZPAR+1 ];  /* Message names */
   static char errbuf[ EMS__MXMSG + 1 ][ EMS__SZMSG+1 ];  /* Message strings */

   int i;                         /* Loop index   */  
   int iend;                      /* Ending loop value */
   int istart;                    /* Starting loop value  */   
   int istat;                     /* Local status */
   int leng;                      /* String length */
   int nerpnt;                    /* Error message pointer */
   short tmprvl;                  /* Temporary storage for MSGRVL */

   TRACE("emsEload");

/*  Is this a new error flush? */
   if ( new ) {

/*     Unset the 'reload holding area' flag */
      new = FALSE;

/*     A new error flush, so initialise output counter. */
      opcnt = 1;

/*     Find the first message to flush. */
      if ( msgmrk > EMS__BASE ) {
         istart = msgcnt[ msgmrk - 1 ] + 1;
      } else {
         istart = 1;
      }

/*     Find the last message. */
      iend = msgcnt[ msgmrk ];

/*     if there are messages to flush, { loop to load error buffer. */
      if ( iend >= istart ) {

/*       Set NERBUF (the number of messages in the context). */
         nerbuf = iend - istart + 1;

         for ( i = 1; i<= nerbuf; i++ ) {
            nerpnt = istart + i - 1;

/*           Construct the output line. */
            strcpy( parbuf[ i ], msgpar[ nerpnt ] );
            prblen[ i ] = msgpln[ nerpnt ];
            strcpy( errbuf[ i ], msgstr[ nerpnt ] );
            erblen[ i ] = msglen[ nerpnt ];
            stabuf[ i ] = msgsta[ nerpnt ];
         }

      } else {

/*        if there are no messages to flush, { load a warning. */
         nerbuf = 1;
         nerpnt = 1;
         strcpy ( parbuf[ nerpnt ], "EMS_ELOAD_NOMSG");
         prblen[ nerpnt ] = strlen( parbuf[ nerpnt ] );
         strcpy ( errbuf[ nerpnt ], 
                    "No error to report (improper use of EMS)." );
         erblen[ nerpnt ] = strlen( errbuf[ nerpnt ] );
         stabuf[ nerpnt ] = EMS__NOMSG;

      }

/*     Annul the error table at the current context.
 *     Switch off MSGRVL whilst doing it as messages have been handled */
      tmprvl = msgrvl;
      msgrvl = FALSE;
      emsAnnul( &istat );
      msgrvl = tmprvl;

   } else {

/*     Increment output counter. */
      opcnt++;
   }

/*  Initialize the return values
 *  These values also serve as the return values if there are no more messages  */
   strcpy (param, " ");
   *parlen = 1;
   strcpy (opstr, " ");
   *oplen = 1;
   *status = SAI__OK;

/*  Now, if there are any messages left to output - return the next one */
   if ( opcnt <= nerbuf ) {
/*     There are more messages - Return the message name */
      leng = prblen[ opcnt ];
      if ( leng  > 0 ) {
/*         *parlen= sprintf( param, "%s%s", param, parbuf[ opcnt ] );*/
         strcpy( param, parbuf[ opcnt ] );
         *parlen = leng;
      }

/*     Return the message string. */
      leng = erblen[ opcnt ];
      if ( leng > 0 ) {
/*         *oplen = sprintf( opstr, "%s%s", errbuf[ opcnt ] );*/
         strcpy( opstr, errbuf[ opcnt ] );
         *oplen = leng;
      }

/*     Return the associated status */
      *status = stabuf[ opcnt ];

   } else {
/*     There are no more messages
 *     Force the holding area to be updated again on the next call */
      new = TRUE;
   }
   return;
}
