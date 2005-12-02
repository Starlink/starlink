#include "ems.h"
#include "ems_par.h"
#include "hds1.h"
#include "hds.h"

/*
*+
*  Name:
*     datErtxt

*  Purpose:
*     Report error including text

*  Language:
*     Starlink ANSI C

*  Invocation:
*     datErtxt( const char * text, int * status );

*  Description:
*     This routine reports an error of the form:
*
*        TEXT : status
*
*     Where TEXT is specified in the subroutine call and status
*     is the message text associated with the status value. Normally
*     called when status is bad. The combined message is stored on
*     the error stack. This routine is obsolete as  HDS now reports 
*     its own errors. Any additional error reports from application 
*     should be made using the ERR_ routines (SUN/104). 
*     Error reports from system code should use the EMS_ routines

*  Parameters:
*     text = const char * (Given)
*        Text to be included in error message. Must be nul-terminated.
*     status = int * (Given)
*        Status value to be translated into error message.

*  Authors:
*     A J Chipperfield (RAL::AJC)
*     Rodney Warren-Smith (RFWS)
*     Tim Jenness (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     27-Mar-1987 (RAL::AJC):
*        Original
*     05-Jun-1987 (RAL::AJC):
*        Use EXC_$MSG to get error text
*     25-Jun-1991 (RFWS):
*        Changed to use EMS
*     29-NOV-2005 (TIMJ):
*        Rewrite in C
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}
*-
*/

void datErtxt( const char * text, int * status ) {

   /* Local Variables: */
   char *msg;   /* Pointer to static buffer */
   size_t lmsg;    /* Length of error message */

   /* Get textual translation of the error code */
   datErmsg( *status, &lmsg, &msg );

   /*  Mark the EMS error stack to prevent interaction with any message
    *  tokens already defined and define a token for the text and the error
    *  message. */
   emsMark();
   emsSetc( "TEXT", text );
   emsSetc( "MSG", msg );

   /*  Report the message and release the error stack. */
   emsRep( "HDS_ERROR", "^TEXT: ^MSG", status );
   emsRlse();
}
