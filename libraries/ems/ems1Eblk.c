/*      BLOCK DATA EMS1_EBLK
*+
*  Name:
*     EMS1_EBLK

*  Purpose:
*     Initial contents of the error message table.

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     External variables

*  Description:
*     This routine initialises the EMS_ error message table.

*  Authors:
*     BDK: B.D. Kelly (ROE)
*     PCTR: P.C.T. Rees (STARLINK)
*     AJC: A.J. Chipperfield (STARLINK)
*     RTP: R.T. Platon (STARLINK)
*     {enter_new_authors_here}

*  History:
*     10-JUN-1987 (BDK):
*        Original Fortran version.
*     14-FEB-2001 (RTP)
*        Rewritten in C based on the Fortran routine EMS1_EBLK
*     13-AUG-2001 (AJC):
*        Cast emsTblk to void
*     {enter_further_changes_here}

*-

*/

#include "sae_par.h"                 /* Standard SAE constants */
#include "ems_par.h"                 /* EMS_ public constants */
#include "ems_sys.h"                 /* EMS_ private constants */
#include "ems_err.h"                 /* EMS_ error codes */

int msgdef=EMS__BASE;          /* Default error reporting context */
int msglev=EMS__BASE;          /* Error context level */
int msglst=SAI__OK;            /* Last reported status (level 1 only) */
int msgmrk=EMS__BASE;          /* Number of markers */
int msgcnt[ EMS__MXLEV + 1 ];  /* Number of messages in table by level */
int msgpln[ EMS__MXMSG + 1 ];  /* Error parameter string lengths */
int msglen[ EMS__MXMSG + 1 ];  /* Error message string lengths */
int msgsta[ EMS__MXMSG + 1 ];  /* Status values with messages */
      

char msgpar[ EMS__MXMSG + 1 ][ EMS__SZPAR + 1 ];     /* Error parameter strings */

char msgstr[ EMS__MXMSG + 1 ][ EMS__SZMSG + 1 ];     /* Error message strings */

int msgbgs[ EMS__MXLEV + 1 ] = {EMS__NSTER}; /* Given status values to EMS_BEGIN */

int msgwsz=EMS__SZOUT;       /* Line wrapping length */

short msgrvl=FALSE;          /* Whether EMS tuning is REVEAL */
short msgslt=FALSE;          /* Whether EMS tuning is SILENT */
short msgstm=FALSE;          /* Whether EMS tuning is STREAM */

extern void ems1Tblk();
void ems1Eblk(){
/* A dummy call to force loading of ems1Tblk as we can't do it within
 * the link script
*/
(void)ems1Tblk();
}
