#if HAVE_CONFIG_H
#  include <config.h>
#endif

#include <string.h>
#include <stddef.h>
#include "f77.h"                 /* F77 <-> C interface macros              */
#include "ems.h"                 /* EMS error reporting routines            */
#include "ems_par.h"             /* EMS__ public constants                  */
#include "hds1.h"                /* Global definitions for HDS              */
#include "rec.h"                 /* Public rec_ definitions                 */
#include "str.h"                 /* Character string import/export macros   */
#include "dat1.h"                /* Internal dat_ definitions               */
#include "dat_err.h"             /* DAT__ error code definitions            */

#include "hds.h"

/* F77_INTEGER_FUNCTION(dat_ermsg)( const F77_INTEGER_TYPE *status,
 *                                  F77_INTEGER_TYPE *len,
 *                                  struct STR *msg_str
 *                                  TRAIL(msg_str) )
 */

int
datErmsg(int  status,
         size_t *len,
         char *msg_str)
{
/*
*+
*  Name:
*     DAT_ERMSG

*  Purpose:
*     Translate a status value into an error message.

*  Language:
*     ANSI C

*  Invocation:
*     CALL DAT_ERMSG( STATUS, LENGTH, MSG )

*  Description:
*     This routine translates an error status value into an associated
*     error message. It first attempts to translate the value supplied
*     as a DAT__ error code. If this fails, it then attempts to
*     translate it as a system status code for the host operating
*     system. If this also fails, then the returned string is a message
*     indicating that the status value could not be translated.

*  Arguments:
*     STATUS = INTEGER (Given)
*        The error status value to be translated.
*     LENGTH = INTEGER (Returned)
*        Number of significant characters in the returned error message
*        (i.e.  excluding trailing blanks). This value will not exceed
*        the length of the character variable supplied for the MSG
*        argument.
*     MSG = CHARACTER * ( * ) (Returned)
*        Buffer of at least EMS__SZMSG+1 bytes to receive the error message.

*  Notes:
*     -  No returned error message will contain more significant
*     characters than the value of the EMS__SZMSG symbolic constant.
*     This constant is defined in the include file EMS_PAR.
*     - The C interface does not check the length of MSG.

*  Authors:
*     RFWS: R.F. Warren-Smith (STARLINK, RAL)
*     BKM:  B.K. McIlwrath    (STARLINK, RAL)
*     TIMJ: Tim Jenness (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     5-APR-1991 (RFWS):
*        Original portable version.
*     8-APR-1991 (RFWS):
*        Improved prologue.
*     13-MAY-1991 (RFWS):
*        Added calls to emsMark and emsRlse to prevent use of ems_
*        routines from affecting any previously defined message tokens.
*     9-JAN-1992 (RFWS):
*        Updated error message for DAT__NOMEM status.
*     1-DEC-1992 (RFWS):
*        Added DAT__WLDIN error message.
*     29-OCT-2000 (BKM):
*        Revised argument list for full CNF compatability.
*     17-MAY-2002 (BKM):
*        Convert to C routine.
*     22-AUG-2002 (BKM):
*        Revise C interface and test
*     29-NOV-2005 (TIMJ):
*        No reason to pass in a pointer to status.
*        Initialise trans. Use size_t for len arg.
*        Use modern ems interface.
*     2012-02-23 (TIMJ):
*        Fill a supplied buffer rather than returning a pointer to
*        stack storage.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
*/

/* Local Variables:                                                         */
   const char *trans = NULL;         /* Pointer to translation text         */
   int lstat;                        /* Local status variable               */
   int emslen;                       /* Length from EMS                     */

/*.                                                                         */

/* Test for each DAT__ error code, obtaining a pointer to the textual       */
/* translation.                                                             */
   switch ( status )
   {
      default:
         trans = NULL;
         break;

      case DAT__OK:
         trans = "OK, no error (DAT__OK)";
         break;

      case DAT__LOCIN:
         trans = "Locator invalid (DAT__LOCIN)";
         break;

      case DAT__TYPIN:
         trans = "Type invalid (DAT__TYPIN)";
         break;

      case DAT__NAMIN:
         trans = "Name invalid (DAT__NAMIN)";
         break;

      case DAT__MODIN:
         trans = "Mode invalid (DAT__MODIN)";
         break;

      case DAT__DELIN:
         trans = "Deletion invalid (DAT__DELIN)";
         break;

      case DAT__DIMIN:
         trans = "Dimensions invalid (DAT__DIMIN)";
         break;

      case DAT__FILIN:
         trans = "File invalid (DAT__FILIN)";
         break;

      case DAT__OBJIN:
         trans = "Object invalid (DAT__OBJIN)";
         break;

      case DAT__GRPIN:
         trans = "Group invalid (DAT__GRPIN)";
         break;

      case DAT__SUBIN:
         trans = "Subscripts invalid (DAT__SUBIN)";
         break;

      case DAT__COMEX:
         trans = "Component already exists (DAT__COMEX)";
         break;

      case DAT__OBJNF:
         trans = "Object not found (DAT__OBJNF)";
         break;

      case DAT__TRUNC:
         trans = "Text truncated (DAT__TRUNC)";
         break;

      case DAT__ACCON:
         trans = "Access conflict (DAT__ACCON)";
         break;

      case DAT__CONER:
         trans = "Conversion error (DAT__CONER)";
         break;

      case DAT__UNSET:
         trans = "Primitive data undefined (DAT__UNSET)";
         break;

      case DAT__VERMM:
         trans = "Version mismatch (DAT__VERMM)";
         break;

      case DAT__PRMAP:
         trans = "Primitive data mapped (DAT__PRMAP)";
         break;

      case DAT__FILCK:
         trans = "File lock error (DAT__FILCK)";
         break;

      case DAT__FILNF:
         trans = "File not found (DAT__FILNF)";
         break;

      case DAT__FILPR:
         trans = "File protected (DAT__FILPR)";
         break;

      case DAT__INCHK:
         trans = "Integrity check (DAT__INCHK)";
         break;

      case DAT__FATAL:
         trans = "Fatal internal error (DAT__FATAL)";
         break;

      case DAT__ISMAP:
         trans = "Data currently mapped (DAT__ISMAP)";
         break;

      case DAT__BOUND:
         trans = "Outside bounds of object (DAT__BOUND)";
         break;

      case DAT__FILCL:
         trans = "File close error (DAT__FILCL)";
         break;

      case DAT__FILCR:
         trans = "File create error (DAT__FILCR)";
         break;

      case DAT__FILMP:
         trans = "File mapping error (DAT__FILMP)";
         break;

      case DAT__FILND:
         trans = "File not deleted (DAT__FILND)";
         break;

      case DAT__FILNX:
         trans = "File not extended (DAT__FILNX)";
         break;

      case DAT__FILRD:
         trans = "File read error (DAT__FILRD)";
         break;

      case DAT__FILWR:
         trans = "File write error (DAT__FILWR)";
         break;

      case DAT__NOMEM:
         trans = "Memory allocation error (DAT__NOMEM)";
         break;

      case DAT__WLDIN:
         trans = "Wild card search context invalid (DAT__WLDIN)";
         break;
   }

/* If translation text was found, then determine the number of significant  */
/* characters to be returned and copy them to the output string.            */
   if ( trans != NULL )
   {
      strcpy( msg_str, trans );
      *len = strlen( msg_str );
   }

/* If the error code is not a DAT__ error code, then use ems_ to translate  */
/* it as a system error code, and copy the resulting text to the output     */
/* string.                                                                  */
   else
   {
      lstat = DAT__OK;
      emsMark( );
      emsSyser( "MESSAGE", status );
      emsMload( " ", "^MESSAGE", msg_str, &emslen, &lstat );
      *len = emslen;
      emsRlse( );
   }

/* Exit the routine.                                                        */
   return DAT__OK;
}
