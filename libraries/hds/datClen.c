#if HAVE_CONFIG_H
#  include <config.h>
#endif

#include "ems.h"                 /* EMS error reporting routines            */
#include "hds1.h"                /* Global definitions for HDS              */
#include "rec.h"                 /* Public rec_ definitions                 */
#include "str.h"                 /* Character string import/export macros   */
#include "dat1.h"                /* Internal dat_ definitions               */
#include "dat_err.h"             /* DAT__ error code definitions            */

#include "hds.h"                 /* Public prototype of this routine        */

int
datClen( HDSLoc* locator,
         int *clen,
         int *status )
{
/*
*+
*  Name:
*     DAT_CLEN

*  Purpose:
*     Obtain character string length.

*  Language:
*     ANSI C

*  Invocation:
*     CALL DAT_CLEN( LOC, CLEN, STATUS )

*  Description:
*     The routine returns the number of characters required to
*     represent the values of a primitive object. If the object is
*     character-type, then its length is returned directly. Otherwise,
*     the value returned is the number of characters required to format
*     the object's values (as a decimal string if appropriate) without
*     loss of information.

*  Arguments:
*     LOC = CHARACTER * ( * ) (Given)
*        Primitive object locator.
*     CLEN = INTEGER (Returned)
*        Character string length.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     -  The value returned by this routine is equal to the default
*     number of characters allocated to each element whenever a
*     primitive object is mapped using an access type of '_CHAR' (i.e.
*     without specifying the length to be used explicitly).
*     -  If this routine is called with STATUS set, then a value of 1
*     will be returned for the CLEN argument, although no further
*     processing will occur. The same value will also be returned if
*     the routine should fail for any reason.

*  Copyright:
*     Copyright (C) 1992 Science & Engineering Research Council

*  Authors:
*     RFWS: R.F. Warren-Smith (STARLINK, RAL)
*     BKM:  B.K. McIlwrath    (STARLINK, RAL)
*     TIMJ: Tim  Jenness      (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     13-AUG-1991 (RFWS):
*        Original version.
*     25-MAR-2002 (BKM):
*        Pure C version
*     15-NOV-2005 (TIMJ):
*        Use dat1_import_loc
*        Change API to use HDSLoc
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
*/

/* Local Variables:                                                         */
   struct LCP *lcp;           /* Pointer to Locator Control Packet          */
   struct LCP_DATA *data=NULL;/* Pointer to LCP data fields                 */
   struct PDD *obj;           /* Pointer to object PDD                      */

/*.                                                                         */

/* Set an initial (safe) default value of 1 for the returned length.        */
   *clen = 1;

/* Check the inherited global status.                                       */
   if ( !_ok( *status ) )
      return *status;
   hds_gl_status = *status;

/* Import the locator and obtain a pointer to the LCP data fields.          */
   dat1_import_loc( locator, &lcp );
   if ( _ok( hds_gl_status ) )
   {
      data = &lcp->data;

/* Report an error if the object is a structure.                            */
      if ( data->struc )
      {
         hds_gl_status = DAT__OBJIN;
         ems_rep_c( "DAT_CLEN_1",
                    "Object is not primitive; the character string length \
is not defined (possible programming error).",
                    &hds_gl_status );
      }
   }

/* Otherwise, obtain a pointer to the object primitive data descriptor.     */
   if ( _ok( hds_gl_status ) )
   {
      obj = &data->obj;

/* If the object is not character type, then obtain the number of           */
/* characters required to format its value(s) as a character string from    */
/* the Native Data Representation lookup table.                             */
      if ( obj->dtype != DAT__C )
      {
         *clen = dat_gl_ndr[ obj->dtype ].txtsize;
      }

/* Otherwise, use the object's length directly.                             */
      else
      {
         *clen = obj->length;
      }
   }

/* If an error occurred, then report contextual information.                */
   if ( !_ok( hds_gl_status ) )
   {
      ems_rep_c( "DAT_CLEN_ERR",
                 "DAT_CLEN: Error obtaining the character string length \
of an HDS primitive.",
                 &hds_gl_status );
   }

/* Return the current global status value.                                  */
   *status = hds_gl_status;
   return *status;
}
