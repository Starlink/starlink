#if HAVE_CONFIG_H
#  include <config.h>
#endif

#include "cnf.h"                 /* F77 <-> C string handling functions     */
#include "ems.h"                 /* EMS error reporting routines            */
#include "ems_par.h"             /* EMS public constants                    */
#include "hds1.h"                /* Global definitions for HDS              */
#include "rec.h"                 /* Public rec_ definitions                 */
#include "dat1.h"                /* Internal dat_ definitions               */
#include "dat_err.h"             /* DAT__ error code definitions            */
#define TRUE  1
#define FALSE 0

/* F77_INTEGER_FUNCTION(dat_prmry)( LOGICAL(SET),
 *                                  CHARACTER(LOC),
 *                                  LOGICAL(PRMRY),
 *                                  INTEGER(STATUS)
 *                                  TRAIL(LOC) )
 */
int
datPrmry(int set,
         char loc[DAT__SZLOC],
         int *prmry,
         int *status)
{
/*
*+
*  Name:
*     DAT_PRMRY

*  Purpose:
*     Set or enquire primary/secondary locator status.

*  Language:
*     ANSI C

*  Invocation:
*     CALL DAT_PRMRY( SET, LOC, PRMRY, STATUS )

*  Description:
*     The routine may be used to promote a locator to become a
*     "primary" locator, to demote it to become a "secondary" locator,
*     or to enquire about the primary/secondary status of a locator.
*     It allows control over the duration for which an HDS container
*     file remains open; each file remains open only so long as there
*     is at least one primary locator associated with it.

*  Arguments:
*     SET = LOGICAL (Given)
*        If a .TRUE. value is given for this argument, then the routine
*        will perform a "set" operation to set the primary/secondary
*        status of a locator.  Otherwise it will perform an "enquire"
*        operation to return the value of this status without changing
*        it.
*     LOC = CHARACTER * ( * ) (Given and Returned)
*        The locator whose primary/secondary status is to be set or
*        enquired.
*     PRMRY = LOGICAL (Given and Returned)
*        If SET is .TRUE., then this is an input argument and specifies
*        the new value to be set (.TRUE. for a primary locator, .FALSE.
*        for a secondary locator). If SET is .FALSE., then this is an
*        output argument and will return a value indicating whether or
*        not a primary locator was supplied.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     -  The value of the LOC argument will not normally be changed.
*     However, if it is the last primary locator associated with a
*     container file, and is being demoted from a primary to a
*     secondary locator, then the container file will be left without
*     an associated primary locator. In this case, the locator supplied
*     will be annulled (along with any other secondary locators
*     associated with the same file), a value of DAT__NOLOC will be
*     returned, and the file will be closed.
*     -  The DAT__NOLOC constant is defined in the include file
*     DAT_PAR.

*  Copyright:
*     Copyright (C) 1992 Science & Engineering Research Council

*  Authors:
*     RFWS: R.F. Warren-Smith (STARLINK, RAL)
*     BKM:  B.K. McIlwrath    (STARLINK, RAL)
*     {enter_new_authors_here}

*  History:
*     24-SEP-1992 (RFWS):
*        Original version.
*     23-APR-2002 (BKM):
*        Convert to C interface.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
*/

/* Local Variables:                                                         */
   int refcnt;                   /* Container file reference count          */
   struct LCP *lcp;              /* Pointer to LCP                          */

/*.                                                                         */

/* Check the inherited global status.                                       */
   if ( !_ok( *status) ) return *status;
      hds_gl_status = *status;

/* Import the locator.                                                      */
   dat1_import_loc( loc, DAT__SZLOC, &lcp );
   if ( _ok( hds_gl_status ) )
   {

/* If the locator attribute is being enquired, then return its value.       */
      if ( !set )
      {
         *prmry = lcp->primary ? TRUE : FALSE;
      }

/* Otherwise, if a secondary locator is being promoted to a primary         */
/* locator, then flag it as such and increment the reference count for its  */
/* container file.                                                          */
      else if ( !lcp->primary && *prmry  )
      {
         lcp->primary = 1;
         rec_refcnt( &lcp->data.han, 1, &refcnt, &hds_gl_status );
      }

/* Otherwise, if a primary locator is being demoted to a secondary locator, */
/* then obtain the current reference count for its container file.          */
      else if ( lcp->primary && !( *prmry ) )
      {
         rec_refcnt( &lcp->data.han, 0, &refcnt, &hds_gl_status );
         if ( _ok( hds_gl_status ) )
         {

/* If the reference count is more than one (so there is at least one other  */
/* primary locator still associated with it), then demote the locator and   */
/* decrement the reference count.                                           */
            if ( refcnt > 1 )
            {
               lcp->primary = 0;
               rec_refcnt( &lcp->data.han, -1, &refcnt, &hds_gl_status );
            }

/* Otherwise, annul the LCP, causing the locator supplied and all other     */
/* locators associated with the same container file to become invalid, and  */
/* the container file itself to be closed.                                  */
            else
            {
               dat1_annul_lcp( &lcp );

/* Nullify the locator value.                                               */
/*                  cnf_expn( DAT__NOLOC, DAT__SZLOC, LOC, LOC_length );    */
               strncpy( (char *) loc, DAT__NOLOC, DAT__SZLOC );

            }
         }
      }
   }

/* If an error occurred, then report appropriate contextual information.    */
   if ( !_ok( hds_gl_status ) )
   {
      ems_setc_c( "DOING", set ? "setting" : "enquiring",
                  EMS__SZTOK );
      ems_rep_c( "DAT_PRMRY_ERR",
                 "DAT_PRMRY: Error ^DOING primary locator status.",
                 &hds_gl_status );
   }

/* Return the current global status value.                                  */
   *status = hds_gl_status;
   return *status;
}
