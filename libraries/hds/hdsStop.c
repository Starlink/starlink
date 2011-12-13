#if HAVE_CONFIG_H
#  include <config.h>
#endif

#include "ems.h"                 /* EMS error reporting routines            */
#include "hds1.h"                /* Global definitions for HDS              */
#include "rec.h"                 /* Public rec_ definitions                 */
#include "str.h"                 /* Character string import/export macros   */
#include "dat1.h"                /* Internal dat_ definitions               */
#include "dat_err.h"             /* DAT__ error code definitions            */

int
hdsStop( int *status)
{
/*
*+
*  Name:
*     HDS_STOP

*  Purpose:
*     Close down HDS.

*  Language:
*     ANSI C

*  Invocation:
*     CALL HDS_STOP( STATUS )

*  Description:
*     This routine closes down HDS, annulling all active locators,
*     closing all container files and releasing all associated
*     resources.  It returns without action if HDS is not active.

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     This routine attempts to execute even if STATUS is set on entry,
*     although no further error report will be made if it subsequently
*     fails under these circumstances.

*  Copyright:
*     Copyright (C) 1992 Science & Engineering Research Council
*     Copyright (C) 2006 Particle Physics and Astronomy Research Council.
*     All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either version 2 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful, but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public
*     License along with this program; if not, write to the Free
*     Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,
*     MA 02110-1301, USA

*  Authors:
*     RFWS: R.F. Warren-Smith (STARLINK, RAL)
*     BKM:  B.K. McIlwrath    (STARLINK, RAL)
*     TIMJ: Tim Jenness (JAC, Hawaii)
*     BC:   Brad Cavanagh (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     4-APR-1991 (RFWS):
*        Added prologue and error handling and made portable.
*     13-DEC-2001 (BKM):
*        Convert to a C function with FORTRAN wrapper.
*     01-FEB-2005 (TIMJ):
*        Free memory associated with the locator control queue
*     28-NOV-2006 (BC):
*        Replace non-EMS code with call to hds1_cleanup.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
*/

/* Local Variables:                                                         */
  struct LCP *lcp;               /* Pointer to Locator Control Packet       */

/*.                                                                         */

/* Begin a new error reporting context.                                     */
   emsBegin( status );
   hds_gl_status = *status;

/* Check that HDS is active. There is nothing to do if it is not.           */
   if ( hds_gl_active )
   {

/* Clean up HDS.                                                            */
      hds1_cleanup( status );

/* If an error occurred, then report contextual information.                */
      if ( !_ok( hds_gl_status ) )
      {
         emsRep( "HDS_STOP_ERR",
                    "HDS_STOP: Error deactivating the Hierarchical Data \
System (HDS).",
                    &hds_gl_status );
      }
   }

/* End the error reporting context.                                         */
   *status = hds_gl_status;
   emsEnd( status );

/* Exit the routine.                                                        */
   return *status;
}
