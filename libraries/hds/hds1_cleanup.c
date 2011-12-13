#if HAVE_CONFIG_H
#  include <config.h>
#endif

#include "hds1.h"                /* Global definitions for HDS              */
#include "rec.h"                 /* Public rec_ definitions                 */
#include "str.h"                 /* Character string import/export macros   */
#include "dat1.h"                /* Internal dat_ definitions               */
#include "dat_err.h"             /* DAT__ error code definitions            */

   int hds1_cleanup( int *status )
   {

/*
 *+
 *  Name:
 *     hds1_cleanup

 *  Purpose:
 *     Close down and clean up HDS.

 *  Language:
 *     ANSI C

 *  Invocation:
 *     hds1_cleanup( status )

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
 *     BC:   Brad Cavanagh (JAC, Hawaii)
 *     {enter_new_authors_here}

 *  History:
 *     28-NOV-2006 (BC):
 *        Code moved from hdsStop().
 *     {enter_changes_here}

 *  Bugs:
 *     {note_any_bugs_here}

 *-
 */

/* Local Variables:                                                         */
     struct LCP *lcp;

/* Dereference the status pointer.                                          */
     hds_gl_status = *status;

/* Check that HDS is active. There is nothing to do if it is not.           */
     if ( hds_gl_active )
     {

/* Defuse all Locator Control Packets.                                      */
       while ( dat_ga_wlq != NULL )
       {
	 lcp = dat_ga_wlq;
	 dau_defuse_lcp( &lcp );
       }

/* Free memory associated with Locator Control Queue                        */
       dau_free_flq( );

/* Close down the rec_ facility.                                            */
       rec_stop( );

/* Note that HDS is no longer active.                                       */
       hds_gl_active = 0;
     }

     *status = hds_gl_status;

/* Exit the routine.                                                        */
     return *status;

   }
