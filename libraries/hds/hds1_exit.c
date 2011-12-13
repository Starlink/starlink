#if HAVE_CONFIG_H
#  include <config.h>
#endif

#include "ems.h"                 /* EMS error reporting routines            */
#include "hds1.h"                /* Global definitions for HDS              */
#include "rec.h"                 /* Public rec_ definitions                 */
#include "str.h"                 /* Character string import/export macros   */
#include "dat1.h"                /* Internal dat_ definitions               */
#include "dat_err.h"             /* DAT__ error code definitions            */
#include "hds.h"

   void hds1_exit( void )
   {
/*+                                                                         */
/* Name:                                                                    */
/*    hds1_exit                                                             */

/* Purpose:                                                                 */
/*    HDS exit handler - close down HDS.                                    */

/* Invocation:                                                              */
/*    hds1_exit( )                                                          */

/* Description:                                                             */
/*    This function acts as an exit handler for HDS. It is intended to      */
/*    execute immediately before a program terminates, at which point it    */
/*    will close HDS down, annulling all Locator Control Packets and        */
/*    flushing all internal buffers as if HDS_STOP had been called.         */

/* Parameters:                                                              */
/*    void                                                                  */

/* Returned Value:                                                          */
/*    void                                                                  */

/* Notes:                                                                   */
/*    This routine takes no action if HDS is not active.                    */

/* Copyright:                                                               */
/*    Copyright (C) 1992 Science & Engineering Research Council             */
/*    Copyright (C) 2006 Particle Physics and Astronomy Research Council    */
/*    Copyright (C) 2007 Science and Technology Facilities Council          */
/*    All Rights Reserved                                                   */

/*  Licence:                                                                */
/*     This program is free software; you can redistribute it and/or        */
/*     modify it under the terms of the GNU General Public License as       */
/*     published by the Free Software Foundation; either version 2 of       */
/*     the License, or (at your option) any later version.                  */

/*     This program is distributed in the hope that it will be              */
/*     useful, but WITHOUT ANY WARRANTY; without even the implied           */
/*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR              */
/*     PURPOSE. See the GNU General Public License for more details.        */

/*     You should have received a copy of the GNU General Public            */
/*     License along with this program; if not, write to the Free           */
/*     Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,       */
/*     MA 02110-1301, USA                                                   */

/* Authors:                                                                 */
/*    RFWS: R.F. Warren-Smith (STARLINK)                                    */
/*    TIMJ: Tim Jenness (JAC, Hawaii)                                       */
/*    BC: Brad Cavanagh (JAC, Hawaii)                                       */
/*    {@enter_new_authors_here@}                                            */

/* History:                                                                 */
/*    4-APR-1991 (RFWS):                                                    */
/*       Original version.                                                  */
/*    25-SEP-1992 (RFWS):                                                   */
/*       Revised method of traversing working locator queue.                */
/*    01-FEB-2006 (TIMJ):                                                   */
/*       Call hdsStop rather than duplicating code                          */
/*    28-NOV-2006 (BC):                                                     */
/*       Replace hdsStop with hds1_cleanup.                                 */
/*    14-JUN-2006 (BC):                                                     */
/*       Allow for HDS_DISABLE_CLEANUP environment variable to disable HDS  */
/*       cleanup from happening.                                            */
/*    20-JUN-2006 (TIMJ):                                                   */
/*       Move exit handler disabling to dat1_init.c. It seems that a SEGV   */
/*       can corrupt the environment which leads to a getenv segv.          */
/*    {@enter_changes_here@}                                                */

/* Bugs:                                                                    */
/*    {@note_any_bugs_here@}                                                */

/*-                                                                         */

/* Local Variables:                                                         */
      int status = DAT__OK;      /* Local status for hdsStop                */

/*.                                                                         */

/* Close down everything                                                    */
      hds1_cleanup( &status );

/* Exit the routine.                                                        */
      return;
   }
