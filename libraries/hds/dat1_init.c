#if HAVE_CONFIG_H
#  include <config.h>
#endif

#include <stdlib.h>
#include <errno.h>
#include "ems.h"                 /* EMS error reporting routines            */
#include "ems_par.h"             /* EMS public constants                    */
#include "hds1.h"                /* Global definitions for HDS              */
#include "rec.h"                 /* Public rec_ definitions                 */
#include "dat1.h"                /* Internal dat_ definitions               */
#include "dat_err.h"             /* DAT__ error code definitions            */

   int dat1_init( void )
   {
/*+                                                                         */
/* Name:                                                                    */
/*    dat1_init                                                             */

/* Purpose:                                                                 */
/*    Initialise HDS.                                                       */

/* Language:                                                                */
/*    ANSI C                                                                */

/* Invocation:                                                              */
/*    dat1_init( )                                                          */

/* Description:                                                             */
/*    This routine should be called to initialise HDS prior to making calls */
/*    to other internal HDS routines which make use of locators or global   */
/*    tuning parameters. It ensures that HDS is active by initialising      */
/*    global tuning parameters, setting up the working/free locator queues, */
/*    allocating space for the latter, initialising the global native data  */
/*    representation information and establishing an "exit handler" to      */
/*    clean up when the program terminates.                                 */

/* Parameters:                                                              */
/*    void                                                                  */

/* Returned Value:                                                          */
/*    int dat1_init                                                         */
/*       The global status value current on exit.                           */

/* Copyright:                                                               */
/*    Copyright (C) 1991, 1992 Science & Engineering Research Council       */
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
/*    {@enter_new_authors_here@}                                            */

/* History:                                                                 */
/*    4-APR-1991 (RFWS):                                                    */
/*       Added prologue and made portable.                                  */
/*    24-FEB-1992 (RFWS):                                                   */
/*       Converted into an internal routine to be called automatically.     */
/*    26-FEB-1992 (RFWS):                                                   */
/*       Separated out the initialisation of tuning parameters.             */
/*    7-SEP-1992 (RFWS):                                                    */
/*       Installed initialisation of the global native data representation  */
/*       information.                                                       */
/*    20-JUN-2007 (TIMJ):                                                   */
/*       Add HDS_DISABLE_CLEANUP (useful for debugging) to disable          */
/*       the exit handler.                                                  */
/*    {@enter_further_changes_here@}                                        */

/* Bugs:                                                                    */
/*    {@note_any_bugs_here@}                                                */

/*-                                                                         */

/* Local Variables:                                                         */
      static int first = 1;      /* First invocation of this routine?       */

/*.                                                                         */

/* Check the inherited global status.                                       */
      if ( !_ok( hds_gl_status ) ) return hds_gl_status;

/* Check if HDS is already active. There is nothing to do if it is.         */
      if ( !hds_gl_active )
      {

/* Initialise HDS tuning parameters.                                        */
         dat1_intune( &hds_gl_status );

/* Start up the rec_ facility.                                              */
         rec_start( );

/* Initialise the Working and Free Locator Queues. Then fill the free queue */
/* with a cluster of virgin Locator Control Packets.                        */
         dat_ga_wlq = NULL;
         dat_ga_flq = NULL;
         dau_refill_flq( );

/* Initialise the global native data representation information.            */
         dat1_init_ndr( &hds_gl_status );

/* Note if HDS is now active.                                               */
         if ( _ok( hds_gl_status ) )
         {
            hds_gl_active = 1;

/* On the first (successful) invocation of this routine, declare hds1_exit  */
/* as an exit handler. (if we want it)                                      */
            if ( first )
            {
	      /* see if we want an exit handler */
	      if (!getenv( "HDS_DISABLE_CLEANUP")) {
		    if ( !atexit( hds1_exit ) )
		      {
			first = 0;
		      }

/* If an error occurred, then report it.                                    */
		    else
		      {
			hds_gl_status = DAT__FATAL;
			emsSyser( "MESSAGE", errno );
			emsRep( "DAT1_INIT_1",
				"Error establishing an exit handler for HDS - ^MESSAGE",
				&hds_gl_status );
		      }
	      } else {
		first = 0;
	      }
	    }
         }
      }

/* Exit the routine, returning the current global status value.             */
      return hds_gl_status;
   }
