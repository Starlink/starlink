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
/*    Copyright (C) 1992 Science & Engineering Research Council             */

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
/* as an exit handler.                                                      */
            if ( first )
            {
               if ( !atexit( hds1_exit ) )
               {
                  first = 0;
               }

/* If an error occurred, then report it.                                    */
               else
               {
                  hds_gl_status = DAT__FATAL;
                  emsSyser( "MESSAGE", errno );
                  ems_rep_c( "DAT1_INIT_1",
                             "Error establishing an exit handler for HDS - \
^MESSAGE",
                             &hds_gl_status );
               }
            }
         }
      }

/* Exit the routine, returning the current global status value.             */
      return hds_gl_status;
   }
