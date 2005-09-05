#if HAVE_CONFIG_H
#  include <config.h>
#endif

#include "ems.h"                 /* EMS error reporting routines            */
#include "hds1.h"                /* Global definitions for HDS              */
#include "rec.h"                 /* Public rec_ definitions                 */
#include "str.h"                 /* Character string import/export macros   */
#include "dat1.h"                /* Internal dat_ definitions               */
#include "dat_err.h"             /* DAT__ error code definitions            */

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

/* Authors:                                                                 */
/*    RFWS: R.F. Warren-Smith (STARLINK)                                    */
/*    {@enter_new_authors_here@}                                            */

/* History:                                                                 */
/*    4-APR-1991 (RFWS):                                                    */
/*       Original version.                                                  */
/*    25-SEP-1992 (RFWS):                                                   */
/*       Revised method of traversing working locator queue.                */
/*    {@enter_changes_here@}                                                */

/* Bugs:                                                                    */
/*    {@note_any_bugs_here@}                                                */

/*-                                                                         */

/* Local Variables:                                                         */
      struct LCP *lcp;           /* Pointer to Locator Control Packet       */

/*.                                                                         */

/* Initialise the global status.                                            */
      hds_gl_status = DAT__OK;

/* Check that HDS is active. There is nothing to do if it is not.           */
      if ( hds_gl_active )
      {

/* Defuse all the Locator Control Packets.                                  */
         while ( dat_ga_wlq != NULL )
         {
            lcp = dat_ga_wlq;
            dau_defuse_lcp( &lcp );
         }

/* Close down the rec_ facility.                                            */
         rec_stop( );

/* Note that HDS is no longer active.                                       */
         hds_gl_active = 0;

/* If an error occurred, then report contextual information.                */
         if ( !_ok( hds_gl_status ) )
         {
            ems_rep_c( "HDS1_EXIT_ERR",
                       "HDS1_EXIT: Error deactivating the Hierarchical Data \
System (HDS).",
            &hds_gl_status );
         }
      }

/* Exit the routine.                                                        */
      return;
   }
