#if HAVE_CONFIG_H
#  include <config.h>
#endif

#include <string.h>
#include <stdlib.h>

#include "ems.h"                 /* EMS error reporting routines            */
#include "hds1.h"                /* Global definitions for HDS              */
#include "rec.h"                 /* Public rec_ definitions                 */
#include "dat1.h"                /* Internal dat_ definitions               */
#include "dat_err.h"             /* DAT__ error code definitions            */

   int dat1_alloc_lcp( struct LOC **loc, struct LCP **lcp )
   {
/*+                                                                         */
/* Name:                                                                    */
/*    dat1_alloc_lcp                                                        */

/* Purpose:                                                                 */
/*    Allocate a Locator Control Packet and initialise a locator.           */

/* Invocation:                                                              */
/*    dat1_alloc_lcp( loc, lcp )                                            */

/* Description:                                                             */
/*    This function allocates a new Locator Control Packet (LCP) to control */
/*    user access to an HDS object and initialises a locator, thereby       */
/*    associating it with the allocated LCP.                                */

/* Parameters:                                                              */
/*    struct LOC **loc                                                      */
/*       Pointer to a pointer to a struct LOC that is to be filled with the */
/*       locator information. Will be malloced by this routine and freed    */
/*       with a datAnnul. *loc Must be NULL on entry.                       */
/*    struct LCP **lcp                                                      */
/*       Pointer to a pointer which will be set to identify the             */
/*       newly-allocated LCP. A null pointer will be returned in *lcp if    */
/*       this routine is invoked with the global status set, or if it       */
/*       should fail for any reason.                                        */

/* Returned Value:                                                          */
/*    int dat1_alloc_lcp                                                    */
/*       The global status value current on exit.                           */


/* Notes:                                                                   */
/*    The returned LCP will have its data fields and "primary locator" flag */
/*    initialised to zero. Its sequence number will be set to match that of */
/*    the locator.                                                          */

/* Copyright:                                                               */
/*    Copyright (C) 1992 Science & Engineering Research Council             */
/*    Copyright (C) 2005 Particle Physics and Astronomy Research Council    */

/* Authors:                                                                 */
/*    RFWS: R.F. Warren-Smith (STARLINK)                                    */
/*    TIMJ: T.   Jenness      (JAC, Hawaii)                                 */
/*    {@enter_new_authors_here@}                                            */

/* History:                                                                 */
/*    14-OCT-1992 (RFWS):                                                   */
/*       Substantially new routine based on old original.                   */
/*    15-NOV-2005 (TIMJ):                                                   */
/*       Change API to use the struct LOC explcitly                         */
/*    {@enter_changes_here@}                                                */

/* Bugs:                                                                    */
/*    {@note_any_bugs_here@}                                                */

/*-                                                                         */

/* Local Variables:                                                         */

/*.                                                                         */

/* Set an initial null value for the returned LCP pointer.                  */
      *lcp = NULL;

/* Check the inherited global status.                                       */
      if ( !_ok( hds_gl_status ) ) return hds_gl_status;

/* Check that the locator is NULL */
      if (*loc != NULL ) {
	hds_gl_status = DAT__LOCIN;
	emsRep( "DAT1_ALLOC_LCP",
		"Supplied locator is not a NULL pointer (Possible programming error)",
		&hds_gl_status);
		return hds_gl_status;
      }

/* Ensure that HDS has been initialised.                                    */
      if ( !hds_gl_active )
      {
         dat1_init( );
      }

/* If the Free Locator Queue is empty, then refill it.                      */
      if ( dat_ga_flq == NULL )
      {
         dau_refill_flq( );
      }

/* Allocate a new LCP from the free queue.                                  */
      if( _ok( hds_gl_status ) )
      {
         *lcp = dat_ga_flq;
         _remque( *lcp, dat_ga_flq );

/* Clear the LCP data fields and the primary LCP flag.                      */
         (void) memset( (void *) &(*lcp)->data, 0, sizeof( struct LCP_DATA ) );
         (*lcp)->primary = 0;

/* Insert the LCP at the head of the Working Locator Queue and increment    */
/* the Queue size.                                                          */
         _insque( *lcp, dat_ga_wlq );
         dat_gl_wlqsize++;

/* Initialise the locator information, including the locator sequence       */
/* number which is duplicated in the LCP.                                   */
	 *loc = malloc( sizeof(struct LOC) );
         (*loc)->check = DAT__LOCCHECK;
         (*loc)->lcp = *lcp;
         (*loc)->seqno = (*lcp)->seqno = ++hds_gl_locseq;

      }

/* Exit the routine.                                                        */
      return hds_gl_status;
   }
