#include "hds1_feature.h"	 /* Define feature-test macros, etc.	    */

#include <string.h>

#include "ems.h"		 /* EMS error reporting routines	    */
#include "hds1.h"		 /* Global definitions for HDS		    */
#include "rec.h"		 /* Public rec_ definitions		    */
#include "dat1.h"		 /* Internal dat_ definitions		    */
#include "dat_err.h"		 /* DAT__ error code definitions	    */

   void dat1_alloc_lcp( INT loc_len, char *loc, struct LCP **lcp )
   {
/*+									    */
/* Name:								    */
/*    dat1_alloc_lcp							    */

/* Purpose:								    */
/*    Allocate a Locator Control Packet and initialise a locator.	    */

/* Invocation:								    */
/*    dat1_alloc_lcp( loc_len, loc, lcp )				    */

/* Description:								    */
/*    This function allocates a new Locator Control Packet (LCP) to control */
/*    user access to an HDS object and initialises a locator, thereby	    */
/*    associating it with the allocated LCP.				    */

/* Parameters:								    */
/*    INT loc_len							    */
/*	 Number of characters in the loc array. This is simply used for	    */
/*	 validating the size of this array and should be equal to the value */
/*	 of DAT__SZLOC, otherwise this routine will report an error.	    */
/*    char *loc								    */
/*	 Pointer to an array of DAT__SZLOC characters which is to be filled */
/*	 with the locator information.					    */
/*    struct LCP **lcp							    */
/*	 Pointer to a pointer which will be set to identify the		    */
/*	 newly-allocated LCP. A null pointer will be returned in *lcp if    */
/*	 this routine is invoked with the global status set, or if it	    */
/*	 should fail for any reason.					    */

/* Returned Value:							    */
/*    void								    */

/* Notes:								    */
/*    The returned LCP will have its data fields and "primary locator" flag */
/*    initialised to zero. Its sequence number will be set to match that of */
/*    the locator.							    */

/* Copyright:								    */
/*    Copyright (C) 1992 Science & Engineering Research Council		    */

/* Authors:								    */
/*    RFWS: R.F. Warren-Smith (STARLINK)				    */
/*    {@enter_new_authors_here@}					    */

/* History:								    */
/*    14-OCT-1992 (RFWS):						    */
/*       Substantially new routine based on old original.		    */
/*    {@enter_changes_here@}						    */

/* Bugs:								    */
/*    {@note_any_bugs_here@}						    */

/*-									    */

/* Local Variables:							    */
      struct LOC locinfo;	 /* Locator information			    */

/*.									    */

/* Set an initial null value for the returned LCP pointer.		    */
      *lcp = NULL;

/* Check the inherited global status.					    */
      if ( !_ok( hds_gl_status ) ) return;

/* Validate the length of the locator buffer.				    */
      if ( loc_len != DAT__SZLOC )
      {
	 hds_gl_status = DAT__LOCIN;
	 ems_seti_c( "LENGTH", loc_len );
	 ems_seti_c( "SZLOC", DAT__SZLOC );
	 ems_rep_c( "DAT1_ALLOC_LCP_1",
		    "Locator argument has an invalid length of ^LENGTH; it \
should be of length ^SZLOC (possible programming error).",
                    &hds_gl_status );
      }

/* Ensure that HDS has been initialised.				    */
      if ( !hds_gl_active )
      {
         dat1_init( );
      }

/* If the Free Locator Queue is empty, then refill it.			    */
      if ( dat_ga_flq == NULL )
      {
	 dau_refill_flq( );
      }

/* Allocate a new LCP from the free queue.				    */
      if( _ok( hds_gl_status ) )
      {
	 *lcp = dat_ga_flq;
	 _remque( *lcp, dat_ga_flq );

/* Clear the LCP data fields and the primary LCP flag.			    */
         (void) memset( (void *) &(*lcp)->data, 0, sizeof( struct LCP_DATA ) );
	 (*lcp)->primary = 0;

/* Insert the LCP at the head of the Working Locator Queue and increment    */
/* the Queue size.							    */
         _insque( *lcp, dat_ga_wlq );
         dat_gl_wlqsize++;

/* Initialise the locator information, including the locator sequence	    */
/* number which is duplicated in the LCP.				    */
         locinfo.check = DAT__LOCCHECK;
         locinfo.lcp = *lcp;
         locinfo.seqno = (*lcp)->seqno = ++hds_gl_locseq;

/* Copy the locator information into the locator buffer (this is necessary  */
/* because the buffer may not be correctly aligned to insert the	    */
/* information directly).						    */
         (void) memcpy( (void *) loc, (const void *) &locinfo,
			sizeof( struct LOC ) );
      }

/* Exit the routine.							    */
      return;
   }
