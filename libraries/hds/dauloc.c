#include "hds1_feature.h"	 /* Define feature-test macros, etc.	    */
/*+DAULOC.C-*/

/* Include files */

#include "ems.h"		 /* EMS error reporting routines	    */

#include "hds1.h"		 /* Global definitions for HDS		    */
#include "rec.h"		 /* Public rec_ definitions		    */
#include "str.h"		 /* Character string import/export macros   */
#include "dat1.h"		 /* Internal dat_ definitions		    */
#include "dat_err.h"		 /* DAT__ error code definitions	    */

/* Control Blocks */



/* Literals */

#define DAT_K_CLUSTER	17		/* FLQ cluster size (packets)	*/

dau_export_loc(locator,pntr)
/* N.B. This routine is obsolete and has been replaced by dat1_alloc_lcp.  */
/* Calls to this routine should be replaced as the opportunity arises and   */
/* it should eventually be removed (RFWS 14-OCT-1992).			    */
/*+
 * DAU_EXPORT_LOC - Export a locator
 *
 * This routine is used to format a buffer, of length DAT__SZLOC bytes, into
 * a 'locator'. The actual structure of a locator has no significance outside
 * the scope of EXPORT/IMPORT and, as such, can be re-defined at will.
 *
 * The current format of a locator is:
 *
 * 	+---------+
 * 	| check # | :Locator
 * 	+---------+
 * 	| LCP ptr | :Locator + 4
 * 	+---------+
 *      | seq #   | :Locator + 8
 *      +---------+
 *
 * Where:
 * 	 check #     is a validity check number (%x'7F7F7F7F')
 * 	 LCP ptr     is the pointer to the allocated Locator
 * 		     Control Packet.
 *       seq #       is a locator sequence number which must tally
 *                   with the sequence number in the Locator Control
 *                   Packet
 *
 * Calling sequence:
 *
 * 	  DAU_EXPORT_LOC(LOCATOR,PNTR)
 * 		
 * LOCATOR is the address of a character string descriptor pointing to a
 *	   buffer to receive the locator.
 * PNTR    is the address of a longword which is to receive the pointer to
 *	   the associated control packet.
 *
 * Routine value:
 *
 * 	  DAT__OK    if successful.
 * 	  DAT__LOCIN if the locator is invalid.
 */

struct DSC		 *locator;
struct LCP		**pntr;

{
struct LOC		 loc;
struct LCP     		 *lcp;

/* Set an initial null value for the returned pointer.			    */
      *pntr = NULL;

/* Check the inherited global status.					    */
      if ( !_ok( hds_gl_status ) ) return hds_gl_status;

/* Ensure that HDS has been initialised.				    */
      if ( !hds_gl_active )
      {
         dat1_init( );
         if ( !_ok( hds_gl_status ) ) return hds_gl_status;
      }

/*
   Check the length of the user's buffer and the descriptor type. Report an
   error if it is not OK.
*/
if ( locator->length != DAT__SZLOC )
{
   hds_gl_status = DAT__LOCIN;
   ems_seti_c( "LENGTH", (int) locator->length );
   ems_seti_c( "SZLOC", DAT__SZLOC );
   ems_rep_c( "DAU_EXPORT_LOC_1",
              "Locator argument has an invalid length of ^LENGTH; it should \
be of length ^SZLOC (possible programming error).",
              &hds_gl_status );
   return hds_gl_status;
}

/* Allocate a new LCP from the free queue (fill the list first if it's	    */
/* empty).								    */
if ( dat_ga_flq == NULL )
{
   if ( !_ok( dau_refill_flq( ) ) ) return hds_gl_status;
}
lcp = dat_ga_flq;
_remque( lcp, dat_ga_flq );

/* Clear all the LCP data fields and the primary LCP flag, insert the LCP   */
/* at the head of the Working Locator Queue and increment the Queue size.   */
(void) memset( (void *) &lcp->data, 0, sizeof( struct LCP_DATA ) );
lcp->primary = 0;
_insque( lcp, dat_ga_wlq );
++dat_gl_wlqsize;

/* Fill the locator fields, including the locator sequence number which is  */
/* duplicated in the LCP.						    */
      loc.check = DAT__LOCCHECK;
      loc.lcp   = lcp;
      loc.seqno = lcp->seqno = ++hds_gl_locseq;

/* Copy the locator information into the user's buffer and return a pointer */
/* to the LCP.								    */
      memcpy( (void *) locator->body, (void *) &loc, sizeof( struct LOC ) );
      if ( _ok( hds_gl_status ) ) *pntr = lcp;

      return hds_gl_status;
}

   dau_import_loc(locator,pntr)
/* N.B. This routine is obsolete and has been replaced by dat1_import_loc.  */
/* Calls to this routine should be replaced as the opportunity arises and   */
/* it should eventually be removed (RFWS 25-SEP-1992).			    */
/*+
 * DAU_IMPORT_LOC - Import a locator
 *
 * This routine is used as the entry point to the locator system. It accepts,
 * as input, a buffer of length DAT__SZLOC bytes which is assumed to contain
 * a valid locator. (The format of a locator is described under EXPORT). If the
 * locator is valid, then the associated control block is found.
 *
 * Calling sequence:
 *
 * 	  DAU_IMPORT_LOC(LOCATOR,PNTR)
 *
 * LOCATOR is the address of a character string descriptor pointing to a
 *	   buffer which holds the locator to be 'imported'.
 * PNTR    is the address of a longword which is to receive the pointer to
 *	   the associated control packet.
 *
 * Routine value:
 *
 * 	  DAT__OK    if successful.
 * 	  DAT__LOCIN if the locator is invalid.
 *	  DAT__INCHK if
 RFWS: 25-FEB-1992 - Ensure that HDS is initialised.
 */

   struct DSC		 *locator;
   struct LCP		**pntr;

   {
      int			  valid;
      struct LCP		 *lcp;
      struct LOC		 loc;
      struct RCL		  rcl;

/* Set an initial null value for the returned pointer.			    */
      *pntr = NULL;

/* Check the inherited global status.					    */
      if ( !_ok( hds_gl_status ) ) return hds_gl_status;

/* See if HDS has been initialised. The locator cannot be valid if it has   */
/* not, as no locators will have been issued.				    */
      valid = hds_gl_active;

/* Validate the locator length.						    */
      if ( valid )
      {
         valid = ( locator->length == DAT__SZLOC );
      }

/* If OK, then extract the locator information from the user's buffer.	    */
      if ( valid )
      {
	 memcpy( (void *) &loc, (void *) locator->body, sizeof( struct LOC ) );

/* Validate the locator check field.					    */
         valid = ( loc.check == DAT__LOCCHECK );
      }

/* If OK, then check that the locator sequence number tallies with the LCP  */
/* sequence number.							    */
      if ( valid )
      {
         lcp = loc.lcp;
         valid = ( loc.seqno == lcp->seqno );
      }

/* If OK, then check that the associated LCP is valid.			    */
      if ( valid )
      {
         valid = lcp->data.valid;
      }

/* If still OK, then read the associated RCL and check that the RID of the  */
/* record's parent, as stored in the LCP, matches the RID stored in the	    */
/* actual record.							    */
      if ( valid )
      {
         rec_get_rcl( &lcp->data.han, &rcl );
         if ( _ok( hds_gl_status ) )
         {

/* If the RIDs do not match, then report an error.			    */
            if ( ( rcl.parent.bloc != lcp->data.parent.bloc ) ||
                 ( rcl.parent.chip != lcp->data.parent.chip ) )
            {
               hds_gl_status = DAT__INCHK;
               ems_setc_c( "NAME", lcp->data.name, DAT__SZNAM );
               ems_rep_c( "DAU_IMPORT_LOC_1",
                          "Locator refers to an object \'^NAME\' which no \
longer exists (possible programming error or corrupted HDS container file).",
                          &hds_gl_status );
            }
         }
      }

/* If the locator is not valid, but no other error has occurred, then	    */
/* report an error.							    */
      if ( !valid && _ok( hds_gl_status ) )
      {
         hds_gl_status = DAT__LOCIN;
         ems_setc_c( "VALUE", (char *) locator->body, locator->length );
         ems_seti_c( "LENGTH", locator->length );
         ems_rep_c( "DAU_IMPORT_LOC_2",
                    "HDS locator invalid: value=\'^VALUE\', length=^LENGTH \
(possible programming error).",
                    &hds_gl_status );
      }

/* If no errors have occurred, then return a pointer to the LCP.	    */
      if ( _ok( hds_gl_status ) )
      {
         *pntr = lcp;
      }

/* Return the current global status value.				    */
      return hds_gl_status;
   }


dau_defuse_lcp(pntr)

/*+
 * DAU_DEFUSE_LCP - Deactivate LCP
 *
 * This routine deactivates a Locator Control Packet. Any mapped data is
 * first unmapped, then the packet is removed from the Working Locator Queue
 * and inserted back into the free list.
 *
 * Calling sequence:
 *
 * 	  DAU_DEFUSE_LCP(PNTR)
 *
 * PNTR    is the address of a longword which contains the pointer to the
 *         control packet to be deactivated. (Note, that this is modified).
 *
 * Routine value:
 *
 * 	  DAT__OK    if successful.
 *
 * Side effects:
 *
 *	  PNTR is set to the address of the next control packet in the list.
 RFWS: 25-FEB-1992 - Ensure that HDS is initialised.
 */

struct LCP	**pntr;

{
struct LCP 	 *lcp;
struct LCP_DATA	 *data;

/* Check if the LCP pointer is null on input. There is nothing to do if it  */
/* is.									    */
      if ( *pntr == NULL ) return hds_gl_status;

/* Check that HDS has been initialised. If not, then there can be no active */
/* locators, so there is nothing to do.					    */
      if ( hds_gl_active )
      {

/* Begin a new error reporting context.					    */
         ems_begin_c( &hds_gl_status );

/* Obtain a pointer to the LCP data fields.				    */
         lcp = *pntr;
         data = &lcp->data;

/* If the LCP is valid, then flush any currently mapped primitive data.	    */
         if ( data->valid ) dau_flush_data( data );

/* Return a pointer the next LCP in the Working Locator Queue.		    */
         *pntr = lcp->flink;

/* Clear the old LCP valid flag, remove the LCP from the Working Locator    */
/* Queue and decrement the queue size.					    */
         data->valid = 0;
         _remque( lcp, dat_ga_wlq );
         --dat_gl_wlqsize;

/* Insert the released LCP back into the Free Locator Queue.		    */
         _insque( lcp, dat_ga_flq );

/* End the error reporting context and return the current global status	    */
/* value.								    */
         ems_end_c( &hds_gl_status );
      }
      return hds_gl_status;
   }

dau_refill_flq()

/*+
 * DAU_REFILL_FLQ - Refill Free Locator Queue
 *
 * This routine will fill the FLQ with a cluster of 'virgin' Locator
 * Control Packets.
 *
 * Calling sequence:
 *
 * 	  DAU_REFILL_FLQ()
 *
 * Routine value:
 *
 * 	  DAT__OK    if successful.
 */

{
struct LCP *lcp;
int		 i;

/* Attempt to get enough virtual memory for a cluster of LCPs. 	*/

_invoke( rec_alloc_mem( DAT_K_CLUSTER * sizeof( struct LCP ),
			(void **) &lcp ) );
(void) memset( (void *) lcp, 0,
               (size_t) ( DAT_K_CLUSTER * sizeof( struct LCP ) ) );

/* Insert each new LCP at the head of the Free Locator Queue.	*/

for (i=1; i<=DAT_K_CLUSTER; i++)
	{
	_insque( lcp, dat_ga_flq );
	lcp++;
	}
return hds_gl_status;
}
