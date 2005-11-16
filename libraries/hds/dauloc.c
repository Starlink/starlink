#if HAVE_CONFIG_H
#  include <config.h>
#endif

/*+DAULOC.C-*/

/* Include files */

#include "ems.h"                 /* EMS error reporting routines            */

#include "hds1.h"                /* Global definitions for HDS              */
#include "rec.h"                 /* Public rec_ definitions                 */
#include "str.h"                 /* Character string import/export macros   */
#include "dat1.h"                /* Internal dat_ definitions               */
#include "dat_err.h"             /* DAT__ error code definitions            */

/* Control Blocks */



/* Literals */

#define DAT_K_CLUSTER   17              /* FLQ cluster size (packets)   */

int
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
 *        DAU_DEFUSE_LCP(PNTR)
 *
 * PNTR    is the address of a longword which contains the pointer to the
 *         control packet to be deactivated. (Note, that this is modified).
 *
 * Routine value:
 *
 *        DAT__OK    if successful.
 *
 * Side effects:
 *
 *        PNTR is set to the address of the next control packet in the list.
 RFWS: 25-FEB-1992 - Ensure that HDS is initialised.
 */

struct LCP      **pntr;

{
struct LCP       *lcp;
struct LCP_DATA  *data;

/* Check if the LCP pointer is null on input. There is nothing to do if it  */
/* is.                                                                      */
      if ( *pntr == NULL ) return hds_gl_status;

/* Check that HDS has been initialised. If not, then there can be no active */
/* locators, so there is nothing to do.                                     */
      if ( hds_gl_active )
      {

/* Begin a new error reporting context.                                     */
         ems_begin_c( &hds_gl_status );

/* Obtain a pointer to the LCP data fields.                                 */
         lcp = *pntr;
         data = &lcp->data;

/* If the LCP is valid, then flush any currently mapped primitive data.     */
         if ( data->valid ) dau_flush_data( data );

/* Return a pointer the next LCP in the Working Locator Queue.              */
         *pntr = lcp->flink;

/* Clear the old LCP valid flag, remove the LCP from the Working Locator    */
/* Queue and decrement the queue size.                                      */
         data->valid = 0;
         _remque( lcp, dat_ga_wlq );
         --dat_gl_wlqsize;

/* Insert the released LCP back into the Free Locator Queue.                */
         _insque( lcp, dat_ga_flq );

/* End the error reporting context and return the current global status     */
/* value.                                                                   */
         ems_end_c( &hds_gl_status );
      }
      return hds_gl_status;
   }

int
dau_refill_flq(void)

/*+
 * DAU_REFILL_FLQ - Refill Free Locator Queue
 *
 * This routine will fill the FLQ with a cluster of 'virgin' Locator
 * Control Packets.
 *
 * Calling sequence:
 *
 *        DAU_REFILL_FLQ()
 *
 * Routine value:
 *
 *        DAT__OK    if successful.
 */

{
struct LCP *lcp;
int              i;

/* Attempt to get enough virtual memory for a cluster of LCPs.  */

_invoke( rec_alloc_mem( DAT_K_CLUSTER * sizeof( struct LCP ),
                        (void **) &lcp ) );
(void) memset( (void *) lcp, 0,
               (size_t) ( DAT_K_CLUSTER * sizeof( struct LCP ) ) );

/* Insert each new LCP at the head of the Free Locator Queue.   */

for (i=1; i<=DAT_K_CLUSTER; i++)
        {
        _insque( lcp, dat_ga_flq );
        lcp++;
        }
return hds_gl_status;
}
