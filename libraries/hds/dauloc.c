#if HAVE_CONFIG_H
#  include <config.h>
#endif

/*+DAULOC.C-*/

/* Copyright:                                                               */
/*    Copyright (C) 1991 Science & Engineering Research Council             */
/*    Copyright (C) 2006 Particle Physics and Astronomy Research Council    */

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

/* Include files */

#include "ems.h"                 /* EMS error reporting routines            */

#include "star/mem.h"
#include "hds1.h"                /* Global definitions for HDS              */
#include "rec.h"                 /* Public rec_ definitions                 */
#include "str.h"                 /* Character string import/export macros   */
#include "dat1.h"                /* Internal dat_ definitions               */
#include "dat_err.h"             /* DAT__ error code definitions            */

/* Control Blocks */



/* Literals */

#define DAT_K_CLUSTER   17              /* FLQ cluster size (packets)   */
#define NBINS_INC       20              /* number of bins to resize     */

/* These file globals are used to track all the blocks malloced by this
   routine. They are tracked so that they can be freed at shutdown
   (via dau_free_flq) to prevent distracting valgrind warnings */

/* Array of malloced pointers */
struct LCP **malloced = NULL;
unsigned int npntrs = 0;
unsigned int totpntrs = 0;

/* internal prototypes */
int dau1_store_flq_malloc( struct LCP * );

/* functions */

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
         emsBegin( &hds_gl_status );

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
         emsEnd( &hds_gl_status );
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

/* Need to store the pointer in the global array */
_invoke(dau1_store_flq_malloc( lcp ));

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

int
dau_free_flq( void ) {
  /*
   * Free memory associated with the Free Locator Queue.
   * Should only be called from hdsStop during shutdown.
   * Relies on a global variable since the dat_ga_flq linked list
   * may have been reorganized such that it no longer includes an
   * element at the start of the malloced memory.
   */
  unsigned int i;

  /* Nothing to free */
  if ( malloced == NULL ) return DAT__OK;

  /* Free all the chunks */
  for (i = 0; i < npntrs; i++) {
    if (malloced[i] != NULL) {
      _invoke(rec_deall_mem( DAT_K_CLUSTER * sizeof( struct LCP ),
			     (void **)&malloced[i] ));
    }
  }
  /* No longer have an pointers */
  npntrs = 0;

  /* and the container */
  _invoke(rec_deall_mem( totpntrs * sizeof(struct LCP*),
			 (void **)&malloced));

  /* Reset values in case we are called twice */
  totpntrs = 0;

  return hds_gl_status;
}


int
dau1_store_flq_malloc( struct LCP * lcp ) {
  struct LCP** new;

  if ( malloced == NULL ) {
    /* get some memory */
    malloced = MEM_MALLOC( NBINS_INC * sizeof(struct LCP*) );
    if (malloced == NULL) {
      /* ignore for now since this is not a real leak */
      return DAT__OK;
    }
    totpntrs += NBINS_INC;

  } else if ( totpntrs == npntrs ) {
    /* Need to realloc some space */
      new = MEM_REALLOC( malloced, (totpntrs + NBINS_INC) * sizeof(struct LCP*));
    if (new == NULL) {
      /* just ignore and leak a bit - do not need to free the
	 old memory, since it can still contain useful information */
      return DAT__OK;
    }
    malloced = new;
    totpntrs += NBINS_INC;

  }

  /* Have some memory, so store the pointer */
  malloced[npntrs] = lcp;
  npntrs++;

  return DAT__OK;
}
