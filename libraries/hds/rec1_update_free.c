#if HAVE_CONFIG_H
#  include <config.h>
#endif

#include "ems.h"                 /* EMS error reporting routines            */
#include "hds1.h"                /* Global definitions for HDS              */
#include "rec1.h"                /* Internal rec_ definitions               */
#include "dat_err.h"             /* DAT__ error code definitions            */

   int rec1_update_free( int slot, INT_BIG bloc, const unsigned char cbm[ 2 ] )
   {
/*+                                                                         */
/* Name:                                                                    */
/*    rec1_update_free                                                      */

/* Purpose:                                                                 */
/*    Update the free space stack for a Logical Record Block.               */

/* Invocation:                                                              */
/*    rec1_update_free( slot, bloc, cbm )                                   */

/* Description:                                                             */
/*    This function updates an entry in the free space stack for a          */
/*    specified Logical Record Block. If, as a result, all the block's      */
/*    chips are spare, it is put on to the free block stack (where it may   */
/*    be used in future as a Pure Data Block) and its modified bit in the   */
/*    Working Page List is reset to prevent it from being written back to   */
/*    disk.                                                                 */

/* Parameters:                                                              */
/*    int slot                                                              */
/*       Slot number of the container file in the File Control Vector.      */
/*    int bloc                                                              */
/*       Logical Record Block number in the container file (the first block */
/*       in the file is no. 1).                                             */
/*    const unsigned char cbm[ 2 ]                                          */
/*       The Logical Record Block's chip bitmap.                            */

/* Returned Value:                                                          */
/*    int rec1_update_free                                                  */
/*       The global status value current on exit.                           */

/* Authors:                                                                 */
/*    RFWS: R.F. Warren-Smith (STARLINK)                                    */
/*    TIMJ: Tim Jenness (JAC, Hawaii)                                       */
/*    {@enter_new_authors_here@}                                            */

/* History:                                                                 */
/*    26-MAR-1991 (RFWS):                                                   */
/*       Prologue added.                                                    */
/*    9-APR-1991 (RFWS):                                                    */
/*       Improved error handling.                                           */
/*    24-SEP-1991 (RFWS):                                                   */
/*       Fixed bug. When resetting the modify flag in the working page      */
/*       list, the file slot number was not being checked. This could cause */
/*       the flag for the same block in another file to be cleared,         */
/*       resulting in failure to flush the block at closedown and a corrupt */
/*       container file.                                                    */
/*    31-OCT-2006 (TIMJ):                                                   */
/*       Sanity check stack overflow.                                       */
/*    {@enter_further_changes_here@}                                        */

/* Bugs:                                                                    */
/*    {@note_any_bugs_here@}                                                */

/*-                                                                         */

/* Local Variables:                                                         */
      int chip;                  /* Free chip number                        */
      int empty;                 /* Empty free chip stack entry number      */
      int entno = 0;             /* Free space stack entry number           */
      int full;                  /* Free space stack is full?               */
      int i;                     /* Loop counter for free space stack       */
      int spare;                 /* Largest free space chip number          */
      struct BCP *bcp;           /* Pointer to block control packet         */
      struct HCB *hcb;           /* Pointer to Header Control Block         */
      struct STK *stk;           /* Pointer to free space stack             */

/*.                                                                         */

/* Check the inherited global status.                                       */
      if ( !_ok( hds_gl_status ) ) return hds_gl_status;

/* Locate the Header Control Block and its free space stack.                */
      rec1_locate_hcb( slot, 'U', &hcb );
      if ( _ok( hds_gl_status ) )
      {
         stk = hcb->stk;

/* Scan through the chip bitmap for the largest free space.                 */
         spare = REC__MXCHIP;
         while ( spare > 0 )
         {
            if ( rec1_scan_cbm( cbm, spare, &chip ) )
            {
               break;
            }
            else
            {
               spare--;
            }
         }

/* Search the free chip stack for a frame match. If no entry currently      */
/* exists for the specified frame, then use a void one.                     */
         empty = REC__MXSTK - 1;
         for ( i = 0; ; i++ )
         {
            if (i== REC__MXSTK) {
              /* something has gone horribly wrong */
              if (_ok(hds_gl_status) ) {
                 hds_gl_status = DAT__WEIRD;
                 emsRep("REC1_UPDATE_FREE","Search of free chip stack for a frame match exceeded stack size",
                         &hds_gl_status);
                 break;
              }
            }
            if ( stk[ i ].bloc == -1 )
            {
               entno = _min( i, empty );
               break;
            }
            else if ( stk[ i ].bloc == bloc )
            {
               entno = i;
               break;
            }
            else if ( stk[ i ].spare == 0 )
            {
               empty  = i;
            }
         }

/* If all chips in the block are spare, make the entry a void one and add   */
/* the block to the free block stack. Then scan through the Working Page    */
/* List and if the block is found reset its modified bit to prevent it      */
/* being written back to disc.                                              */
         if ( spare == REC__MXCHIP )
         {
            spare = 0;
            rec1_deall_frame( slot, 1, bloc );
            if ( _ok( hds_gl_status ) )
            {
               bcp = rec_ga_wpl;
               for ( i = 0; i < rec_gl_wplsize; i++ )
               {
                  if ( ( bcp->bid.bloc == bloc ) &&
                       ( bcp->bid.slot == slot ) )
                  {
                     bcp->modify = 0;
                     break;
                  }
                  bcp = bcp->flink;
               }
            }
         }

/* Fill the entry if one is available.                                      */
         if ( _ok( hds_gl_status ) )
         {
            full = ( ( stk[ entno ].bloc == -1 ) &&
                     ( stk[ entno + 1 ].bloc != -1 ) );
            if ( !full )
            {
               stk[ entno ].bloc = bloc;
               stk[ entno ].spare = spare;
            }
         }
      }

/* Return the current global status value.                                  */
      return hds_gl_status;
   }
