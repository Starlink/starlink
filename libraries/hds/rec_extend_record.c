#if HAVE_CONFIG_H
#  include <config.h>
#endif

#include <stddef.h>
#include "hds1.h"                /* Global definitions for HDS              */
#include "rec.h"                 /* Public rec_ definitions                 */
#include "rec1.h"                /* Internal rec_ definitions               */
#include "dat_err.h"             /* DAT__ error code definitions            */

int rec_extend_record( const struct HAN *han, INT_BIG extent )
{
/*+                                                                         */
/* Name:                                                                    */
/*    rec_extend_record                                                     */

/* Purpose:                                                                 */
/*    Extend a record's Dynamic Domain.                                     */

/* Invocation:                                                              */
/*    rec_extend_record( han, extent )                                      */

/* Description:                                                             */
/*    This function extends a record's Dynamic Domain by a specified        */
/*    amount. The new space allocated is left in an undefined state, but    */
/*    any existing contents are preserved.                                  */

/* Parameters:                                                              */
/*    const struct HAN *han                                                 */
/*       Pointer to a HAN structure containing a handle for the record to   */
/*       be extended.                                                       */
/*    INT_BIG extent                                                        */
/*       The number of unsigned chars by which the size of the record's     */
/*       Dynamic Domain is to be extended.                                  */

/* Returned Value:                                                          */
/*    int rec_extend_record                                                 */
/*       The global status value current on exit.                           */

/* Authors:                                                                 */
/*    RFWS: R.F. Warren-Smith (STARLINK)                                    */
/*    BKM:  B.K. McIlwrath    (STARLINK)                                    */
/*    {@enter_new_authors_here@}                                            */

/* History:                                                                 */
/*    24-APR-1991 (RFWS):                                                   */
/*       Added prologue and error handling, made portable and tidied.       */
/*    14-JUNE-1991 (RFWS):                                                  */
/*       Changed access mode to 'U' in rec_locate_block.                    */
/*    6-AUG-1993 (RFWS):                                                    */
/*       Added extra status check in case of failure to map a new chained   */
/*       dynamic domain.                                                    */
/*    27-JUN-2000 (BKM):                                                    */
/*       Revise for extended format (64-bit) HDS files                      */
/*    {@enter_further_changes_here@}                                        */

/* Bugs:                                                                    */
/*    {@note_any_bugs_here@}                                                */

/*-                                                                         */

/* Local Variables:                                                         */
   INT_BIG bloc;                /* No. of first block in frame of PDBs      */
   INT_BIG dlen;                /* Dynamic Domain length (unsigned chars)   */
   INT_BIG extra;               /* No. extra blocks or chips required       */
   INT_BIG size;                /* Size of frame in blocks                  */
   int chip;                    /* Chip number                              */
   int spare;                   /* No. chips freed                          */
   struct RCL rcl;              /* Record Control Label                     */
   unsigned char *cbm;          /* Pointer to Chip Bitmap                   */
   unsigned char *cdom;         /* Pointer to Control Domain                */
   unsigned char *ddom;         /* Pointer to Dynamic Domain                */
   unsigned char *lrb;          /* Pointer to Logical Record Block          */
   unsigned char *pntr;         /* Pointer to mapped frame of PDBs          */

/*.                                                                         */

/* Check the inherited global status.                                       */
   if ( !_ok( hds_gl_status ) )
      return hds_gl_status;

/* Locate the Logical Record Block containing the record.                   */
   rec_locate_block( han->slot, han->rid.bloc, 'U', &lrb );

/* Find the Chip Bitmap.                                                    */
   cbm = lrb;

/* Obtain a pointer to the Control Domain and unpack the Record Control     */
/* Label.                                                                   */
   cdom = lrb + REC__SZCBM + ( han->rid.chip * REC__SZCHIP );
   rec1_unpack_rcl( cdom, &rcl );

/* Derive a pointer to the Dynamic Domain and calculate the new length of   */
/* this domain.                                                             */
   ddom = cdom + ( rcl.extended ? REC__SZRCL : REC__SZORCL ) + rcl.slen;
   dlen = rcl.dlen + extent;

/* If the Dynamic Domain is chained, then unpack the number of the first    */
/* block in the associated frame of Pure Data Blocks.                       */
   if ( rcl.chain )
   {
      rec1_unpack_chain( ddom, rcl.extended, &bloc );

/* Extend the size of this frame if necessary.                              */
      extra = _nblocs( dlen ) - _nblocs( rcl.dlen );
      if ( extra > 0 )
      {
         size = _nblocs( rcl.dlen );
         rec1_extend_frame( han->slot, size, extra, &bloc );

/* If the record is zero-on-create, map the extension, filling it with      */
/* zeros, then unmap it.                                                    */
         if (rcl.zero)
         {
            rec1_map_frame( han->slot, bloc + size, extra * REC__SZBLK, 0,
                            'Z', &pntr );
            rec1_unmap_frame( han->slot, bloc + size, extra * REC__SZBLK, 0,
                              'Z', &pntr );
         }

/* Re-pack the first block number of the frame (which may have changed      */
/* during the extension process).                                           */
         rec1_pack_chain( bloc, ddom );
      }
   }

/* If the Dynamic Domain is not initially chained, then check if the        */
/* expansion implies that extra chips are required.                         */
   else
   {
      extra = _nchips( rcl.slen + dlen ) - rcl.size;

/* If more chips are required, then first determine if there are any spare  */
/* after the current end of the record.                                     */
      if ( extra > 0 )
      {
         chip = han->rid.chip + rcl.size;
         rcl.chain = ( ( chip + extra ) >= REC__MXCHIP );
         if ( !rcl.chain && _ok( hds_gl_status ) )
         {
            rcl.chain = !rec1_test_cbm( cbm, chip, extra );
         }

/* If the necessary number of spare chips are available, then the Dynamic   */
/* Domain will not need to be chained. Modify the record size and set the   */
/* appropriate bits in the Chip Bitmap.                                     */
         if ( !rcl.chain )
         {
            rcl.size += extra;
            rec1_set_cbm( cbm, extra, chip );
         }

/* Otherwise, the Dynamic Domain must be chained. Allocate and map a        */
/* suitable frame of Pure Data Blocks.                                      */
         else
         {
            size = _nblocs( dlen );
            rec1_alloc_frame( han->slot, size, &bloc );
            rec1_map_frame( han->slot, bloc, size * REC__SZBLK, 0, 'Z',
                            &pntr );

/* Move the record's Dynamic Domain contents into the frame and then unmap  */
/* it.  Fill the released chips in the Logical Record Block with zeros.     */
            if ( _ok ( hds_gl_status ) )
            {
               _chmove( rcl.dlen, ddom, pntr );
               rec1_unmap_frame( han->slot, bloc, size * REC__SZBLK, 0, 'W',
                                 &pntr );
               (void) memset( (void *) ddom, 0, (size_t) rcl.dlen );

/* Pack the block number of the start of the frame into the Dynamic Domain  */
/* and calculate the new size of the record.                                */
               rec1_pack_chain( bloc, ddom );
               size = _nchips( rcl.slen + SZCHAIN(rcl.extended) );

/* Clear the appropriate bits in the Chip Bitmap.                           */
               spare = rcl.size - size;
               chip = han->rid.chip + size;
               rcl.size = size;
               rec1_clear_cbm( cbm, spare, chip );
            }
         }

/* Update the free space stack appropriately.                               */
         rec1_update_free( han->slot, han->rid.bloc, cbm );
      }
   }

/* Reset the length of the Dynamic Domain and re-pack the Record Control    */
/* Label into the Control Domain.                                           */
   rcl.dlen = dlen;
   rec1_pack_rcl( &rcl, cdom );

/* Release the Logical Record Block.                                        */
   if ( lrb != NULL )
      rec_release_block( han->slot, han->rid.bloc );

/* Return the current global status value.                                  */
   return hds_gl_status;
}
